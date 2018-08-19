package main

import (
	"bytes"
	"flag"
	"fmt"
	"go/ast"
	"go/build"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"io/ioutil"
	"log"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"strings"
	"unicode"

	"github.com/pkg/errors"
	"golang.org/x/tools/imports"
)

var tagRegex = regexp.MustCompile(`([0-9a-zA-Z,_=&\(\)\-]+)(:( )?"([0-9a-zA-Z,_=&\(\)\-]*)")?`)

var (
	src = flag.String("src", "", "comma-separated list of type names; must be set")
	dst = flag.String("dst", "", "comma-separated list of type names; must be set")
)

// Usage is a replacement usage function for the flags package.
func Usage() {
	fmt.Fprintf(os.Stderr, "Usage of %s:\n", os.Args[0])
	flag.PrintDefaults()
}

func main() {
	log.SetFlags(0)
	log.SetPrefix("repacker: ")
	flag.Usage = Usage
	flag.Parse()
	if len(*src) == 0 || len(*dst) == 0 {
		flag.Usage()
		os.Exit(2)
	}

	args := flag.Args()
	if len(args) == 0 {
		// Default: process whole package in current directory.
		args = []string{"."}
	} else if len(args) > 1 {
		flag.Usage()
		os.Exit(2)
	}

	if !isDirectory(args[0]) {
		log.Fatalf("Directory must be specified")
		os.Exit(2)
	}

	if err := run(args[0]); err != nil {
		log.Fatalf("%+v", err)
	}
}

func run(argDir string) (err error) {

	g := &Generator{}
	g.funcNames = map[string]bool{}
	g.dir = argDir

	d, err := filepath.Abs(g.dir)
	if err != nil {
		return errors.Wrapf(err, "Abs %s: %s", g.dir, err)
	}
	g.rootDir = d
	dstType := Type{
		dir:  d,
		name: *dst,
	}
	dstPkg := g.parsePackageDir(dstType.dir)

	srcType := g.parseFullTypeString(*src, &Package{dir: d})

	log.Println("Generating...")
	g.generateHead(dstPkg.name, srcType.dir)
	if _, err = g.generate(srcType, dstType); err != nil {
		return errors.Wrapf(err, "generate: %s", err)
	}

	var formatOnly bool
	if g.rootDir == srcType.dir {
		formatOnly = true
	}
	// Format the output.
	srcCode, err := g.goimport(formatOnly)
	if err != nil {
		return errors.Wrapf(err, "goimport: %s", err)
	}

	// Write to file.
	baseName := fmt.Sprintf("%s_from_%s_repack.go", dstType.name, srcType.name)
	outputName := filepath.Join(dstType.dir, strings.ToLower(baseName))
	err = ioutil.WriteFile(outputName, srcCode, 0644)
	if err != nil {
		return errors.Wrapf(err, "Writing output: %s", err)
	}
	return nil
}

// Generator holds the state of the analysis. Primarily used to buffer
// the output for format.Source.
type Generator struct {
	buf       bytes.Buffer
	dir       string
	funcNames map[string]bool
	fset      *token.FileSet
	rootDir   string
}

func (g *Generator) parseFullTypeString(fullType string, pkg *Package) Type {
	// Split full type (e.g. github.com/knqyf263/repackr.User)
	importPath, typeName := splitType(fullType)
	t := Type{
		name: typeName,
		dir:  pkg.dir, // default value
	}
	if importPath != "" && importPath != pkg.name {
		buildPkg, err := build.Import(importPath, pkg.dir, build.FindOnly)
		if err != nil {
			log.Fatalf("Import %s: %s", importPath, err)
			os.Exit(2)
		}
		t.dir = buildPkg.Dir
	}
	return t
}

func (g *Generator) parseType(t types.Type, pkg *Package) Type {
	var typeName string
	var isSlice, isPointer, isBasic bool
	if s, ok := t.(*types.Slice); ok {
		isSlice = true
		t = s.Elem()
	}

	if p, ok := t.(*types.Pointer); ok {
		isPointer = true
		t = p.Elem()
	}

	switch s := t.(type) {
	case *types.Struct, *types.Named:
		typeName = s.String()
	case *types.Basic:
		isBasic = true
		typeName = s.String()
	default:
		typeName = ""
	}

	typ := g.parseFullTypeString(typeName, pkg)
	typ.isSlice = isSlice
	typ.isPointer = isPointer
	typ.isBasic = isBasic

	return typ
}

// parsePackageDir parses the package residing in the directory.
func (g *Generator) parsePackageDir(directory string) *Package {
	pkg, err := build.Default.ImportDir(directory, 0)
	if err != nil {
		log.Fatalf("cannot process directory %s: %s", directory, err)
	}
	names := prefixDirectory(directory, pkg.GoFiles)
	return g.parsePackage(directory, names, nil)
}

// prefixDirectory places the directory name on the beginning of each name in the list.
func prefixDirectory(directory string, names []string) []string {
	if directory == "." {
		return names
	}
	ret := make([]string, len(names))
	for i, name := range names {
		ret[i] = filepath.Join(directory, name)
	}
	return ret
}

// parsePackage analyzes the single package constructed from the named files.
// If text is non-nil, it is a string to be used instead of the content of the file,
// to be used for testing. parsePackage exits if there is an error.
func (g *Generator) parsePackage(directory string, names []string, text interface{}) *Package {
	var astFiles []*ast.File
	fset := token.NewFileSet()
	for _, name := range names {
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_repack.go") {
			continue
		}
		parsedFile, err := parser.ParseFile(fset, name, text, parser.ParseComments)
		if err != nil {
			log.Fatalf("parsing package: %s: %s", name, err)
		}
		astFiles = append(astFiles, parsedFile)
	}
	if len(astFiles) == 0 {
		log.Fatalf("%s: no buildable Go files", directory)
		return nil
	}
	return &Package{
		dir:      directory,
		name:     astFiles[0].Name.Name,
		astFiles: astFiles,
		fset:     fset,
	}
}

// Printf prints
func (g *Generator) Printf(format string, args ...interface{}) {
	fmt.Fprintf(&g.buf, format, args...)
}

func (g *Generator) generateHead(pkgName, importPath string) {
	g.Printf("// Code generated by \"repacker %s\"; DO NOT EDIT\n", strings.Join(os.Args[1:], " "))
	g.Printf("\n")
	g.Printf("package %s", pkgName)
	g.Printf("\n")
	if g.rootDir != importPath {
		g.Printf("import \"%s\"\n", imports.VendorlessPath(importPath))
	}
}

type Type struct {
	dir       string
	name      string
	isSlice   bool
	isPointer bool
	isBasic   bool
}

func (g *Generator) generate(srcType, dstType Type) (funcName string, err error) {
	if reflect.DeepEqual(srcType, dstType) {
		log.Println("same type")
		return "", nil
	}
	if srcType.isSlice != dstType.isSlice {
		return "", errors.New("One type is slice")
	}
	srcPkg := g.parsePackageDir(srcType.dir)
	dstPkg := g.parsePackageDir(dstType.dir)

	conf := types.Config{
		// Importer: importer.Default(),
		Importer: importer.For("source", nil),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	srcObj, err := g.lookup(conf, srcPkg, srcType)
	if err != nil {
		return "", errors.Wrapf(err, "Lookup: %s.%s", srcPkg.name, srcType.name)
	}

	dstObj, err := g.lookup(conf, dstPkg, dstType)
	if err != nil {
		return "", errors.Wrapf(err, "Lookup: %s.%s", dstPkg.name, dstType.name)
	}

	if srcObj == nil || dstObj == nil {
		return "", errors.New("package not found")
	}

	src := Object{
		pkg:    srcPkg,
		typ:    srcType,
		object: srcObj,
	}
	dst := Object{
		pkg:    dstPkg,
		typ:    dstType,
		object: dstObj,
	}

	if srcType.isSlice {
		return g.generateSliceCode(src, dst), nil
	}

	return g.generateCode(src, dst), nil
}
func (g *Generator) lookup(conf types.Config, pkg *Package, typ Type) (types.Object, error) {
	log.Printf("Lookup %s.%s\n", pkg.name, typ.name)
	p, err := conf.Check(pkg.name, pkg.fset, pkg.astFiles, nil)
	if err != nil {
		return nil, err
	}
	obj := p.Scope().Lookup(typ.name)
	if obj == nil {
		return nil, fmt.Errorf("Failed to lookup: %s", typ.name)
	}
	return obj, nil
}

type Object struct {
	pkg    *Package
	typ    Type
	object types.Object
}

func (o Object) Name() string {
	return fmt.Sprintf("*%s", o.object.Name())
}
func (o Object) SliceName() (name string) {
	if o.typ.isSlice && !o.typ.isPointer {
		return o.object.Name()
	}
	return o.Name()
}

func (o Object) FullName(rootDir string) string {
	if rootDir == o.pkg.dir {
		return fmt.Sprintf("*%s", o.object.Name())
	}
	return fmt.Sprintf("*%s.%s", o.pkg.name, o.object.Name())
}

func (o Object) SliceFullName(rootDir string) (name string) {
	if o.typ.isSlice && !o.typ.isPointer {
		if rootDir == o.pkg.dir {
			return o.object.Name()
		}
		return fmt.Sprintf("%s.%s", o.pkg.name, o.object.Name())
	}
	return o.FullName(rootDir)
}

func (o Object) PtrName() string {
	return fmt.Sprintf("&%s", o.object.Name())
}

func (g *Generator) generateCode(src, dst Object) (funcName string) {
	srcInternal := src.object.Type().Underlying().(*types.Struct)
	dstInternal := dst.object.Type().Underlying().(*types.Struct)

	var code bytes.Buffer
	var body bytes.Buffer
	var variables bytes.Buffer

	var pkgName string
	if g.rootDir != src.pkg.dir {
		pkgName = strings.Title(src.pkg.name)
	}
	funcName = fmt.Sprintf("New%sFrom%s%s",
		dst.object.Name(), pkgName, src.object.Name())
	if g.funcNames[funcName] {
		return funcName
	}
	g.funcNames[funcName] = true

	fmt.Fprintf(&code, "// %s creates %s from %s\n", funcName, dst.Name(), src.FullName(g.rootDir))
	fmt.Fprintf(&code, "func %s (s %s) %s {\n", funcName, src.FullName(g.rootDir), dst.Name())
	fmt.Fprintf(&body, "	return %s{\n", dst.PtrName())
	for i := 0; i < srcInternal.NumFields(); i++ {
		for j := 0; j < dstInternal.NumFields(); j++ {
			srcField := srcInternal.Field(i)
			dstField := dstInternal.Field(j)
			srcTag, srcTagFound := reflect.StructTag(srcInternal.Tag(i)).Lookup("repack")
			dstTag, dstTagFound := reflect.StructTag(dstInternal.Tag(j)).Lookup("repack")

			if srcField.Name() == dstField.Name() || (srcTagFound && dstTagFound && (srcTag == dstTag)) {
				srcFieldCode := fmt.Sprintf("s.%s", srcField.Name())
				if !reflect.DeepEqual(srcField.Type(), dstField.Type()) {
					nestedSrcType := g.parseType(srcField.Type(), src.pkg)
					nestedDstType := g.parseType(dstField.Type(), dst.pkg)

					switch {
					case nestedDstType.name == "string":
						srcFieldCode = fmt.Sprintf(`fmt.Sprint(%s)`, srcFieldCode)
						if nestedDstType.isPointer {
							tmpSrcField := toLowerFirstChar(srcField.Name())
							fmt.Fprintf(&variables, "	%s := %s\n", tmpSrcField, srcFieldCode)
							srcFieldCode = "&" + tmpSrcField
						}
					case nestedDstType.isBasic:
						converter, err := g.generateConverteCode(nestedSrcType, nestedDstType.name)
						if err != nil {
							log.Printf("skip field (%s) due to difference types", srcField.Name())
							continue
						}
						srcFieldCode = fmt.Sprintf("s.%s.%s", srcField.Name(), converter)

						if nestedDstType.isPointer {
							tmpSrcField := toLowerFirstChar(srcField.Name())
							fmt.Fprintf(&variables, "	%s := %s\n", tmpSrcField, srcFieldCode)
							srcFieldCode = "&" + tmpSrcField
						}
					default:
						nestedFuncName, err := g.generate(nestedSrcType, nestedDstType)
						if err != nil {
							log.Printf("skip %s(%s) and %s(%s)\n", srcField.Name(), srcField.Type().String(),
								dstField.Name(), dstField.Type().String())
							continue
						}
						if nestedFuncName != "" {
							if !nestedSrcType.isSlice && !nestedSrcType.isPointer {
								srcFieldCode = fmt.Sprintf(`%s(&%s)`, nestedFuncName, srcFieldCode)
							} else {
								srcFieldCode = fmt.Sprintf(`%s(%s)`, nestedFuncName, srcFieldCode)
							}
							if !nestedDstType.isSlice && !nestedDstType.isPointer {
								srcFieldCode = fmt.Sprintf("*%s", srcFieldCode)
							}
						}
					}
				}
				fmt.Fprintf(&body, "		%s:  %s,\n", dstField.Name(), srcFieldCode)
				break
			}
		}
	}
	code.Write(variables.Bytes())
	code.Write(body.Bytes())
	code.WriteString("	}\n")
	code.WriteString("}\n")

	g.Printf(code.String())
	return funcName
}

func (g *Generator) generateSliceCode(src, dst Object) (funcName string) {
	nestedFunc := g.generateCode(src, dst)
	if !dst.typ.isPointer {
		nestedFunc = fmt.Sprintf("*%s", nestedFunc)
	}
	variable := "t"
	if !src.typ.isPointer {
		variable = "&t"
	}
	nestedFunc = fmt.Sprintf("%s(%s)", nestedFunc, variable)

	dstType := dst.object.Name()
	if dst.typ.isPointer {
		dstType = "Ptr" + dstType
	}
	srcType := strings.Title(src.pkg.name) + src.object.Name()
	if src.typ.isPointer {
		srcType = "Ptr" + srcType
	}
	funcName = fmt.Sprintf("New%sSliceFrom%s", dstType, srcType)
	if g.funcNames[funcName] {
		return funcName
	}
	g.funcNames[funcName] = true

	var code bytes.Buffer
	fmt.Fprintf(&code, "// %s creates []%s from []%s\n", funcName, dst.SliceName(), src.SliceFullName(g.rootDir))
	fmt.Fprintf(&code, "func %s (s []%s) (d []%s) {\n", funcName, src.SliceFullName(g.rootDir), dst.SliceName())
	fmt.Fprintf(&code, "	for _, t := range s{\n")
	fmt.Fprintf(&code, "		d = append(d, %s)\n", nestedFunc)
	fmt.Fprintf(&code, "	}\n")
	fmt.Fprintf(&code, "	return d\n")
	fmt.Fprintf(&code, "}\n")
	g.Printf(code.String())

	return funcName
}

func (g *Generator) generateConverteCode(typ Type, primitive string) (string, error) {
	pkg := g.parsePackageDir(typ.dir)

	conf := types.Config{
		Importer: importer.For("source", nil),
		Error: func(err error) {
			fmt.Printf("!!! %#v\n", err)
		},
	}

	obj, err := g.lookup(conf, pkg, typ)
	if err != nil {
		return "", errors.Wrapf(err, "Lookup: %s.%s", pkg.name, typ.name)
	}
	s := obj.Type().Underlying().(*types.Struct)
	for i := 0; i < s.NumFields(); i++ {
		field := s.Field(i)
		if field.Exported() && primitive == strings.ToLower(field.Name()) && primitive == field.Type().String() {
			return field.Name(), nil
		}
	}

	return "", errors.New("not found")

}

func (g *Generator) goimport(formatOnly bool) ([]byte, error) {
	opts := &imports.Options{
		Comments:  true,
		TabIndent: true,
		TabWidth:  8,
	}
	if formatOnly {
		opts.FormatOnly = true
	}
	src, err := imports.Process("", g.buf.Bytes(), opts)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to formats and adjusts imports for the provided file")
	}
	return src, nil
}

type Package struct {
	dir      string
	name     string
	astFiles []*ast.File
	fset     *token.FileSet
}

// isDirectory reports whether the named file is a directory.
func isDirectory(name string) bool {
	info, err := os.Stat(name)
	if err != nil {
		log.Fatal(err)
	}
	return info.IsDir()
}

func splitType(name string) (importPath, typeName string) {
	token := strings.Split(name, ".")
	if len(token) == 1 {
		return "", name
	}
	importPath = strings.Join(token[:len(token)-1], ".")
	typeName = token[len(token)-1]
	return importPath, typeName
}

func toLowerFirstChar(str string) string {
	for i, v := range str {
		return string(unicode.ToLower(v)) + str[i+1:]
	}
	return ""
}
