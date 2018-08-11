package foo

type FooTag struct {
        ID   int
        Name string
        Foo  string `repack:"foo"`
}
