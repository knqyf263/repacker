package foo

//go:generate repacker -dst=FooConversion -src=github.com/knqyf263/repacker/example/conversion/bar.BarConversion

type FooConversion struct {
        ID        int
        Name      string
        CreatedAt *string
}
