package bar

type BarTag struct {
        ID   int
        Name string
        Bar  string `repack:"foo"`
}
