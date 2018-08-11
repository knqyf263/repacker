package bar

type Bar struct {
	ID   int
	Name string
	Nest NestedBar
}

type NestedBar struct {
	ID int
}
