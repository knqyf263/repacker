package foo

type Foo struct {
	ID   int
	Name string
	Nest NestedFoo
}

type NestedFoo struct {
	ID int
}
