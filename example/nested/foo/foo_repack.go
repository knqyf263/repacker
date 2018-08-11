// Code generated by "repacker -dst=Foo -src=github.com/knqyf263/repacker/example/nested/bar.Bar foo/"; DO NOT EDIT

package foo

import "github.com/knqyf263/repacker/example/nested/bar"

// NewNestedFooFromBarNestedBar creates *NestedFoo from *bar.NestedBar
func NewNestedFooFromBarNestedBar(s *bar.NestedBar) *NestedFoo {
	return &NestedFoo{
		ID: s.ID,
	}
}

// NewFooFromBarBar creates *Foo from *bar.Bar
func NewFooFromBarBar(s *bar.Bar) *Foo {
	return &Foo{
		ID:   s.ID,
		Name: s.Name,
		Nest: *NewNestedFooFromBarNestedBar(&s.Nest),
	}
}
