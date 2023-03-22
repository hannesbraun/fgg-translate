package main

import "fmt"

type empty struct {}

type I interface {
	foo(s S) int
}

type S struct {
	f1 int
	f2 I
}

func (r S) foo(x S) int {
	return r.f1
}

func (r empty) foo(x S) int {
	return x.f2.foo(x)
}

func main() {
	var e = empty{}
	var s1 = S{1, e}
	var s2 = S{2, s1}
	var result = e.foo(s2)
	fmt.Printf("%v", result)
}
