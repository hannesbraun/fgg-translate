package main

import "fmt"

type empty struct {}

type t struct {
	f int
}

type s struct {
	f int
	g bool
}

type I interface {}

func (r empty) foo(x s) I { return x }
func (r empty) bar(x s) bool { return x.g }

func main() {
	var r = empty{}
	var x = r.foo(s{1, true})
	var y = r.bar(x.(s))
	fmt.Printf("%v", y)
	// foo(x); does not work
}
