package main

import "fmt"

type empty struct {}

type I interface {
	foo() int
}

func (r empty) bar(x I) int {
	return x.foo()
}

type T struct {f int}

func (r T) foo() int {
	return r.f
}

func main() {
	var t = T{1}
	var e = empty{}
	var result = e.bar(t)
	fmt.Printf("%v", result)
}
