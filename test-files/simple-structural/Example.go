package main

import "fmt"

type empty struct {}
type IntWrap struct {f int}

type A1 interface {
	methodA(b bool) int
}

func (x IntWrap) methodA(b bool) int {
	return 1
}

func (r empty) useA1(x A1) int {
	return x.methodA(false)
}

type A2 interface {
	methodA(b bool) int
}

func (r empty) useA2(x A2) int {
	return r.useA1(x)
}

func main() {
	var r = empty{}
	var x = IntWrap{42}
	var _ = x.methodA(true)
	var r2 = r.useA2(x)
	fmt.Printf("%v", r2)
}
