package main

import "fmt"

type I1 interface {
	foo(x I2) I1
}

type I2 interface {
	bar(x I1) I1
}

type T struct {f int}

func (x T) foo(y I2) I1 {
	return y.bar(x) // result type is I1
}

func (x T) bar(y I1) I1 {
	return y
}

func main() {
	var x = T{42}
	var result = x.foo(x)
	fmt.Printf("%v", result)
}
