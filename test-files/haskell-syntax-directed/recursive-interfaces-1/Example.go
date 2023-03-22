package main

import "fmt"

type I1 interface {
	foo() I2
}

type I2 interface {
	bar() I1
}

type T struct { f int }

func (t T) foo() I2 { return t }

func (t T) bar() I1 { return t }

func main() {
	var result = T{1}.foo().bar()
	fmt.Printf("%v", result)
}
