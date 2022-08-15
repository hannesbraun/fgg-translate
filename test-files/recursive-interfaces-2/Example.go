package main

import "fmt"

type U1 interface {
	fooU(x U2) bool
}

type U2 interface {
	barU(x U1) bool
}

type B struct { f bool }

func (b B) fooU(x U2) bool {
	return x.barU(b)
}

func (b B) barU(x U1) bool {
	return true
}


func main() {
	var y = B{true}
	var result = y.fooU(y)
	fmt.Printf("%v", result)
}
