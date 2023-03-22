package main

import "fmt"

type I interface {
	idString(x string) string
	idInt(x int) int
	idBool(x bool) bool
}

type B struct { f bool }

func (b B) idString(x string) string {
	return x
}

func (b B) idInt(x int) int {
	return x
}

func (b B) idBool(x bool) bool {
	return x
}

func main() {
	var result = B{true}.idString("foo")
	fmt.Printf("%v", result)
}
