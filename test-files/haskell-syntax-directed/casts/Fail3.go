package main

//#import Lib.go

import "fmt"

func main() {
	var e = empty{}
	var s = S{0, true}
	var _ = e.i1ToI2(s, false)
	fmt.Printf("NOT OK")
}
