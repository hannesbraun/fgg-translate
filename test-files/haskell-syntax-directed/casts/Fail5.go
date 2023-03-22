package main

//#import Lib.go

import "fmt"

func main() {
	var e = empty{}
	var u = U{1}
	var _ = e.i2ToI1(u, false)
	fmt.Printf("NOT OK")
}
