package main

//#import Lib.go

import "fmt"

func main() {
	var e = empty{}
	var u = U{1}
	var _ = e.i2ToT(u, false)
	fmt.Printf("NOT OK")
}
