package main

//#import Lib.go

import "fmt"

func main() {
	var e = empty{}
	var t = T{1}
	var _ = e.i1ToS(t, false)
	fmt.Printf("NOT OK")
}
