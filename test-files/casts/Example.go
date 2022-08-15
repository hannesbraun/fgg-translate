package main

//#import Lib.go

import "fmt"

func main() {
	var e = empty{}
	var t = T{1}
	var s = S{0, true}
	//var u = U{1}

	//i1ToS(t, false)
	var _ = e.i1ToT(t, true)
	var _ = e.i2ToT(t, true)
	var _ = e.i1ToI2(t, true)
	var _ = e.i2ToI1(t, true)

	var res = e.i1ToS(s, true)
	//i1ToT(s, false)
	//i1ToI2(s, false)

	// i2ToT(u, false)
	//i2ToI1(u, false)
	fmt.Printf("%v", res)
}
