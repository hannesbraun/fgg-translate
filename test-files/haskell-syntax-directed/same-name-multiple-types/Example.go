package main

import "fmt"

type I1 interface { spam() bool }

type I2 interface { egg() int }

type T struct {f int}

func (t T) foo(x I1) bool {
	return x.spam()
}

type U struct {g int}

func (t U) foo(x I2) int {
	return x.egg()
}

type V struct {h bool}
func (t V) spam() bool {return true}
func (t V) egg() int {return 1}

type W struct {f2 int}
func (t W) spam() bool {return false}

func (t T) use(b bool, b2 bool, i int) bool {return true}

func main() {
	var x = T{0}
	var y = U{1}
	var i = V{false}
	var j = W{2}
	var r1 /*bool*/ = x.foo(i)
	var r2 /*bool*/ = x.foo(j)
	var r3 /*int*/ = y.foo(i)
	var r4 = x.use(r1, r2, r3)
	fmt.Printf("%v", r4)
}
