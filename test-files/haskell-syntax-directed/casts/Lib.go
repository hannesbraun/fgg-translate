package main

type empty struct {}

type T struct {
	f int
}

type S struct {
	f int
	g bool
}

type U struct {h int}

type I1 interface {
	foo() empty
	spam(x I2) I1
}

type I2 interface {
	bar() empty
}

// T implements I1 and I2
func (t T) foo() empty { return empty{} }
func (t T) spam(x I2) I1 {
	return t
}
func (t T) bar() empty { return empty{} }

// S implements I1 only
func (s S) foo() empty { return empty{} }
func (s S) spam(x I2) I1 {
	return s
}

// U implements a function foo with a different signature
func (u U) foo(b bool) empty { return empty{} }
// U implements I2
func (u U) bar() empty { return empty{} }

func (r empty) i1ToS(x I1, exp bool) S {
	return x.(S)
}
// func i2ToS(x I2) S { return x.(S) }

func (r empty) i1ToT(x I1, exp bool) T {
	return x.(T)
}

func (r empty) i2ToT(x I2, exp bool) T {
	return x.(T)
}

func (r empty) i1ToI2(x I1, exp bool) I2 {
	return x.(I2)
}

func (r empty) i2ToI1(x I2, exp bool) I1 {
	return x.(I1)
}
