package main

type I1 interface {
	foo(x I2) I1
}

type I2 interface {
	bar(x I1) I1
}

type empty struct {}

func (r empty) consumeI1(x I1) I1 {
    return x
}

type T struct {f int}

func (x T) foo(y I2) I1 {
	return empty{}.consumeI1(y.bar(x))
}

func (x T) bar(y I1) I1 {
	return y
}

func main() {
	var x = T { 42 }
	var result = x.foo(x)
	fmt.Printf("%v", result)
}
