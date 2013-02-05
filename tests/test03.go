package main;

type myInt int32;

func myFunc(x myInt) myInt;
func (x *myInt) myMeth(y myInt) myInt { return 1; };

