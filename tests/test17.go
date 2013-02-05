package main

/*
 This tests for the channel type (THIS WILL FAIL FOR NOW)
 */

type MyType int
type MyChan1 <- chan MyType
type MyChan2 chan <- MyType
type MyChan3 chan MyType
