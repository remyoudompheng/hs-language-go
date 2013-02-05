package main;

/*
 * This test does not work in gc/gccgo because it uses special Unicode syntax
 */

/*
multi-line
*******
comment
*/

//single-line comment

func #[fraçais]() {
    return 0;
};

func #[今日は]() {
    return 0;
};

func Main() int {
    return 1;
};