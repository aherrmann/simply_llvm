#include "stdlib.h"

typedef struct {
    int (*funptr)(void *, int);
    void *envptr;
} closure;

typedef struct {
    int a;
} adder_env;

int call_adder(void *envptr, int b) {
    int a = ((adder_env *)envptr)->a;
    return a + b;
}

/*
    mkAdder (a : Int) : Int -> Int =
        (+) a
*/
closure make_adder(int a) {
    adder_env *envptr = (adder_env *)malloc(sizeof(adder_env));
    envptr->a = a;
    closure adder = { call_adder, (void *)envptr };
    return adder;
}

/*
    apply (f : Int -> Int) (x : Int) : Int =
        f x
*/
int apply(closure f, int x) {
    return f.funptr(f.envptr, x);
}

/*
main =
    let
        add3 = mkAdder 3
    in
        apply add3 5
*/
int main() {
    closure add3 = make_adder(3);
    return apply(add3, 5);
}

/*
./03_closure ; echo $?
clang -O -S -emit-llvm 03_closure.c -o 03_closure.ll
*/
