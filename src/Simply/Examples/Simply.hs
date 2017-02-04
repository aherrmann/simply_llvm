module Simply.Examples.Simply where

import Protolude hiding (Type)
import Simply.AST.Simply


ex01a_factorial :: Program
ex01a_factorial = Program
  [
    Def "factorial'" [("acc", TInt), ("n", TInt)] TInt (
      If ("n" ==. int 0) {-then-} (
        "acc"
      ) {-else-} (
        "factorial'" @. [("acc" *. "n"), ("n" -. int 1)]
      )
    )
  , Def "factorial" [("n", TInt)] TInt (
      "factorial'" @. [int 1, "n"]
    )
  ] {-main-} [] (
    "factorial" @. [int 5]
  )


ex01b_factorial :: Program
ex01b_factorial = Program
  [
    Def "factorial'" [("acc", TInt), ("n", TInt)] TInt (
      If ("n" ==. int 0) {-then-} (
        "acc"
      ) {-else-} (
        "factorial'" @. [("acc" *. "n"), ("n" -. int 1)]
      )
    )
  , Def "factorial" [("n", TInt)] TInt (
      "factorial'" @. [int 1, "n"]
    )
  ] {-main-} [("n", TInt)] (
    "factorial" @. ["n"]
  )


ex01c_factorial :: Program
ex01c_factorial = Program
  [
    Def "factorial" [("n", TInt)] TInt (
      If ("n" ==. int 0) {-then-} (
        int 1
      ) {-else-} (
        "n" *. "factorial" @. ["n" -. int 1]
      )
    )
  ] {-in-} [] (
    "factorial" @. [int 5]
  )


ex01d_factorial :: Program
ex01d_factorial = Program
  [
    Def "factorial" [("n", TInt)] TInt (
      If ("n" ==. int 0) {-then-} (
        int 1
      ) {-else-} (
        "n" *. "factorial" @. ["n" -. int 1]
      )
    )
  ] {-in-} [("n", TInt)] (
    "factorial" @. ["n"]
  )


ex02a_higher_order :: Program
ex02a_higher_order = Program
  [
    Def "apply" [("f", TInt ->. TInt), ("x", TInt)] TInt (
      "f" @. ["x"]
    )
  , Def "mkAdder" [("a", TInt)] (TInt ->. TInt) (
      add @. ["a"]
    )
  ] {-main-} [("n", TInt)] (
    Let "add3" {-=-} ( "mkAdder" @. [int 3] ) {-in-} (
      "apply" @. ["add3", int 4]
    )
  )


ex02b_higher_order :: Program
ex02b_higher_order = Program
  [
    Def "apply" [("f", TInt ->. TInt), ("x", TInt)] TInt (
      "f" @. ["x"]
    )
  , Def "mkAdder" [("a", TInt)] (TInt ->. TInt) (
      add @. ["a"]
    )
  ] {-main-} [("n", TInt)] (
    Let "add3" {-=-} ( "mkAdder" @. [int 3] ) {-in-} (
      "apply" @. ["add3", "n"]
    )
  )


ex03_factorial_fix :: Program
ex03_factorial_fix = Program
  [
    Def "fix2i"
      [("f", (TInt ->. TInt ->. TInt) ->. TInt ->. TInt ->. TInt)]
      (TInt ->. TInt ->. TInt)
    (
      Let "f'" {-=-} (
        Lam [("a", TInt), ("b", TInt)]
        {-=>-} (
          "fix2i" @. ["f", "a", "b"]
        )
      ) {-in-} (
        "f" @. ["f'"]
      )
    )
  , Def "fac" [] (TInt ->. TInt) (
      Let "fac'" {-=-} (
        Lam [("self", TInt ->. TInt ->. TInt), ("acc", TInt), ("n", TInt)]
        {-=>-} (
          If ("n" ==. int 0) {-then-} (
            "acc"
          ) {-else-} (
            "self" @. ["acc" *. "n", "n" -. int 1]
          )
        )
      ) {-in-} (
        "fix2i" @. ["fac'", int 1]
      )
    )
  ] {-in-} [("n", TInt)] (
    "fac" @. ["n"]
  )
