define i32 @factorial(i32 %n) {
 ; Blocks can be named with labels
entry:
  %cond = icmp eq i32 %n, 0
  ; Branch instruction jumps to labels.
  ; Either conditionally.
  br i1 %cond, label %basecase, label %recurse

basecase:
  ; Or unconditionally.
  br label %return

recurse:
  %pred = sub i32 %n, 1
  %pred_fac = call i32 @factorial(i32 %pred)
  %rec_fac = mul i32 %n, %pred_fac
  br label %return

return:
  ; phi joins multiple paths.
  %res = phi i32 [ 1, %basecase ], [ %rec_fac, %recurse ]
  ret i32 %res
}

define i32 @main() {
  %1 = call i32 @factorial(i32 5)
  ret i32 %1
}

; llc -march=x86-64 02_factorial.ll
; lli 02_factorial.ll ; echo $?
