; Global variables are prefixed with '@'.
; Local variables are prefixed with '%'.
; Everything is type-annotated.

; Functions are defined globally.
define i32 @addmul(i32 %a, i32 %b, i32 %c) {
  ; Static Single Assignment form.
  ; Every name is assigned to exactly once,
  ; and defined before used.
  %1 = mul i32 %b, %c
  %2 = add i32 %a, %1
  ret i32 %2
}

define i32 @main() {
  %1 = call i32 @addmul(i32 1, i32 2, i32 3)
  ret i32 %1
}

; llc -march=x86-64 01_addmul.ll
; lli 01_addmul.ll ; echo $?
