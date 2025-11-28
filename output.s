.text
.global _get_unit
_get_unit:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    ldp x29, x30, [sp], #256
    ret
.global _returns_nothing
_returns_nothing:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    mov x9, #10
    str x9, [x29, #-8]
    mov x9, #20
    str x9, [x29, #-16]
    ldp x29, x30, [sp], #256
    ret
.global _test_unit
_test_unit:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    bl _get_unit
    mov x0, #42
    ldp x29, x30, [sp], #256
    ret
