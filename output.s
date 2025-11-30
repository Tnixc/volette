.text
.global _test
_test:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    str x1, [x29, #-16]
    ldp x29, x30, [sp], #256
    ret
