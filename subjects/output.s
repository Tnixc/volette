.text
.global _ok
_ok:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    str x1, [x29, #-16]
    ldr x9, [x29, #-8]
    str x9, [x29, #-24]
    ldr x9, [x29, #-16]
    str x9, [x29, #-32]
    ldr x9, [x29, #-24]
    str x9, [x29, #-40]
    ldr x9, [x29, #-32]
    mov x10, x9
    ldr x9, [x29, #-40]
    add x9, x9, x10
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
