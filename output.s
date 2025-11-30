.text
.global _add
_add:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    str x1, [x29, #-16]
    ldr x9, [x29, #-8]
    str x9, [x29, #-24]
    ldr x9, [x29, #-16]
    mov x10, x9
    ldr x9, [x29, #-24]
    add x9, x9, x10
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
.global _sub
_sub:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    str x1, [x29, #-16]
    ldr x9, [x29, #-8]
    str x9, [x29, #-24]
    ldr x9, [x29, #-16]
    mov x10, x9
    ldr x9, [x29, #-24]
    sub x9, x9, x10
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
.global _compute
_compute:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    str x1, [x29, #-16]
    ldr x9, [x29, #-8]
    mov x0, x9
    ldr x9, [x29, #-16]
    mov x1, x9
    bl _add
    mov x9, x0
    str x9, [x29, #-24]
    ldr x9, [x29, #-8]
    mov x0, x9
    ldr x9, [x29, #-16]
    mov x1, x9
    bl _sub
    mov x9, x0
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
