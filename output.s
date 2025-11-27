.text
.global _w
_w:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    mov x9, #0
    str x9, [x29, #-8]
    mov x9, #0
    str x9, [x29, #-16]
while_start_0:
    ldr x9, [x29, #-8]
    str x9, [x29, #-24]
    mov x10, #20
    ldr x9, [x29, #-24]
    cmp x9, x10
    cset x9, lt
    cmp x9, #0
    b.ne while_body_1
    b while_end_2
while_body_1:
    ldr x9, [x29, #-8]
    str x9, [x29, #-32]
    mov x10, #1
    ldr x9, [x29, #-32]
    add x9, x9, x10
    str x9, [x29, #-8]
    ldr x9, [x29, #-8]
    str x9, [x29, #-40]
    mov x10, #3
    ldr x9, [x29, #-40]
    sdiv x11, x9, x10
    mul x11, x11, x10
    sub x9, x9, x11
    str x9, [x29, #-48]
    mov x10, #0
    ldr x9, [x29, #-48]
    cmp x9, x10
    cset x9, eq
    cmp x9, #0
    b.ne then_3
    b else_4
then_3:
    b while_start_0
    b endif_5
else_4:
    b endif_5
endif_5:
    ldr x9, [x29, #-8]
    str x9, [x29, #-56]
    mov x10, #15
    ldr x9, [x29, #-56]
    cmp x9, x10
    cset x9, ge
    cmp x9, #0
    b.ne then_6
    b else_7
then_6:
    b while_end_2
    b endif_8
else_7:
    b endif_8
endif_8:
    ldr x9, [x29, #-16]
    str x9, [x29, #-64]
    ldr x9, [x29, #-8]
    mov x10, x9
    ldr x9, [x29, #-64]
    add x9, x9, x10
    str x9, [x29, #-16]
    b while_start_0
while_end_2:
    ldr x9, [x29, #-8]
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
.global _factorial
_factorial:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    mov x9, #1
    str x9, [x29, #-16]
    mov x9, #1
    str x9, [x29, #-24]
while_start_9:
    ldr x9, [x29, #-24]
    str x9, [x29, #-32]
    ldr x9, [x29, #-8]
    mov x10, x9
    ldr x9, [x29, #-32]
    cmp x9, x10
    cset x9, le
    cmp x9, #0
    b.ne while_body_10
    b while_end_11
while_body_10:
    ldr x9, [x29, #-16]
    str x9, [x29, #-40]
    ldr x9, [x29, #-24]
    mov x10, x9
    ldr x9, [x29, #-40]
    mul x9, x9, x10
    str x9, [x29, #-16]
    ldr x9, [x29, #-24]
    str x9, [x29, #-48]
    mov x10, #1
    ldr x9, [x29, #-48]
    add x9, x9, x10
    str x9, [x29, #-24]
    b while_start_9
while_end_11:
    ldr x9, [x29, #-16]
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
.global _fib
_fib:
    stp x29, x30, [sp, #-256]!
    mov x29, sp
    str x0, [x29, #-8]
    mov x9, #0
    str x9, [x29, #-16]
    mov x9, #1
    str x9, [x29, #-24]
    mov x9, #0
    str x9, [x29, #-32]
while_start_12:
    ldr x9, [x29, #-32]
    str x9, [x29, #-40]
    ldr x9, [x29, #-8]
    mov x10, x9
    ldr x9, [x29, #-40]
    cmp x9, x10
    cset x9, lt
    cmp x9, #0
    b.ne while_body_13
    b while_end_14
while_body_13:
    ldr x9, [x29, #-16]
    str x9, [x29, #-48]
    ldr x9, [x29, #-24]
    mov x10, x9
    ldr x9, [x29, #-48]
    add x9, x9, x10
    str x9, [x29, #-56]
    ldr x9, [x29, #-24]
    str x9, [x29, #-16]
    ldr x9, [x29, #-56]
    str x9, [x29, #-24]
    ldr x9, [x29, #-32]
    str x9, [x29, #-64]
    mov x10, #1
    ldr x9, [x29, #-64]
    add x9, x9, x10
    str x9, [x29, #-32]
    b while_start_12
while_end_14:
    ldr x9, [x29, #-16]
    mov x0, x9
    ldp x29, x30, [sp], #256
    ret
