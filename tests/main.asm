.org 0x0000
    jmp     init

.org 0x0010
rst_16:
    nop
    nop
    ret

init:
    lxi     sp, 0x2800

    mvi     a, 8
    call    fib

loop:
    jmp     loop

my_fn:
    nop
    rz

fib:
    cpi     2
    rc

    dcr     a
    push    psw
    call    fib

    pop     b
    dcr     b
    push    psw
    mov     a, b
    call    fib

    pop     b
    add     b

    ret
