.org 0x0000
    jmp     init

.org 0x0010
rst_16:
    nop
    nop
    ret

init:
    lxi     sp, 0x2800

    mvi     a, 0x42
    sta     0x2000
    mvi     a, 0x00
    lda     0x2000

loop:
    jmp     loop

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
