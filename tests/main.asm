.org 0x0000
    jmp     init

.org 0x0008
    ret

.org 0x0010
    ret

init:
    lxi     sp, 0x2800

    mvi     a, 0b00110101
    sta     0x2400
    sta     0x2401
    sta     0x2403
    sta     0x2405
    sta     0x2406

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
