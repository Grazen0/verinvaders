.org 0x0000

    mvi     a, 0x42
init:
    add     a

    lxi     h, init
    pchl

loop:
    jmp     loop

thing:
    dcr     l
    jnz     thing

    mvi     a, 0x69
    nop
    nop

