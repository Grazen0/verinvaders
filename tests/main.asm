.org 0x0000

    lxi     h, 0x2000
    lxi     b, 0x0000

    inx     b
    inx     b

    mvi     a, 0x42
    sbi     0x02
    inx     b
    inx     b
