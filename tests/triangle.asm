        .orig x3000
        add r2, r2, 10
loop
        ld r0, dot
        trap x21
        add r1, r1, r2
        add r2, r2, #-1
        brp loop
        halt
dot     .fill #46
        .end
