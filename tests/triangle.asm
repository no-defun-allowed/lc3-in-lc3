        .orig x3000
        add r0, r0, 10
loop
        add r1, r1, r0
        add r0, r0, #-1
        brp loop
        halt
        .end
