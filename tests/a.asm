        .orig x3000
        add r6, r6, #1
        add r5, r6, r6
        add r4, r5, r5
        add r3, r4, r4
        add r2, r3, r3
        add r1, r2, r2
        add r0, r1, r1
        add r0, r0, #1
        trap x21
        trap x25
        .end
