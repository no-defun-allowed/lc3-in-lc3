        .orig 0x3000
loop    LEA R0, hello    ; R0 = &hello
        TRAP 0x22        ; PUTS (print char array at addr in R0)
        LD R1, hello
        ADD R1, R1, #1
        ST R1, hello
        BRp loop
        TRAP 0x25        ; HALT
hello   .stringz "Hello World!"
        .end
