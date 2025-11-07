#define CST 42
        
        TEXT _main(SB), $4
        MOVW $CST, R1
        ADD R10, R1, R0
        RET
        