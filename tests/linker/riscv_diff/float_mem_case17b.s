// case 17 (OOP_FP family), memory side: FLD/FSD (real RISC-V's
// LOAD-FP/STORE-FP major opcodes 0x07/0x27, a genuinely different
// major opcode from the integer LOAD/STORE's 0x03/0x23, same funct3
// convention) -- both plain register-indirect ("MOVD F,off(R)") and
// SB-relative ("MOVD sym(SB),F", reading a real global double table)
// forms. Also MOVUF/MOVUD (unsigned-int->float/double, real RISC-V
// FCVT.S/D.WU -- same funct7 as the already-verified signed MOVWF/
// MOVWD, only the rs2-select field differs, 1 instead of 0).
//
// This whole "off(SP)/off(FP) pseudo-frame-relative float memory
// access, and SB-relative float loads" story was completely
// unimplemented before -- found stress-testing real lib_core/libc
// (fmt/strtod.c's own real "MOVD x-8(SP),F1"/"MOVD F0,x-8(SP)", and
// "MOVD pows10<>+1272(SB),F2" reading a real global double table).
TEXT _start(SB), $16
    MOVW $setSB(SB), R3
    MOVD tbl+8(SB), F1     // F1 = 20.0 (SB-relative load)
    MOVD F1, x-8(SP)       // store to local frame slot
    MOVD x-8(SP), F2       // reload -- F2 = 20.0
    MOVUD R9, F3           // R9=0 default -> F3 = 0.0 (unsigned int->double)
    ADDD F3, F2, F4        // F4 = F2 + F3 = 20.0
    MOVUF R9, F5           // F5 = 0.0 (single precision)
    CMPEQD F2, F4, R8      // F4 == F2 -> 1
    MOVW R8, R10
    MOVW $93, R17
    ECALL
    GLOBL tbl(SB), $16
    DATA tbl+0(SB)/8, $1.00000000000000000e+01
    DATA tbl+8(SB)/8, $2.00000000000000000e+01
