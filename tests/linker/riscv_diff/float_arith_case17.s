// case 17 (OOP_FP family): FMOV ("MOVD Fs,Fd", real RISC-V's own
// "FSGNJ.D Fd,Fs,Fs" self-sign-inject move idiom -- no dedicated
// float-move opcode exists), ArithF (ADDD/SUBD/MULD/DIVD, the
// 3-explicit-register form), and CmpF (CMPEQD/CMPLTD/CMPLED, a
// floating-point compare writing 0/1 into a GP register, real RISC-V
// FEQ/FLT/FLE.D). F1's own initial value comes from a real GLOBL/DATA
// double, not a bare "MOVD $2.5,F1" immediate -- real goken's own ia
// rejects that syntax outright, confirmed empirically (no C_FCONST
// row exists for AMOVD taking a *register* destination directly, only
// for the linker's own internal ADATA-symbol rewrite -- see
// Codegeni.ml's own "MOVD $const,Fd" comment for the fuller story,
// and tests/linker/hello_libc_riscv/'s own real "MOVD
// $2.68435456000000000e+08,F2" case, which goken's ia can't
// reassemble either).
//
// A real bug was found and fixed while writing this: ArithF/CmpF's
// `from`/`middle`/`to` operands map to rs2/rs1/rd, NOT the "obvious"
// left-to-right rs1/rs2/rd reading -- an earlier attempt assumed
// left-to-right and produced byte-identical *size* but wrong
// *content*, caught by hand-decoding real ia/il's own encoded
// instruction words, not by trusting the derivation (same asymmetric
// convention as Bxx's own middle/rf mapping, case 3's own fixture).
TEXT _start(SB), $0
    MOVW $setSB(SB), R3
    MOVD half(SB), F1     // F1 = 2.5
    MOVD F1, F2           // FMOV: F2 = F1 = 2.5
    ADDD F1, F2, F3        // F3 = F2 + F1 = 5.0
    SUBD F1, F3, F4        // F4 = F3 - F1 = 2.5
    MULD F1, F4, F5        // F5 = F4 * F1 = 6.25
    DIVD F1, F5, F6        // F6 = F5 / F1 = 2.5
    CMPEQD F4, F6, R8      // F6 == F4 (both 2.5) -> 1
    CMPLTD F3, F1, R9      // from=F3,middle=F1: F1(2.5) < F3(5.0) -> 1
    CMPLED F1, F4, R10     // from=F1,middle=F4: F4(2.5) <= F1(2.5) -> 1
    ADD R9, R8, R8
    ADD R10, R8, R8        // R8 = 1+1+1 = 3
    MOVW R8, R10
    MOVW $93, R17
    ECALL
DATA half+0(SB)/8, $2.50000000000000000e+00
GLOBL half(SB), $8
