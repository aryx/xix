// Exercises ADD/SUB/ADDW/SUBW/CMP/CMPW with an immediate too big for
// the real "addcon" shape (12 bits, optionally shifted left 12, see
// isaddcon) -- goken's own real fallback there is a genuine "$lcon"
// extended-register literal-pool mechanism this port doesn't
// implement, so this exercises the REGTMP-materialize-then-register-
// op substitute added this session at each of those call sites in
// Codegen7.ml. Not expected to be byte-identical to goken (an extra
// MOVZW + register-register op here vs goken's own direct addcon/
// lcon encoding), only functionally identical.
//
// 65536000 = 1000<<16: land 0xFFF == 0 but (v >> 12) = 16000 > 0xFFF,
// so isaddcon rejects it outright even though it fits a single
// MOVZW lane (the case actually found stress-testing real
// lib_core/libc, port/frexp.c's real "ADDW $268435456,R5").
TEXT _start(SB), $0
    MOV $10, R1
    ADD $65536000, R1, R2   // R2 = 65536010
    SUB $65536000, R2, R3   // R3 = 10
    ADDW $65536000, R1, R4  // 32-bit view: R4 = 65536010
    SUBW $65536000, R4, R5  // R5 = 10
    CMP $65536000, R1       // R1=10 < 65536000 (signed)
    BLT cmp_ok
    MOV $0, R0
    B end
cmp_ok:
    CMPW $65536000, R4      // R4=65536010 >= 65536000 (signed)
    BGE cmpw_ok
    MOV $0, R0
    B end
cmpw_ok:
    ADD R3, R5, R0          // R0 = 10 + 10 = 20
end:
    MOV $93, R8
    SVC $0
