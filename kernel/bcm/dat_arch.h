
struct Soc {            /* SoC dependent configuration */
    ulong   dramsize;
    uintptr physio;
    uintptr busdram;
    uintptr busio;
    u32int  l1ptedramattrs;
    u32int  l2ptedramattrs;
    uintptr armlocal;
};

extern Soc soc;
