
struct Soc {            /* SoC dependent configuration */
    ulong   dramsize;
    uintptr physio;
    uintptr busdram;
    uintptr busio;
    uintptr armlocal;
};

extern Soc soc;
