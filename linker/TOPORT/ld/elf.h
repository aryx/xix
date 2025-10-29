enum {
	Ehdr64sz	= 64,
	Phdr64sz	= 56,
	Shdr64sz	= 64,
};

enum {
	/* Shdr Codes */
	Progbits = 1,	/* section types */
	Strtab = 3,
	Nobits = 8,

	Swrite = 1,	/* section attributes (flags) */
	Salloc = 2,
	Sexec = 4,
};
