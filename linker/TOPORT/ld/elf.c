enum {
	/* offsets into string table */
	Stitext		= 1,
	Stidata		= 7,
	Stistrtab	= 13,
};

void
elfstrtab(void)
{
	/* string table */
	cput(0);
	strnput(".text", 5);		/* +1 */
	cput(0);
	strnput(".data", 5);		/* +7 */
	cput(0);
	strnput(".strtab", 7);		/* +13 */
	cput(0);
	cput(0);
}


void
elf32shdr(void (*putl)(long), ulong name, ulong type, ulong flags, ulong vaddr,
	ulong off, ulong sectsz, ulong link, ulong addnl, ulong align,
	ulong entsz)
{
	putl(name);
	putl(type);
	putl(flags);
	putl(vaddr);
	putl(off);
	putl(sectsz);
	putl(link);
	putl(addnl);
	putl(align);
	putl(entsz);
}

static void
elf32sectab(void (*putl)(long))
{
	seek(cout, HEADR+textsize+datsize+symsize, 0);
	elf32shdr(putl, Stitext, Progbits, Salloc|Sexec, INITTEXT,
		HEADR, textsize, 0, 0, 0x10000, 0);
    //NEW: must rnd() data segment for ELF Linux
	elf32shdr(putl, Stidata, Progbits, Salloc|Swrite, INITDAT,
		rnd(HEADR+textsize, INITRND), datsize, 0, 0, 0x10000, 0);
	elf32shdr(putl, Stistrtab, Strtab, 1 << 5, 0,
		HEADR+textsize+datsize+symsize+3*Shdr32sz, 14, 0, 0, 1, 0);
	elfstrtab();
}

/* if addpsects > 0, putpsects must emit exactly that many psects. */
void
elf32(int mach, int bo, int addpsects, void (*putpsects)(Putl))
{
	ulong phydata;
    ...

	/*
	 * we need INITDATP, but it has to be computed.
	 * assume distance between INITTEXT & INITTEXTP is also
	 * correct for INITDAT and INITDATP.
	 */
	phydata = INITDAT - (INITTEXT - INITTEXTP);
    //NEW: must rnd() data segment for ELF Linux
	elf32phdr(putl, PT_LOAD, rnd(HEADR+textsize, INITRND), INITDAT, phydata,
		datsize, datsize+bsssize, R|W|X, INITRND); /* data */
	elf32phdr(putl, NOPTYPE, HEADR+textsize+datsize, 0, 0,
		symsize, lcsize, R, 4);			/* symbol table */


	if(debug['S'])
		elf32sectab(putl);
}




/*
 * elf64
 */

void
elf64phdr(void (*putl)(long), void (*putll)(vlong), ulong type, uvlong off,
	uvlong vaddr, uvlong paddr, uvlong filesz, uvlong memsz, ulong prots,
	uvlong align)
{
	putl(type);		
	putl(prots);		
	putll(off);		
	putll(vaddr);	
	putll(paddr);	
	putll(filesz);	
	putll(memsz);	
	putll(align);		
}

void
elf64shdr(void (*putl)(long), void (*putll)(vlong), ulong name, ulong type,
	uvlong flags, uvlong vaddr, uvlong off, uvlong sectsz, ulong link,
	ulong addnl, uvlong align, uvlong entsz)
{
	putl(name);
	putl(type);
	putll(flags);
	putll(vaddr);
	putll(off);
	putll(sectsz);
	putl(link);
	putl(addnl);
	putll(align);
	putll(entsz);
}

static void
elf64sectab(void (*putl)(long), void (*putll)(vlong))
{
	seek(cout, HEADR+textsize+datsize+symsize, SEEK__START);
	elf64shdr(putl, putll, Stitext, Progbits, Salloc|Sexec, INITTEXT,
		HEADR, textsize, 0, 0, 0x10000, 0);
    //NEW: must rnd() data segment for ELF Linux
	elf64shdr(putl, putll, Stidata, Progbits, Salloc|Swrite, INITDAT,
		rnd(HEADR+textsize, INITRND)/*old: HEADR+textsize*/, datsize, 0, 0, 0x10000, 0);
	elf64shdr(putl, putll, Stistrtab, Strtab, 1 << 5, 0,
		HEADR+textsize+datsize+symsize+3*Shdr64sz, 14, 0, 0, 1, 0);
	elfstrtab();
}

/* if addpsects > 0, putpsects must emit exactly that many psects. */
void
elf64(int mach, int bo, int addpsects, void (*putpsects)(Putl))
{
	uvlong phydata;
	void (*putw)(long), (*putl)(long);
	void (*putll)(vlong);

	if(bo == ELFDATA2MSB){
		putw = wput;
		putl = lput;
		putll = llput;
	}else if(bo == ELFDATA2LSB){
		putw = wputl;
		putl = lputl;
		putll = llputl;
	}else{
		print("elf64 byte order is mixed-endian\n");
		errorexit();
		return;
	}

	elfident(bo, ELFCLASS64);
	putw(EXEC);
	putw(mach);
	putl(1L);			/* version = CURRENT */
	putll(entryvalue());		/* entry vaddr */
	putll(Ehdr64sz);		/* offset to first phdr */
	if(debug['S'])
		putll(HEADR+textsize+datsize+symsize); /* offset to 1st shdr */
	else
		putll(0);
	putl(0L);			/* flags */
	putw(Ehdr64sz);
	putw(Phdr64sz);
	putw(3 + addpsects);		/* # of Phdrs */
	putw(Shdr64sz);
	if(debug['S']){
		putw(3);		/* # of Shdrs */
		putw(2);		/* Shdr table index */
	}else{
		putw(0);
		putw(0);
	}

	elf64phdr(putl, putll, PT_LOAD, HEADR, INITTEXT, INITTEXTP,
		textsize, textsize, R|X, INITRND);	/* text */
	/*
	 * see 32-bit ELF case for physical data address computation.
	 */
	phydata = INITDAT - (INITTEXT - INITTEXTP);
    //NEW: must rnd() data segment for ELF Linux
	elf64phdr(putl, putll, PT_LOAD, rnd(HEADR+textsize, INITRND)/*old: HEADR+textsize*/, INITDAT, phydata,
		datsize, datsize+bsssize, R|W, INITRND); /* data */
	elf64phdr(putl, putll, NOPTYPE, HEADR+textsize+datsize, 0, 0,
		symsize, lcsize, R, 4);			/* symbol table */
	if (addpsects > 0)
		putpsects(putl);
	cflush();

	if(debug['S'])
		elf64sectab(putl, putll);
}
