// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include	"l.h"
#include	"lib.h"
#include	"../ld/elf_.h"

/*
 * We use the 64-bit data structures on both 32- and 64-bit machines
 * in order to write the code just once.  The 64-bit data structure is
 * written in the 32-bit format on the 32-bit machines.
 */
#define	NSECT	32

int	iself;

static	int	elf64;
static	ElfEhdr	hdr;
static	ElfPhdr	*phdr[NSECT];
static	ElfShdr	*shdr[NSECT];
static	char	*interp;

typedef struct Elfstring Elfstring;
struct Elfstring
{
	char *s;
	int off;
};

static Elfstring elfstr[100];
static int nelfstr;

/*
 Initialize the global variable that describes the ELF header. It will be updated as
 we write section and prog headers.
 */
void
elfinit(void)
{
	iself = 1;

	switch(thechar) {
	// 64-bit architectures
	case '6':
		elf64 = 1;
		hdr.phoff = ELF64HDRSIZE;	/* Must be be ELF64HDRSIZE: first PHdr must follow ELF header */
		hdr.shoff = ELF64HDRSIZE;	/* Will move as we add PHeaders */
		hdr.ehsize = ELF64HDRSIZE;	/* Must be ELF64HDRSIZE */
		hdr.phentsize = ELF64PHDRSIZE;	/* Must be ELF64PHDRSIZE */
		hdr.shentsize = ELF64SHDRSIZE;	/* Must be ELF64SHDRSIZE */
		break;

    //goken: 
	case '5':
        // similar to default case below
		hdr.phoff = ELF32HDRSIZE;
		hdr.shoff = ELF32HDRSIZE;
		hdr.ehsize = ELF32HDRSIZE;
		hdr.phentsize = ELF32PHDRSIZE;
		hdr.shentsize = ELF32SHDRSIZE;
        //goken: Version5 EABI
        hdr.flags = 0x5000200;

	// 32-bit architectures
	default:
		hdr.phoff = ELF32HDRSIZE;	/* Must be be ELF32HDRSIZE: first PHdr must follow ELF header */
		hdr.shoff = ELF32HDRSIZE;	/* Will move as we add PHeaders */
		hdr.ehsize = ELF32HDRSIZE;	/* Must be ELF32HDRSIZE */
		hdr.phentsize = ELF32PHDRSIZE;	/* Must be ELF32PHDRSIZE */
		hdr.shentsize = ELF32SHDRSIZE;	/* Must be ELF32SHDRSIZE */
	}
}

void
elf64phdr(ElfPhdr *e)
{
	LPUT(e->type);
	LPUT(e->flags);
	VPUT(e->off);
	VPUT(e->vaddr);
	VPUT(e->paddr);
	VPUT(e->filesz);
	VPUT(e->memsz);
	VPUT(e->align);
}

void
elf32phdr(ElfPhdr *e)
{
	LPUT(e->type);
	LPUT(e->off);
	LPUT(e->vaddr);
	LPUT(e->paddr);
	LPUT(e->filesz);
	LPUT(e->memsz);
	LPUT(e->flags);
	LPUT(e->align);
}

void
elf64shdr(ElfShdr *e)
{
	LPUT(e->name);
	LPUT(e->type);
	VPUT(e->flags);
	VPUT(e->addr);
	VPUT(e->off);
	VPUT(e->size);
	LPUT(e->link);
	LPUT(e->info);
	VPUT(e->addralign);
	VPUT(e->entsize);
}

void
elf32shdr(ElfShdr *e)
{
	LPUT(e->name);
	LPUT(e->type);
	LPUT(e->flags);
	LPUT(e->addr);
	LPUT(e->off);
	LPUT(e->size);
	LPUT(e->link);
	LPUT(e->info);
	LPUT(e->addralign);
	LPUT(e->entsize);
}

uint32
elfwriteshdrs(void)
{
	int i;

	if (elf64) {
		for (i = 0; i < hdr.shnum; i++)
			elf64shdr(shdr[i]);
		return hdr.shnum * ELF64SHDRSIZE;
	}
	for (i = 0; i < hdr.shnum; i++)
		elf32shdr(shdr[i]);
	return hdr.shnum * ELF32SHDRSIZE;
}

void
elfsetstring(char *s, int off)
{
	if(nelfstr >= nelem(elfstr)) {
		diag("too many elf strings");
		errorexit();
	}
	elfstr[nelfstr].s = s;
	elfstr[nelfstr].off = off;
	nelfstr++;
}

uint32
elfwritephdrs(void)
{
	int i;

	if (elf64) {
		for (i = 0; i < hdr.phnum; i++)
			elf64phdr(phdr[i]);
		return hdr.phnum * ELF64PHDRSIZE;
	}
	for (i = 0; i < hdr.phnum; i++)
		elf32phdr(phdr[i]);
	return hdr.phnum * ELF32PHDRSIZE;
}

ElfPhdr*
newElfPhdr(void)
{
	ElfPhdr *e;

	e = malloc(sizeof *e);
	memset(e, 0, sizeof *e);
	if (hdr.phnum >= NSECT)
		diag("too many phdrs");
	else
		phdr[hdr.phnum++] = e;
	if (elf64)
		hdr.shoff += ELF64PHDRSIZE;
	else
		hdr.shoff += ELF32PHDRSIZE;
	return e;
}

ElfShdr*
newElfShstrtab(vlong name)
{
	hdr.shstrndx = hdr.shnum;
	return newElfShdr(name);
}

ElfShdr*
newElfShdr(vlong name)
{
	ElfShdr *e;

	e = malloc(sizeof *e);
	memset(e, 0, sizeof *e);
	e->name = name;
	if (hdr.shnum >= NSECT) {
		diag("too many shdrs");
	} else {
		shdr[hdr.shnum++] = e;
	}
	return e;
}

ElfEhdr*
getElfEhdr(void)
{
	return &hdr;
}

uint32
elf64writehdr(void)
{
	int i;

	for (i = 0; i < EI_NIDENT; i++)
		cput(hdr.ident[i]);
	WPUT(hdr.type);
	WPUT(hdr.machine);
	LPUT(hdr.version);
	VPUT(hdr.entry);
	VPUT(hdr.phoff);
	VPUT(hdr.shoff);
	LPUT(hdr.flags);
	WPUT(hdr.ehsize);
	WPUT(hdr.phentsize);
	WPUT(hdr.phnum);
	WPUT(hdr.shentsize);
	WPUT(hdr.shnum);
	WPUT(hdr.shstrndx);
	return ELF64HDRSIZE;
}

uint32
elf32writehdr(void)
{
	int i;

	for (i = 0; i < EI_NIDENT; i++)
		cput(hdr.ident[i]);
	WPUT(hdr.type);
	WPUT(hdr.machine);
	LPUT(hdr.version);
	LPUT(hdr.entry);
	LPUT(hdr.phoff);
	LPUT(hdr.shoff);
	LPUT(hdr.flags);
	WPUT(hdr.ehsize);
	WPUT(hdr.phentsize);
	WPUT(hdr.phnum);
	WPUT(hdr.shentsize);
	WPUT(hdr.shnum);
	WPUT(hdr.shstrndx);
	return ELF32HDRSIZE;
}

uint32
elfwritehdr(void)
{
	if(elf64)
		return elf64writehdr();
	return elf32writehdr();
}

/* Taken directly from the definition document for ELF64 */
uint32
elfhash(uchar *name)
{
	uint32 h = 0, g;
	while (*name) {
		h = (h << 4) + *name++;
		if (g = h & 0xf0000000)
			h ^= g >> 24;
		h &= 0x0fffffff;
	}
	return h;
}

void
elfwritedynent(Sym *s, int tag, uint64 val)
{
	if(elf64) {
		adduint64(s, tag);
		adduint64(s, val);
	} else {
		adduint32(s, tag);
		adduint32(s, val);
	}
}

void
elfwritedynentsym(Sym *s, int tag, Sym *t)
{
	if(elf64)
		adduint64(s, tag);
	else
		adduint32(s, tag);
	addaddr(s, t);
}

void
elfwritedynentsymsize(Sym *s, int tag, Sym *t)
{
	if(elf64)
		adduint64(s, tag);
	else
		adduint32(s, tag);
	addsize(s, t);
}

int
elfwriteinterp(void)
{
	int n;
	
	if(interp == nil)
		return 0;

	n = strlen(interp)+1;
	seek(cout, ELFRESERVE-n, 0);
	ewrite(cout, interp, n);
	return n;
}

void
elfinterp(ElfShdr *sh, uint64 startva, char *p)
{
	int n;
	
	interp = p;
	n = strlen(interp)+1;
	sh->addr = startva + ELFRESERVE - n;
	sh->off = ELFRESERVE - n;
	sh->size = n;
}

void
elfdynhash(int nsym)
{
	Sym *s, *sy;
	int i, h, nbucket, b;
	uchar *pc;
	uint32 hc, g;
	uint32 *chain, *buckets;

	s = lookup(".hash", 0);
	s->type = SELFDATA;	// TODO: rodata
	s->reachable = 1;

	i = nsym;
	nbucket = 1;
	while(i > 0) {
		++nbucket;
		i >>= 1;
	}

	chain = malloc(nsym * sizeof(uint32));
	memset(chain, 0, nsym * sizeof(uint32));
	buckets = malloc(nbucket * sizeof(uint32));
	memset(buckets, 0, nbucket * sizeof(uint32));
	i = 1;
	for(h = 0; h<NHASH; h++) {
		for(sy=hash[h]; sy!=S; sy=sy->hash) {
			if (!sy->reachable || (sy->type != STEXT && sy->type != SDATA && sy->type != SBSS) || sy->dynimpname == nil)
				continue;

			hc = 0;
			for(pc = (uchar*)sy->dynimpname; *pc; pc++) {
				hc = (hc<<4) + *pc;
				g = hc & 0xf0000000;
				hc ^= g >> 24;
				hc &= ~g;
			}

			b = hc % nbucket;
			chain[i] = buckets[b];
			buckets[b] = i;
			i++;
		}
	}

	adduint32(s, nbucket);
	adduint32(s, nsym);
	for(i = 0; i<nbucket; i++)
		adduint32(s, buckets[i]);
	for(i = 0; i<nsym; i++)
		adduint32(s, chain[i]);

	free(chain);
	free(buckets);
}

ElfPhdr*
elfphload(Segment *seg)
{
	ElfPhdr *ph;
	
	ph = newElfPhdr();
	ph->type = PT_LOAD;
	if(seg->rwx & 4)
		ph->flags |= PF_R;
	if(seg->rwx & 2)
		ph->flags |= PF_W;
	if(seg->rwx & 1)
		ph->flags |= PF_X;
	ph->vaddr = seg->vaddr;
	ph->paddr = seg->vaddr;
	ph->memsz = seg->len;
	ph->off = seg->fileoff;
	ph->filesz = seg->filelen;
	ph->align = INITRND;
	
	return ph;
}

ElfShdr*
elfshbits(Section *sect)
{
	int i, off;
	ElfShdr *sh;
	
	for(i=0; i<nelfstr; i++) {
		if(strcmp(sect->name, elfstr[i].s) == 0) {
			off = elfstr[i].off;
			goto found;
		}
	}
	diag("cannot find elf name %s", sect->name);
	errorexit();
	return nil;

found:
	sh = newElfShdr(off);
	if(sect->vaddr < sect->seg->vaddr + sect->seg->filelen)
		sh->type = SHT_PROGBITS;
	else
		sh->type = SHT_NOBITS;
	sh->flags = SHF_ALLOC;
	if(sect->rwx & 1)
		sh->flags |= SHF_EXECINSTR;
	if(sect->rwx & 2)
		sh->flags |= SHF_WRITE;
	sh->addr = sect->vaddr;
	sh->addralign = PtrSize;
	sh->size = sect->len;
	sh->off = sect->seg->fileoff + sect->vaddr - sect->seg->vaddr;
	
	return sh;
}
