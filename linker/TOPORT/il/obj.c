char	thechar		= 'i';
char	*thestring 	= "riscv";

/*
 *	-H0						is headerless
 *	-H2 -T4128 -R4096		is plan9 format
 *	-H7 -T0x4000A0 -R4		is elf executable
 */

void
main(int argc, char *argv[])
{
	int c;
	char *a;

	ptrsize = 4;

	a = strrchr(argv[0], '/');
	if(a == nil)
		a = argv[0];
	else
		a++;
	if(*a == 'j')
		thechar = 'j';

	ARGBEGIN {
	default:
	case 'P':
		a = ARGF();
		if(a)
			INITTEXTP = atolwhex(a);
		break;
	} ARGEND


	if(debug['j'])
		thechar = 'j';
	if(thechar == 'j'){
		thestring = "riscv64";
		ptrsize = 8;
	}

	switch(HEADTYPE) {
	case 2:	/* plan 9 */
		HEADR = 32L;
		if(INITTEXT == -1)
			INITTEXT = 4128;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
 	case 7:	/* elf executable */
        // similar to 5l_ code
		HEADR = rnd(Ehdr32sz+3*Phdr32sz, 16);
		//alt: HEADR = rnd(52L+3*32L, 16);
		if(INITTEXT == -1)
			INITTEXT = 0x10000 + HEADR;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 4096;
		break;
	}

	nopalign = zprg;
	nopalign.as = AADD;
	nopalign.from.type = D_CONST;
	nopalign.to.type = D_REG;
	nopalign.to.reg = REGZERO;
	nopalign.reg = REGZERO;

	dtype = 4;
    ...
}

void
objfile(char *file)
{
    ...
	if(file[0] == '-' && file[1] == 'l') {
		sprint(name, "/usr/%clib/lib", thechar);
		strcat(name, file+2);
		strcat(name, ".a");
		file = name;
	}

	return;
}

int
zaddr(uchar *p, Adr *a, Sym *h[])
{
	int i, c;
	long l;
	Sym *s;
	Auto *u;


	switch(a->type) {

	case D_CTLREG:
	case D_BRANCH:
	case D_OREG:
	case D_CONST:
		a->offset = p[4] | (p[5]<<8) |
			(p[6]<<16) | (p[7]<<24);
		c += 4;
		break;

	case D_VCONST:
		while(nhunk < 12)
			gethunk();
		if((uintptr)hunk & 2){
			nhunk -= 4;
			hunk += 4;
		}
		a->vval = (vlong*)hunk;
		nhunk -= 8;
		hunk += 8;
		*(long*)a->vval = p[4] | (p[5]<<8) |
			(p[6]<<16) | (p[7]<<24);
		*((long*)a->vval + 1) = p[8] | (p[9]<<8) |
			(p[10]<<16) | (p[11]<<24);
		c += 8;
		break;

	case D_FCONST:
		while(nhunk < sizeof(Ieee))
			gethunk();
		a->ieee = (Ieee*)hunk;
		nhunk -= NSNAME;
		hunk += NSNAME;

		a->ieee->l = p[4] | (p[5]<<8) |
			(p[6]<<16) | (p[7]<<24);
		a->ieee->h = p[8] | (p[9]<<8) |
			(p[10]<<16) | (p[11]<<24);
		c += 8;
		break;
	}
	s = a->sym;
	if(s == S)
		return c;
	i = a->name;
	if(i != D_AUTO && i != D_PARAM)
		return c;

	l = a->offset;
	for(u=curauto; u; u=u->link)
		if(u->asym == s)
		if(u->type == i) {
			if(u->aoffset > l)
				u->aoffset = l;
			return c;
		}
   ...
}

void
addlib(char *obj)
{
	char name[1024], comp[256], *p;
	int i;

	if(histfrogp <= 0)
		return;

	if(histfrog[0]->name[1] == '/') {
		sprint(name, "");
		i = 1;
	} else
	if(histfrog[0]->name[1] == '.') {
		sprint(name, ".");
		i = 0;
	} else {
			sprint(name, "/usr/%clib", thechar);
		i = 0;
	}

	for(; i<histfrogp; i++) {
		snprint(comp, sizeof comp, histfrog[i]->name+1);
		for(;;) {
			p = strstr(comp, "$O");
			if(p == 0)
				break;
			memmove(p+1, p+2, strlen(p+2)+1);
			p[0] = thechar;
		}
		for(;;) {
			p = strstr(comp, "$M");
			if(p == 0)
				break;
			if(strlen(comp)+strlen(thestring)-2+1 >= sizeof comp) {
				diag("library component too long");
				return;
			}
			memmove(p+strlen(thestring), p+2, strlen(p+2)+1);
			memmove(p, thestring, strlen(thestring));
		}
		if(strlen(name) + strlen(comp) + 3 >= sizeof(name)) {
			diag("library component too long");
			return;
		}
		strcat(name, "/");
		strcat(name, comp);
	}
	for(i=0; i<libraryp; i++)
		if(strcmp(name, library[i]) == 0)
			return;
	if(libraryp == nelem(library)){
		diag("too many autolibs; skipping %s", name);
		return;
	}

	p = malloc(strlen(name) + 1);
	strcpy(p, name);
	library[libraryp] = p;
	p = malloc(strlen(obj) + 1);
	strcpy(p, obj);
	libraryobj[libraryp] = p;
	libraryp++;
}



void
ldobj(int f, long c, char *pn)
{
    ...
   
		if(debug['S'] && r == 0)
			sig = 1729;
		if(sig != 0){
			if(s->sig != 0 && s->sig != sig)
				diag("incompatible type signatures %lux(%s) and %lux(%s) for %s", s->sig, filen[s->file], sig, pn, s->name);
			s->sig = sig;
			s->file = files-1;
		}

	if(thechar == 'i') {
		switch(o) {
		case AMOV:
			if(p->from.type == D_OREG || p->to.type == D_OREG)
				o = AMOVW;
			break;

		case AMOVW:
		case AMOVWU:
			if(p->from.type == D_OREG || p->to.type == D_OREG)
				o = AMOVW;
			else
				o = AMOV;
			break;

		case ADIVW:
		case ADIVUW:
			o = ADIV;
			break;

		case AREMW:
		case AREMUW:
			o = AREM;
			break;

		case AADDW:	o = AADD; break;
		case ASUBW:	o = ASUB; break;
		case ASLLW:	o = ASLL; break;
		case ASRLW:	o = ASRL; break;
		case ASRAW:	o = ASRA; break;
		case AMULW:	o = AMUL; break;
		}
		p->as = o;
	}

	switch(o) {
	case AHISTORY:
		if(p->to.offset == -1) {
			addlib(pn);
			histfrogp = 0;
			goto loop;
		}
		addhist(p->line, D_FILE);		/* 'z' */
		if(p->to.offset)
			addhist(p->to.offset, D_FILE1);	/* 'Z' */
		histfrogp = 0;
		goto loop;

	case AEND:
		histtoauto();
		if(curtext != P)
			curtext->to.autom = curauto;
		curauto = 0;
		curtext = P;
		if(c)
			goto newloop;
		return;

	case AGLOBL:
		s = p->from.sym;
		if(s == S) {
			diag("GLOBL must have a name\n%P", p);
			errorexit();
		}
		if(s->type == 0 || s->type == SXREF) {
			s->type = SBSS;
			s->value = 0;
		}
		if(s->type != SBSS) {
			diag("redefinition: %s\n%P", s->name, p);
			s->type = SBSS;
			s->value = 0;
		}
		if(p->to.offset > s->value)
			s->value = p->to.offset;
		break;

	case ADYNT:
		if(p->to.sym == S) {
			diag("DYNT without a sym\n%P", p);
			break;
		}
		di = p->to.sym;
		p->reg = 4;
		if(di->type == SXREF) {
			if(debug['z'])
				Bprint(&bso, "%P set to %d\n", p, dtype);
			di->type = SCONST;
			di->value = dtype;
			dtype += 4;
		}
		if(p->from.sym == S)
			break;

		p->from.offset = di->value;
		p->from.sym->type = SDATA;
		if(curtext == P) {
			diag("DYNT not in text: %P", p);
			break;
		}
		p->to.sym = curtext->from.sym;
		p->to.type = D_CONST;
		p->link = datap;
		datap = p;
		break;

	case AINIT:
		if(p->from.sym == S) {
			diag("INIT without a sym\n%P", p);
			break;
		}
		if(di == S) {
			diag("INIT without previous DYNT\n%P", p);
			break;
		}
		p->from.offset = di->value;
		p->from.sym->type = SDATA;
		p->link = datap;
		datap = p;
		break;
	
	case ADATA:
		if(p->from.sym == S) {
			diag("DATA without a sym\n%P", p);
			break;
		}
		p->link = datap;
		datap = p;
		break;

	case AGOK:
		diag("unknown opcode\n%P", p);
		p->pc = pc;
		pc++;
		break;

	case ATEXT:
		if(curtext != P) {
			histtoauto();
			curtext->to.autom = curauto;
			curauto = 0;
		}
		skip = 0;
		curtext = p;
		autosize = p->to.offset;
		if(autosize < 0)
			autosize = -ptrsize;
		else if(autosize != 0){
			autosize = (autosize + 3) & ~3;
			if(thechar == 'j'){
				if((autosize & 4) != 0)
					autosize += 4;
			}else{
				if((autosize & 4) == 0)
					autosize += 4;
			}
		}
		p->to.offset = autosize;
		s = p->from.sym;
		if(s == S) {
			diag("TEXT must have a name\n%P", p);
			errorexit();
		}
		if(s->type != 0 && s->type != SXREF) {
			if(p->reg & DUPOK) {
				skip = 1;
				goto casedef;
			}
			diag("redefinition: %s\n%P", s->name, p);
		}
		s->type = STEXT;
		s->value = pc;
		lastp->link = p;
		lastp = p;
		p->pc = pc;
		pc++;
		if(textp == P) {
			textp = p;
			etextp = p;
			goto loop;
		}
		etextp->cond = p;
		etextp = p;
		break;

	case ASUB:
	case ASUBW:
		if(p->from.type == D_CONST)
		if(p->from.name == D_NONE) {
			p->from.offset = -p->from.offset;
			p->as = o == ASUB ? AADD : AADDW;
		}
		goto casedef;

	case AMOVF:
		if(skip)
			goto casedef;

		if(p->from.type == D_FCONST) {
			/* size sb 9 max */
			sprint(literal, "$%lux", ieeedtof(p->from.ieee));
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SBSS;
				s->value = 4;
				t = prg();
				t->as = ADATA;
				t->line = p->line;
				t->from.type = D_OREG;
				t->from.sym = s;
				t->from.name = D_EXTERN;
				t->reg = 4;
				t->to = p->from;
				t->link = datap;
				datap = t;
			}
			p->from.type = D_OREG;
			p->from.sym = s;
			p->from.name = D_EXTERN;
			p->from.offset = 0;
		}
		goto casedef;

	case AMOVD:
		if(skip)
			goto casedef;

		if(p->from.type == D_FCONST) {
			/* size sb 18 max */
			sprint(literal, "$%lux.%lux",
				p->from.ieee->l, p->from.ieee->h);
			s = lookup(literal, 0);
			if(s->type == 0) {
				s->type = SBSS;
				s->value = 8;
				t = prg();
				t->as = ADATA;
				t->line = p->line;
				t->from.type = D_OREG;
				t->from.sym = s;
				t->from.name = D_EXTERN;
				t->reg = 8;
				t->to = p->from;
				t->link = datap;
				datap = t;
			}
			p->from.type = D_OREG;
			p->from.sym = s;
			p->from.name = D_EXTERN;
			p->from.offset = 0;
		}
		goto casedef;

	case ABGT:
	case ABGTU:
	case ABLE:
	case ABLEU:
		if(p->from.type == D_REG){
			p->as = relrev(p->as);
			if(p->reg == NREG)
				p->reg = REGZERO;
			else{
				r = p->from.reg;
				p->from.reg = p->reg;
				p->reg = r;
			}
		}
		goto casedef;

	case ASGT:
	case ASGTU:
		r = p->from.reg;
		p->from.reg = p->reg;
		p->reg = r;
		p->as = p->as == ASGT ? ASLT : ASLTU;
		goto casedef;

	case AMOV:
		if(p->from.type == D_CONST &&
		   p->from.name == D_NONE &&
		   p->from.reg != NREG &&
		   p->from.reg != REGZERO){
			p->as = AADD;
			p->reg = p->from.reg;
			p->from.reg = NREG;
		}
		goto casedef;

/*
	case AMUL:
	case AMULXSS:
	case AMULXSU:
	case AMULXUU:
		if(!debug['m'])
			goto casedef;
		if(o != AMUL || p->from.type != D_REG){
			diag("unsupported multiply operation");
			if(!debug['v'])
				prasm(p);
			break;
		}
		print("transforming multiply");
		prasm(p);
		if(p->reg == NREG)
			p->reg = p->to.reg;
		p->as = ACUSTOM;
		p->from.type = D_CONST;
		p->from.name = D_NONE;
		p->from.offset = 0;
		p->to.type = D_CONST;
		p->to.name = D_NONE;
		p->to.offset = p->from.reg<<16 | p->reg<<8 | p->to.reg;
		p->from.reg = NREG;
		p->to.reg = NREG;
		prasm(p);
		goto casedef;
*/

	default:
	casedef:
		if(skip)
			nopout(p);

		if(p->to.type == D_BRANCH)
			p->to.offset += ipc;
		lastp->link = p;
		lastp = p;
		p->pc = pc;
		pc++;
		break;
	}
	goto loop;
   ...
}


void
doprof2(void)
{
	Sym *s2, *s4;
	Prog *p, *q, *q2, *ps2, *ps4;

	if(debug['e']){
		s2 = lookup("_tracein", 0);
		s4 = lookup("_traceout", 0);
	}else{
		s2 = lookup("_profin", 0);
		s4 = lookup("_profout", 0);
	}
	if(s2->type != STEXT || s4->type != STEXT) {
		if(debug['e'])
			diag("_tracein/_traceout not defined %d %d", s2->type, s4->type);
		else
			diag("_profin/_profout not defined");
		return;
	}

	ps2 = P;
	ps4 = P;
	for(p = firstp; p != P; p = p->link) {
		if(p->as == ATEXT) {
			if(p->from.sym == s2) {
				ps2 = p;
				p->reg = 1;
			}
			if(p->from.sym == s4) {
				ps4 = p;
				p->reg = 1;
			}
		}
	}
	for(p = firstp; p != P; p = p->link) {
		if(p->as == ATEXT) {
			if(p->reg & NOPROF) {
				for(;;) {
					q = p->link;
					if(q == P)
						break;
					if(q->as == ATEXT)
						break;
					p = q;
				}
				continue;
			}

			/*
			 * CALL	profin, R2
			 */
			q = prg();
			q->line = p->line;
			q->pc = p->pc;
			q->link = p->link;
			if(debug['e']){		/* embedded tracing */
				q2 = prg();
				p->link = q2;
				q2->link = q;

				q2->line = p->line;
				q2->pc = p->pc;

				q2->as = AJMP;
				q2->to.type = D_BRANCH;
				q2->to.sym = p->to.sym;
				q2->cond = q->link;
			}else
				p->link = q;
			p = q;
			p->as = AJAL;
			p->to.type = D_BRANCH;
			p->cond = ps2;
			p->to.sym = s2;

			continue;
		}
		if(p->as == ARET) {
			/*
			 * RET (default)
			 */
			if(debug['e']){		/* embedded tracing */
				q = prg();
				q->line = p->line;
				q->pc = p->pc;
				q->link = p->link;
				p->link = q;
				p = q;
			}
			/*
			 * RET
			 */
			q = prg();
			q->as = ARET;
			q->from = p->from;
			q->to = p->to;
			q->link = p->link;
			p->link = q;

			/*
			 * CALL	profout
			 */
			p->as = AJAL;
			p->from = zprg.from;
			p->to = zprg.to;
			p->to.type = D_BRANCH;
			p->cond = ps4;
			p->to.sym = s4;

			p = q;

			continue;
		}
	}
}

void
nuxiinit(void)
{
	for(i=0; i<4; i++){
		c = find1(0x04030201L, i+1);
		if(i < 2)
			inuxi2[i] = c;
		if(i < 1)
			inuxi1[i] = c;
		inuxi4[i] = c;
		inuxi8[i] = c;
		inuxi8[i+4] = c+4;
		fnuxi4[i] = c;
		fnuxi8[i] = c;
		fnuxi8[i+4] = c+4;
	}
}


long
ieeedtof(Ieee *ieeep)
{
	int exp;
	long v;

	if(ieeep->h == 0)
		return 0;
	exp = (ieeep->h>>20) & ((1L<<11)-1L);
	exp -= (1L<<10) - 2L;
	v = (ieeep->h & 0xfffffL) << 3;
	v |= (ieeep->l >> 29) & 0x7L;
	if((ieeep->l >> 28) & 1) {
		v++;
		if(v & 0x800000L) {
			v = (v & 0x7fffffL) >> 1;
			exp++;
		}
	}
	if(exp <= -126 || exp >= 130)
		diag("double fp to single fp overflow");
	v |= ((exp + 126) & 0xffL) << 23;
	v |= ieeep->h & 0x80000000L;
	return v;
}

double
ieeedtod(Ieee *ieeep)
{
	Ieee e;
	double fr;
	int exp;

	if(ieeep->h & (1L<<31)) {
		e.h = ieeep->h & ~(1L<<31);
		e.l = ieeep->l;
		return -ieeedtod(&e);
	}
	if(ieeep->l == 0 && ieeep->h == 0)
		return 0;
	fr = ieeep->l & ((1L<<16)-1L);
	fr /= 1L<<16;
	fr += (ieeep->l>>16) & ((1L<<16)-1L);
	fr /= 1L<<16;
	fr += (ieeep->h & (1L<<20)-1L) | (1L<<20);
	fr /= 1L<<21;
	exp = (ieeep->h>>20) & ((1L<<11)-1L);
	exp -= (1L<<10) - 2L;
	return ldexp(fr, exp);
}
