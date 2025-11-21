int little;

void
main(int argc, char *argv[])
{
	ARGBEGIN {
	default:
	case  'm':			/* for little-endian mips */
		thechar = '0';
		thestring = "spim";
		little = 1;
		break;
	case 'P':
		a = ARGF();
		if(a)
			INITTEXTP = atolwhex(a);
		break;
	case 'L':
		addlibpath(EARGF(usage()));
		break;
	} ARGEND

	a = "";
	snprint(name, sizeof(name), "%s/%s/lib", a, thestring);
	addlibpath(name);


	switch(HEADTYPE) {
	case 2:	/* plan 9 */
		HEADR = 32L;
		if(INITDAT == -1)
			INITDAT = 0;
		if(INITRND == -1)
			INITRND = 16*1024;
		if(INITTEXT == -1)
			INITTEXT = INITRND + HEADR;
		break;
	}

	dtype = 4;
    ...
}

void
objfile(char *file)
{
	if(file[0] == '-' && file[1] == 'l') {
		sprint(name, "/usr/%clib/lib", thechar);
		strcat(name, file+2);
		strcat(name, ".a");
		file = name;
	}
    ...
}

int
zaddr(uchar *p, Adr *a, Sym *h[])
{
	int i, c;
	int l;
	Sym *s;
	Auto *u;
	switch(a->type) {

	case D_FCONST:
        ...
		a->ieee = (Ieee*)hunk;
        ...
		a->ieee->l = p[4] | (p[5]<<8) |
			(p[6]<<16) | (p[7]<<24);
		a->ieee->h = p[8] | (p[9]<<8) |
			(p[10]<<16) | (p[11]<<24);
		c += 8;
		break;
	}
...
}


void
ldobj(int f, long c, char *pn)
{
    ...
	p->as = o;
	p->reg = bloc[1] & 0x7f;
	if(bloc[1] & 0x80)
		p->mark = NOSCHED;
    ...

	switch(o) {
    ...
	case ASUB:
	case ASUBU:
		if(p->from.type == D_CONST)
		if(p->from.name == D_NONE) {
			p->from.offset = -p->from.offset;
			if(p->as == ASUB)
				p->as = AADD;
			else
				p->as = AADDU;
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
			 * JAL	profin, R2
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
			 * JAL	profout
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
