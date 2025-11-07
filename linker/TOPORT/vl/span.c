
void
pagebug(Prog *p)
{
	Prog *q;

	switch(p->as) {
	case ABGEZAL:
	case ABLTZAL:
	case AJAL:
	case ABEQ:
	case ABGEZ:
	case ABGTZ:
	case ABLEZ:
	case ABLTZ:
	case ABNE:
	case ABFPT:
	case ABFPF:
	case AJMP:
		q = prg();
		*q = *p;
		p->link = q;
		p->as = ANOR;
		p->optab = 0;
		p->from = zprg.from;
		p->from.type = D_REG;
		p->from.reg = REGZERO;
		p->to = p->from;
	}
}

void
span(void)
{
	Prog *p, *q;
	Sym *setext, *s;
	Optab *o;
	int m, bflag, i;
	long c, otxt, v;

	bflag = 0;
	c = INITTEXT;
	otxt = c;
	for(p = firstp; p != P; p = p->link) {

		/* bug in early 4000 chips delayslot on page boundary */
		if((c&(0x1000-1)) == 0xffc)
			pagebug(p);

		p->pc = c;
		o = oplook(p);
		m = o->size;

		if(m == 0) {

			if(p->as == ATEXT) {
				curtext = p;
				autosize = p->to.offset + 4;
				if(p->from.sym != S)
					p->from.sym->value = c;
				/* need passes to resolve branches */
				if(c-otxt >= 1L<<17)
					bflag = 1;
				otxt = c;
				continue;
			}
			diag("zero-width instruction\n%P", p);
			continue;
		}
		c += m;
	}

	/*
	 * if any procedure is large enough to
	 * generate a large SBRA branch, then
	 * generate extra passes putting branches
	 * around jmps to fix. this is rare.
	 */
	while(bflag) {
		if(debug['v'])
			Bprint(&bso, "%5.2f span1\n", cputime());
		bflag = 0;
		c = INITTEXT;
		for(p = firstp; p != P; p = p->link) {
			/* bug in early 4000 chips delayslot on page boundary */
			if((c&(0x1000-1)) == 0xffc)
				pagebug(p);

			p->pc = c;
			o = oplook(p);

			if(o->type == 6 && p->cond) {
				otxt = p->cond->pc - c;
				if(otxt < 0)
					otxt = -otxt;
				if(otxt >= (1L<<17) - 10) {
					q = prg();
					q->link = p->link;
					p->link = q;
					q->as = AJMP;
					q->to.type = D_BRANCH;
					q->cond = p->cond;
					p->cond = q;
					q = prg();
					q->link = p->link;
					p->link = q;
					q->as = AJMP;
					q->to.type = D_BRANCH;
					q->cond = q->link->link;
					addnop(p->link);
					addnop(p);
					bflag = 1;
				}
			}
			m = o->size;
			if(m == 0) {
				if(p->as == ATEXT) {
					curtext = p;
					autosize = p->to.offset + 4;
					if(p->from.sym != S)
						p->from.sym->value = c;
					continue;
				}
				diag("zero-width instruction\n%P", p);
				continue;
			}
			c += m;
		}
	}

	if(debug['t']) {
		/* 
		 * add strings to text segment
		 */
		c = rnd(c, 8);
		for(i=0; i<NHASH; i++)
		for(s = hash[i]; s != S; s = s->link) {
			if(s->type != SSTRING)
				continue;
			v = s->value;
			while(v & 3)
				v++;
			s->value = c;
			c += v;
		}
	}

	c = rnd(c, 8);

	setext = lookup("etext", 0);
	if(setext != S) {
		setext->value = c;
		textsize = c - INITTEXT;
	}
	if(INITRND)
		INITDAT = rnd(c, INITRND);
	if(debug['v'])
		Bprint(&bso, "tsize = %lux\n", textsize);
	Bflush(&bso);
}
		
long
regoff(Adr *a)
{

	instoffset = 0;
	aclass(a);
	return instoffset;
}

int
aclass(Adr *a)
{
	Sym *s;
	int t;

	switch(a->type) {
	case D_NONE:
		return C_NONE;

	case D_REG:
		return C_REG;

	case D_FREG:
		return C_FREG;

	case D_FCREG:
		return C_FCREG;

	case D_MREG:
		return C_MREG;

	case D_OREG:
		switch(a->name) {
		case D_EXTERN:
		case D_STATIC:
			if(a->sym == 0 || a->sym->name == 0) {
				print("null sym external\n");
				print("%D\n", a);
				return C_GOK;
			}
			t = a->sym->type;
			if(t == 0 || t == SXREF) {
				diag("undefined external: %s in %s",
					a->sym->name, TNAME);
				a->sym->type = SDATA;
			}
			instoffset = a->sym->value + a->offset - BIG;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SEXT;
			return C_LEXT;
		case D_AUTO:
			instoffset = autosize + a->offset;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SAUTO;
			return C_LAUTO;

		case D_PARAM:
			instoffset = autosize + a->offset + 4L;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SAUTO;
			return C_LAUTO;
		case D_NONE:
			instoffset = a->offset;
			if(instoffset == 0)
				return C_ZOREG;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SOREG;
			return C_LOREG;
		}
		return C_GOK;

	case D_HI:
		return C_LO;
	case D_LO:
		return C_HI;

	case D_OCONST:
		switch(a->name) {
		case D_EXTERN:
		case D_STATIC:
			s = a->sym;
			t = s->type;
			if(t == 0 || t == SXREF) {
				diag("undefined external: %s in %s",
					s->name, TNAME);
				s->type = SDATA;
			}
			instoffset = s->value + a->offset + INITDAT;
			if(s->type == STEXT || s->type == SLEAF)
				instoffset = s->value + a->offset;
			return C_LCON;
		}
		return C_GOK;

	case D_CONST:
		switch(a->name) {

		case D_NONE:
			instoffset = a->offset;
		consize:
			if(instoffset > 0) {
				if(instoffset <= 0x7fff)
					return C_SCON;
				if(instoffset <= 0xffff)
					return C_ANDCON;
				if((instoffset & 0xffff) == 0)
					return C_UCON;
				return C_LCON;
			}
			if(instoffset == 0)
				return C_ZCON;
			if(instoffset >= -0x8000)
				return C_ADDCON;
			if((instoffset & 0xffff) == 0)
				return C_UCON;
			return C_LCON;

		case D_EXTERN:
		case D_STATIC:
			s = a->sym;
			if(s == S)
				break;
			t = s->type;
			switch(t) {
			case 0:
			case SXREF:
				diag("undefined external: %s in %s",
					s->name, TNAME);
				s->type = SDATA;
				break;
			case SCONST:
				instoffset = s->value + a->offset;
				goto consize;
			case STEXT:
			case SLEAF:
			case SSTRING:
				instoffset = s->value + a->offset;
				return C_LCON;
			}
			instoffset = s->value + a->offset - BIG;
			if(instoffset >= -BIG && instoffset < BIG && instoffset != 0L)
				return C_SECON;
			instoffset = s->value + a->offset + INITDAT;
			return C_LCON;

		case D_AUTO:
			instoffset = autosize + a->offset;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SACON;
			return C_LACON;

		case D_PARAM:
			instoffset = autosize + a->offset + 4L;
			if(instoffset >= -BIG && instoffset < BIG)
				return C_SACON;
			return C_LACON;
		}
		return C_GOK;

	case D_BRANCH:
		return C_SBRA;
	}
	return C_GOK;
}

int
cmp(int a, int b)
{

	if(a == b)
		return 1;
	switch(a) {
	case C_LCON:
		if(b == C_ZCON || b == C_SCON || b == C_UCON ||
		   b == C_ADDCON || b == C_ANDCON)
			return 1;
		break;
	case C_ADD0CON:
		if(b == C_ADDCON)
			return 1;
	case C_ADDCON:
		if(b == C_ZCON || b == C_SCON)
			return 1;
		break;
	case C_AND0CON:
		if(b == C_ANDCON)
			return 1;
	case C_ANDCON:
		if(b == C_ZCON || b == C_SCON)
			return 1;
		break;
	case C_UCON:
		if(b == C_ZCON)
			return 1;
		break;
	case C_SCON:
		if(b == C_ZCON)
			return 1;
		break;
	case C_LACON:
		if(b == C_SACON)
			return 1;
		break;
	case C_LBRA:
		if(b == C_SBRA)
			return 1;
		break;
	case C_LEXT:
		if(b == C_SEXT)
			return 1;
		break;
	case C_LAUTO:
		if(b == C_SAUTO)
			return 1;
		break;
	case C_REG:
		if(b == C_ZCON)
			return 1;
		break;
	case C_LOREG:
		if(b == C_ZOREG || b == C_SOREG)
			return 1;
		break;
	case C_SOREG:
		if(b == C_ZOREG)
			return 1;
		break;
	}
	return 0;
}


void
buildop(void)
{
		switch(r)
		{
        ...
		case AABSF:
			oprange[AMOVFD] = oprange[r];
			oprange[AMOVDF] = oprange[r];
			oprange[AMOVWF] = oprange[r];
			oprange[AMOVFW] = oprange[r];
			oprange[AMOVWD] = oprange[r];
			oprange[AMOVDW] = oprange[r];
			oprange[ANEGF] = oprange[r];
			oprange[ANEGD] = oprange[r];
			oprange[AABSD] = oprange[r];
			break;
		case AADD:
			buildrep(1, AADD);
			oprange[ASGT] = oprange[r];
			repop[ASGT] = 1;
			oprange[ASGTU] = oprange[r];
			repop[ASGTU] = 1;
			oprange[AADDU] = oprange[r];
			repop[AADDU] = 1;
			oprange[AADDVU] = oprange[r];
			repop[AADDVU] = 1;
			break;
		case AADDF:
			oprange[ADIVF] = oprange[r];
			oprange[ADIVD] = oprange[r];
			oprange[AMULF] = oprange[r];
			oprange[AMULD] = oprange[r];
			oprange[ASUBF] = oprange[r];
			oprange[ASUBD] = oprange[r];
			oprange[AADDD] = oprange[r];
			break;
		case AAND:
			buildrep(2, AAND);
			oprange[AXOR] = oprange[r];
			repop[AXOR] = 2;
			oprange[AOR] = oprange[r];
			repop[AOR] = 2;
			break;
		case ABEQ:
			oprange[ABNE] = oprange[r];
			break;
		case ABLEZ:
			oprange[ABGEZ] = oprange[r];
			oprange[ABGEZAL] = oprange[r];
			oprange[ABLTZ] = oprange[r];
			oprange[ABLTZAL] = oprange[r];
			oprange[ABGTZ] = oprange[r];
			break;
		case AMOVB:
			buildrep(3, AMOVB);
			oprange[AMOVH] = oprange[r];
			repop[AMOVH] = 3;
			break;
		case AMOVBU:
			buildrep(4, AMOVBU);
			oprange[AMOVHU] = oprange[r];
			repop[AMOVHU] = 4;
			break;
		case AMUL:
			oprange[AREM] = oprange[r];
			oprange[AREMU] = oprange[r];
			oprange[ADIVU] = oprange[r];
			oprange[AMULU] = oprange[r];
			oprange[ADIV] = oprange[r];
			oprange[ADIVVU] = oprange[r];
			oprange[ADIVV] = oprange[r];
			break;
		case ASLL:
			oprange[ASRL] = oprange[r];
			oprange[ASRA] = oprange[r];
			oprange[ASLLV] = oprange[r];
			oprange[ASRAV] = oprange[r];
			oprange[ASRLV] = oprange[r];
			break;
		case ASUB:
			oprange[ASUBU] = oprange[r];
			oprange[ANOR] = oprange[r];
			break;
		case ASYSCALL:
			oprange[ATLBP] = oprange[r];
			oprange[ATLBR] = oprange[r];
			oprange[ATLBWI] = oprange[r];
			oprange[ATLBWR] = oprange[r];
			break;
		case ACMPEQF:
			oprange[ACMPGTF] = oprange[r];
			oprange[ACMPGTD] = oprange[r];
			oprange[ACMPGEF] = oprange[r];
			oprange[ACMPGED] = oprange[r];
			oprange[ACMPEQD] = oprange[r];
			break;
		case ABFPT:
			oprange[ABFPF] = oprange[r];
			break;
		case AMOVWL:
			oprange[AMOVWR] = oprange[r];
			oprange[AMOVVR] = oprange[r];
			oprange[AMOVVL] = oprange[r];
			break;
		case AMOVW:
			buildrep(5, AMOVW);
			break;
		case AMOVD:
			buildrep(6, AMOVD);
			break;
		case AMOVF:
			buildrep(7, AMOVF);
			break;
		case AMOVV:
			buildrep(8, AMOVV);
			break;
		case ABREAK:
		case AWORD:
		case ARFE:
		case AJAL:
		case AJMP:
		case ATEXT:
		case ACASE:
		case ABCASE:
			break;
		}
	}
}
