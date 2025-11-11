/* can't include a.out.h due to name clashes, but these are taken from it */
#define	_MAGIC(f, b)	((f)|((((4*(b))+0)*(b))+7))
#define	V_MAGIC		_MAGIC(0, 16)		/* mips 3000 BE */
#define	P_MAGIC		_MAGIC(0, 24)		/* mips 3000 LE */

void
asmb(void)
{
    ...
	switch(HEADTYPE) {
	case 2:
		if (little)
			lput(P_MAGIC);		/* mips 3000 LE */
		else
			lput(V_MAGIC);		/* mips 3000 BE */
	}
	cflush();
}


void
nopstat(char *f, Count *c)
{
	if(c->outof)
	Bprint(&bso, "%s delay %ld/%ld (%.2f)\n", f,
		c->outof - c->count, c->outof,
		(double)(c->outof - c->count)/c->outof);
}

void
asmsym(void)
{
			switch(s->type) {
            ...
			case SSTRING:
				putsymb(s->name, 'T', s->value, s->version);
				continue;

			case SBSS:
				putsymb(s->name, 'B', s->value+INITDAT, s->version);
				continue;

			}
}


#define	MINLC	4
void
asmlc(void)
{
	long oldpc, oldlc;
	Prog *p;
	long v, s;

	oldpc = INITTEXT;
	oldlc = 0;
	for(p = firstp; p != P; p = p->link) {
		if(p->line == oldlc || p->as == ATEXT || p->as == ANOP) {
			if(p->as == ATEXT)
				curtext = p;
			if(debug['V'])
				Bprint(&bso, "%6lux %P\n",
					p->pc, p);
			continue;
		}
		if(debug['V'])
			Bprint(&bso, "\t\t%6ld", lcsize);
		v = (p->pc - oldpc) / MINLC;
		while(v) {
			s = 127;
			if(v < 127)
				s = v;
			CPUT(s+128);	/* 129-255 +pc */
			if(debug['V'])
				Bprint(&bso, " pc+%ld*%d(%ld)", s, MINLC, s+128);
			v -= s;
			lcsize++;
		}
		s = p->line - oldlc;
		oldlc = p->line;
		oldpc = p->pc + MINLC;
		if(s > 64 || s < -64) {
			CPUT(0);	/* 0 vv +lc */
			CPUT(s>>24);
			CPUT(s>>16);
			CPUT(s>>8);
			CPUT(s);
			if(debug['V']) {
				if(s > 0)
					Bprint(&bso, " lc+%ld(%d,%ld)\n",
						s, 0, s);
				else
					Bprint(&bso, " lc%ld(%d,%ld)\n",
						s, 0, s);
				Bprint(&bso, "%6lux %P\n",
					p->pc, p);
			}
			lcsize += 5;
			continue;
		}
		if(s > 0) {
			CPUT(0+s);	/* 1-64 +lc */
			if(debug['V']) {
				Bprint(&bso, " lc+%ld(%ld)\n", s, 0+s);
				Bprint(&bso, "%6lux %P\n",
					p->pc, p);
			}
		} else {
			CPUT(64-s);	/* 65-128 -lc */
			if(debug['V']) {
				Bprint(&bso, " lc%ld(%ld)\n", s, 64-s);
				Bprint(&bso, "%6lux %P\n",
					p->pc, p);
			}
		}
		lcsize++;
	}
	while(lcsize & 1) {
		s = 129;
		CPUT(s);
		lcsize++;
	}
	if(debug['v'] || debug['V'])
		Bprint(&bso, "lcsize = %ld\n", lcsize);
	Bflush(&bso);
}

void
datblk(long s, long n, int str)
{
	Prog *p;
	char *cast;
	long l, fl, j, d;
	int i, c;

	memset(buf.dbuf, 0, n+100);
	for(p = datap; p != P; p = p->link) {
		curp = p;
		if(str != (p->from.sym->type == SSTRING))
			continue;
		l = p->from.sym->value + p->from.offset - s;
		c = p->reg;
		i = 0;
		if(l < 0) {
			if(l+c <= 0)
				continue;
			while(l < 0) {
				l++;
				i++;
			}
		}
		if(l >= n)
			continue;
		if(p->as != AINIT && p->as != ADYNT) {
			for(j=l+(c-i)-1; j>=l; j--)
				if(buf.dbuf[j]) {
					print("%P\n", p);
					diag("multiple initialization");
					break;
				}
		}
		switch(p->to.type) {
		default:
			diag("unknown mode in initialization\n%P", p);
			break;

		case D_FCONST:
			switch(c) {
			default:
			case 4:
				fl = ieeedtof(p->to.ieee);
				cast = (char*)&fl;
				for(; i<c; i++) {
					buf.dbuf[l] = cast[fnuxi8[i+4]];
					l++;
				}
				break;
			case 8:
				cast = (char*)p->to.ieee;
				for(; i<c; i++) {
					buf.dbuf[l] = cast[fnuxi8[i]];
					l++;
				}
				break;
			}
			break;

		case D_SCONST:
			for(; i<c; i++) {
				buf.dbuf[l] = p->to.sval[i];
				l++;
			}
			break;

		case D_CONST:
			d = p->to.offset;
			if(p->to.sym) {
				switch(p->to.sym->type) {
				case STEXT:
				case SLEAF:
				case SSTRING:
					d += p->to.sym->value;
					break;
				case SDATA:
				case SBSS:
					d += p->to.sym->value + INITDAT;
					break;
				}
			}
			cast = (char*)&d;
			switch(c) {
			default:
				diag("bad nuxi %d %d\n%P", c, i, curp);
				break;
			case 1:
				for(; i<c; i++) {
					buf.dbuf[l] = cast[inuxi1[i]];
					l++;
				}
				break;
			case 2:
				for(; i<c; i++) {
					buf.dbuf[l] = cast[inuxi2[i]];
					l++;
				}
				break;
			case 4:
				for(; i<c; i++) {
					buf.dbuf[l] = cast[inuxi4[i]];
					l++;
				}
				break;
			}
			break;
		}
	}
	write(cout, buf.dbuf, n);
}



#define	OP_SRR(op,s,r2,r3)\
	(op|(((s)&31L)<<6)|(((r2)&31L)<<16)|(((r3)&31L)<<11))
#define	OP_FRRR(op,r1,r2,r3)\
	(op|(((r1)&31L)<<16)|(((r2)&31L)<<11)|(((r3)&31L)<<6))
#define	OP_JMP(op,i)\
		((op)|((i)&0x3ffffffL))


#define	BCOND(x,y)\
	(((x)<<19)|((y)<<16))

#define	MMU(x,y)\
	(SP(2,0)|(16<<21)|((x)<<3)|((y)<<0))
#define	FPF(x,y)\
	(SP(2,1)|(16<<21)|((x)<<3)|((y)<<0))
#define	FPD(x,y)\
	(SP(2,1)|(17<<21)|((x)<<3)|((y)<<0))
#define	FPW(x,y)\
	(SP(2,1)|(20<<21)|((x)<<3)|((y)<<0))

int
asmout(Prog *p, Optab *o)
{
	long v;
	Prog *ct;
	int r, a;

	switch(o->type) {
    ...
	case 1:		/* mov[v] r1,r2 ==> OR r1,r0,r2 */
		o1 = OP_RRR(oprrr(AOR), p->from.reg, REGZERO, p->to.reg);
		break;

	case 2:		/* add/sub r1,[r2],r3 */
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;
		o1 = OP_RRR(oprrr(p->as), p->from.reg, r, p->to.reg);
		break;

	case 3:		/* mov $soreg, r ==> or/add $i,o,r */
		///v = regoff(&p->from);
		///r = p->from.reg;
		if(r == NREG)
			r = o->param;
		a = AADDU;
		///if(o->a1 == C_ANDCON)
		///	a = AOR;
		///o1 = OP_IRR(opirr(a), v, r, p->to.reg);
		break;

	//case 4:		/* add $scon,[r1],r2 */
		//v = regoff(&p->from);
		//r = p->reg;
		//if(r == NREG)
		//	r = p->to.reg;
		//o1 = OP_IRR(opirr(p->as), v, r, p->to.reg);
		//break;

	case 5:		/* syscall */
		o1 = oprrr(p->as);
		break;

	case 6:		/* beq r1,[r2],sbra */
		if(p->cond == P)
			v = -4 >> 2;
		else
			v = (p->cond->pc - pc-4) >> 2;
		if(((v << 16) >> 16) != v)
			diag("short branch too far: %ld\n%P", v, p);
		o1 = OP_IRR(opirr(p->as), v, p->from.reg, p->reg);
		break;

	case 7:		/* mov r, soreg ==> sw o(r) */
		//r = p->to.reg;
		if(r == NREG)
			r = o->param;
		//v = regoff(&p->to);
		//o1 = OP_IRR(opirr(p->as), v, r, p->from.reg);
		//break;

	case 8:		/* mov soreg, r ==> lw o(r) */
		//r = p->from.reg;
		if(r == NREG)
			r = o->param;
		//v = regoff(&p->from);
		//o1 = OP_IRR(opirr(p->as+ALAST), v, r, p->to.reg);
		//break;

	case 9:		/* asl r1,[r2],r3 */
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;
		o1 = OP_RRR(oprrr(p->as), r, p->from.reg, p->to.reg);
		break;

	case 10:	/* add $con,[r1],r2 ==> mov $con,t; add t,[r1],r2 */
		v = regoff(&p->from);
		r = AOR;
		if(v < 0)
			r = AADDU;
		o1 = OP_IRR(opirr(r), v, 0, REGTMP);
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;
		o2 = OP_RRR(oprrr(p->as), REGTMP, r, p->to.reg);
		break;

	case 11:	/* jmp lbra */
		if(p->cond == P)
			v = p->pc >> 2;
		else
			//v = p->cond->pc >> 2;
		o1 = OP_JMP(opirr(p->as), v);


		if(!debug['Y'] && p->link && p->cond && isnop(p->link)) {
			nop.branch.count--;
			nop.branch.outof--;
			nop.jump.outof++;
			o2 = asmout(p->cond, oplook(p->cond), 1);
			if(o2) {
				o1 += 1;
				if(debug['a'])
					Bprint(&bso, " %.8lux: %.8lux %.8lux%P\n",
						p->pc, o1, o2, p);
				LPUT(o1);
				LPUT(o2);
				return 1;
			}
		}
		break;

	case 12:	/* movbs r,r */
		v = 16;
		if(p->as == AMOVB)
			v = 24;
		o1 = OP_SRR(opirr(ASLL), v, p->from.reg, p->to.reg);
		o2 = OP_SRR(opirr(ASRA), v, p->to.reg, p->to.reg);
		break;

	case 13:	/* movbu r,r */
		if(p->as == AMOVBU)
			o1 = OP_IRR(opirr(AAND), 0xffL, p->from.reg, p->to.reg);
		else
			o1 = OP_IRR(opirr(AAND), 0xffffL, p->from.reg, p->to.reg);
		break;

	case 16:	/* sll $c,[r1],r2 */
		v = regoff(&p->from);
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;

		/* OP_SRR will use only the low 5 bits of the shift value */
		if(v >= 32 && vshift(p->as))
			o1 = OP_SRR(opirr(p->as+ALAST), v-32, r, p->to.reg);
		else 
			o1 = OP_SRR(opirr(p->as), v, r, p->to.reg);
		break;

	case 18:	/* jmp [r1],0(r2) */
		r = p->reg;
		if(r == NREG)
			r = o->param;
		//o1 = OP_RRR(oprrr(p->as), 0, p->to.reg, r);
		break;

	//case 19:	/* mov $lcon,r ==> lu+or */
	//	v = regoff(&p->from);
	//	o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, p->to.reg);
	//	o2 = OP_IRR(opirr(AOR), v, p->to.reg, p->to.reg);
	//	break;

	case 20:	/* mov lohi,r */
		r = OP(2,0);		/* mfhi */
		if(p->from.type == D_LO)
			r = OP(2,2);	/* mflo */
		o1 = OP_RRR(r, REGZERO, REGZERO, p->to.reg);
		break;

	case 21:	/* mov r,lohi */
		r = OP(2,1);		/* mthi */
		if(p->to.type == D_LO)
			r = OP(2,3);	/* mtlo */
		o1 = OP_RRR(r, REGZERO, p->from.reg, REGZERO);
		break;

	case 22:	/* mul r1,r2 */
		o1 = OP_RRR(oprrr(p->as), p->from.reg, p->reg, REGZERO);
		break;

	case 23:	/* add $lcon,r1,r2 ==> lu+or+add */
		v = regoff(&p->from);
		if(p->to.reg == REGTMP || p->reg == REGTMP)
			diag("cant synthesize large constant\n%P", p);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;
		o3 = OP_RRR(oprrr(p->as), REGTMP, r, p->to.reg);
		break;

	case 24:	/* mov $ucon,,r ==> lu r */
		v = regoff(&p->from);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, p->to.reg);
		break;

	case 25:	/* add/and $ucon,[r1],r2 ==> lu $con,t; add t,[r1],r2 */
		v = regoff(&p->from);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		r = p->reg;
		if(r == NREG)
			r = p->to.reg;
		o2 = OP_RRR(oprrr(p->as), REGTMP, r, p->to.reg);
		break;

	case 26:	/* mov $lsext/auto/oreg,,r2 ==> lu+or+add */
		v = regoff(&p->from);
		if(p->to.reg == REGTMP)
			diag("cant synthesize large constant\n%P", p);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
		r = p->from.reg;
		if(r == NREG)
			r = o->param;
		o3 = OP_RRR(oprrr(AADDU), REGTMP, r, p->to.reg);
		break;

	case 27:		/* mov [sl]ext/auto/oreg,fr ==> lwc1 o(r) */
		r = p->from.reg;
		if(r == NREG)
			r = o->param;
		v = regoff(&p->from);
		switch(o->size) {
		case 20:
			o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
			o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
			o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
			o4 = OP_IRR(opirr(AMOVF+ALAST), 0, REGTMP, p->to.reg+1);
			o5 = OP_IRR(opirr(AMOVF+ALAST), 4, REGTMP, p->to.reg);
			break;
		case 16:
			o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
			o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
			o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
			o4 = OP_IRR(opirr(AMOVF+ALAST), 0, REGTMP, p->to.reg);
			break;
		case 8:
			o1 = OP_IRR(opirr(AMOVF+ALAST), v, r, p->to.reg+1);
			o2 = OP_IRR(opirr(AMOVF+ALAST), v+4, r, p->to.reg);
			break;
		case 4:
			o1 = OP_IRR(opirr(AMOVF+ALAST), v, r, p->to.reg);
			break;
		}
		break;

	case 28:		/* mov fr,[sl]ext/auto/oreg ==> swc1 o(r) */
		r = p->to.reg;
		if(r == NREG)
			r = o->param;
		v = regoff(&p->to);
		switch(o->size) {
		case 20:
			if(r == REGTMP)
				diag("cant synthesize large constant\n%P", p);
			o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
			o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
			o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
			o4 = OP_IRR(opirr(AMOVF), 0, REGTMP, p->from.reg+1);
			o5 = OP_IRR(opirr(AMOVF), 4, REGTMP, p->from.reg);
			break;
		case 16:
			if(r == REGTMP)
				diag("cant synthesize large constant\n%P", p);
			o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
			o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
			o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
			o4 = OP_IRR(opirr(AMOVF), 0, REGTMP, p->from.reg);
			break;
		case 8:
			o1 = OP_IRR(opirr(AMOVF), v, r, p->from.reg+1);
			o2 = OP_IRR(opirr(AMOVF), v+4, r, p->from.reg);
			break;
		case 4:
			o1 = OP_IRR(opirr(AMOVF), v, r, p->from.reg);
			break;
		}
		break;

	case 30:	/* movw r,fr */
		r = SP(2,1)|(4<<21);		/* mtc1 */
		o1 = OP_RRR(r, p->from.reg, 0, p->to.reg);
		break;

	case 31:	/* movw fr,r */
		r = SP(2,1)|(0<<21);		/* mfc1 */
		o1 = OP_RRR(r, p->to.reg, 0, p->from.reg);
		break;

	case 32:	/* fadd fr1,[fr2],fr3 */
		r = p->reg;
		if(r == NREG)
			o1 = OP_FRRR(oprrr(p->as), p->from.reg, p->to.reg, p->to.reg);
		else
			o1 = OP_FRRR(oprrr(p->as), p->from.reg, r, p->to.reg);
		break;

	case 33:	/* fabs fr1,fr3 */
		o1 = OP_FRRR(oprrr(p->as), 0, p->from.reg, p->to.reg);
		break;

	case 34:	/* mov $con,fr ==> or/add $i,r,r2 */
		v = regoff(&p->from);
		r = AADDU;
		if(o->a1 == C_ANDCON)
			r = AOR;
		o1 = OP_IRR(opirr(r), v, 0, REGTMP);
		o2 = OP_RRR(SP(2,1)|(4<<21), REGTMP, 0, p->to.reg);	/* mtc1 */
		break;

	case 35:	/* mov r,lext/luto/oreg ==> sw o(r) */
		/*
		 * the lowbits of the constant cannot
		 * be moved into the offset of the load
		 * because the mips 4000 in 64-bit mode
		 * does a 64-bit add and it will screw up.
		 */
		v = regoff(&p->to);
		r = p->to.reg;
		if(r == NREG)
			r = o->param;
		if(r == REGTMP)
			diag("cant synthesize large constant\n%P", p);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
		o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
		o4 = OP_IRR(opirr(p->as), 0, REGTMP, p->from.reg);
		break;

	case 36:	/* mov lext/lauto/lreg,r ==> lw o(r30) */
		v = regoff(&p->from);
		r = p->from.reg;
		if(r == NREG)
			r = o->param;
		if(r == REGTMP)
			diag("cant synthesize large constant\n%P", p);
		o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		o2 = OP_IRR(opirr(AOR), v, REGTMP, REGTMP);
		o3 = OP_RRR(oprrr(AADDU), r, REGTMP, REGTMP);
		o4 = OP_IRR(opirr(p->as+ALAST), 0, REGTMP, p->to.reg);
		break;

	case 37:	/* movw r,mr */
		r = SP(2,0)|(4<<21);		/* mtc0 */
		if(p->as == AMOVV)
			r = SP(2,0)|(5<<21);	/* dmtc0 */
		o1 = OP_RRR(r, p->from.reg, 0, p->to.reg);
		break;

	case 38:	/* movw mr,r */
		r = SP(2,0)|(0<<21);		/* mfc0 */
		if(p->as == AMOVV)
			r = SP(2,0)|(1<<21);	/* dmfc0 */
		o1 = OP_RRR(r, p->to.reg, 0, p->from.reg);
		break;

	case 39:	/* rfe ==> jmp+rfe */
		o1 = OP_RRR(oprrr(AJMP), 0, p->to.reg, REGZERO);
		o2 = oprrr(p->as);
		break;

	case 40:	/* word */
		o1 = regoff(&p->to);
		break;

	case 41:	/* movw r,fcr */
		o1 = OP_RRR(SP(2,1)|(2<<21), REGZERO, 0, p->to.reg); 	/* mfcc1 */
		o2 = OP_RRR(SP(2,1)|(6<<21), p->from.reg, 0, p->to.reg);/* mtcc1 */
		break;

	case 42:	/* movw fcr,r */
		o1 = OP_RRR(SP(2,1)|(2<<21), p->to.reg, 0, p->from.reg);/* mfcc1 */
		break;

	case 45:	/* case r */
		if(p->link == P)
			v = p->pc+28;
		else
			v = p->link->pc;
		if(v & (1<<15))
			o1 = OP_IRR(opirr(ALAST), (v>>16)+1, REGZERO, REGTMP);
		else
			o1 = OP_IRR(opirr(ALAST), v>>16, REGZERO, REGTMP);
		o2 = OP_SRR(opirr(ASLL), 2, p->from.reg, p->from.reg);
		o3 = OP_RRR(oprrr(AADD), p->from.reg, REGTMP, REGTMP);
		o4 = OP_IRR(opirr(AMOVW+ALAST), v, REGTMP, REGTMP);
		o5 = OP_RRR(oprrr(ANOR), REGZERO, REGZERO, REGZERO);
		o6 = OP_RRR(oprrr(AJMP), 0, REGTMP, REGZERO);
		o7 = OP_RRR(oprrr(ANOR), REGZERO, REGZERO, REGZERO);
		break;

	case 46:	/* bcase $con,lbra */
		if(p->cond == P)
			v = p->pc;
		else
			v = p->cond->pc;
		o1 = v;
		break;
	}

	v = p->pc;

	return 0;
}

int
isnop(Prog *p)
{
	if(p->as != ANOR)
		return 0;
	if(p->reg != REGZERO && p->reg != NREG)
		return 0;
	if(p->from.type != D_REG || p->from.reg != REGZERO)
		return 0;
	if(p->to.type != D_REG || p->to.reg != REGZERO)
		return 0;
	return 1;
}

long
oprrr(int a)
{
	switch(a) {
	case ANOR:	return OP(4,7);
    ...
	case AJAL:	return OP(1,1);

    ...
	case ATLBP:	return MMU(1,0);
	case ATLBR:	return MMU(0,1);
	case ATLBWI:	return MMU(0,2);
	case ATLBWR:	return MMU(0,6);
	case ARFE:	return MMU(2,0);

	case ADIVF:	return FPF(0,3);
	case ADIVD:	return FPD(0,3);
	case AMULF:	return FPF(0,2);
	case AMULD:	return FPD(0,2);
	case ASUBF:	return FPF(0,1);
	case ASUBD:	return FPD(0,1);
	case AADDF:	return FPF(0,0);
	case AADDD:	return FPD(0,0);

	case AMOVFW:	return FPF(4,4);
	case AMOVDW:	return FPD(4,4);
	case AMOVWF:	return FPW(4,0);
	case AMOVDF:	return FPD(4,0);
	case AMOVWD:	return FPW(4,1);
	case AMOVFD:	return FPF(4,1);
	case AABSF:	return FPF(0,5);
	case AABSD:	return FPD(0,5);
	case AMOVF:	return FPF(0,6);
	case AMOVD:	return FPD(0,6);
	case ANEGF:	return FPF(0,7);
	case ANEGD:	return FPD(0,7);

	case ACMPEQF:	return FPF(6,2);
	case ACMPEQD:	return FPD(6,2);
	case ACMPGTF:	return FPF(7,4);
	case ACMPGTD:	return FPD(7,4);
	case ACMPGEF:	return FPF(7,6);
	case ACMPGED:	return FPD(7,6);
    ...
	}
}

long
opirr(int a)
{
	switch(a) {
    ...
	//case ALAST:	return SP(1,7); // abused for extra codes
    ...

	//case AJMP:	return SP(0,2);
	//case AJAL:	return SP(0,3);
	case ABEQ:	return SP(0,4);
	case ABNE:	return SP(0,5);

	case ABGEZ:	return SP(0,1)|BCOND(0,1);
	case ABGEZAL:	return SP(0,1)|BCOND(2,1);
	case ABGTZ:	return SP(0,7);
	case ABLEZ:	return SP(0,6);
	case ABLTZ:	return SP(0,1)|BCOND(0,0);
	case ABLTZAL:	return SP(0,1)|BCOND(2,0);

	case ABFPT:	return SP(2,1)|(257<<16);
	case ABFPF:	return SP(2,1)|(256<<16);

	case AMOVB:
	case AMOVBU:	return SP(5,0);
	case AMOVH:
	case AMOVHU:	return SP(5,1);
	//case AMOVW:	return SP(5,3);
	//case AMOVV:	return SP(7,7);
	//case AMOVF:	return SP(7,1);
	case AMOVWL:	return SP(5,2);
	case AMOVWR:	return SP(5,6);
	case AMOVVL:	return SP(5,4);
	case AMOVVR:	return SP(5,5);

	case ABREAK:	return SP(5,7);

    // abused, should define an opirr2
	case AMOVWL+ALAST:	return SP(4,2);
	case AMOVWR+ALAST:	return SP(4,6);
	case AMOVVL+ALAST:	return SP(3,2);
	case AMOVVR+ALAST:	return SP(3,3);
	case AMOVB+ALAST:	return SP(4,0);
	case AMOVBU+ALAST:	return SP(4,4);
	case AMOVH+ALAST:	return SP(4,1);
	case AMOVHU+ALAST:	return SP(4,5);
	//case AMOVW+ALAST:	return SP(4,3);
	//case AMOVV+ALAST:	return SP(6,7);
	//case AMOVF+ALAST:	return SP(6,1);

	case ASLLV+ALAST:	return OP(7,4);
	case ASRLV+ALAST:	return OP(7,6);
	case ASRAV+ALAST:	return OP(7,7);
	}
}

int
vshift(int a)
{
	switch(a){
	case ASLLV:		return 1;
	case ASRLV:		return 1;
	case ASRAV:		return 1;
	}
	return 0;
}
