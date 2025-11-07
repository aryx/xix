
// see ../../runtime/proc.c:/StackGuard
enum
{
	StackBig = 4096,
};

static	Sym*	sym_div;
static	Sym*	sym_divu;
static	Sym*	sym_mod;
static	Sym*	sym_modu;

static Prog *
movrr(Prog *q, int rs, int rd, Prog *p)
{
	if(q == nil)
		q = prg();
	q->as = AMOVW;
	q->line = p->line;
	q->from.type = D_REG;
	q->from.reg = rs;
	q->to.type = D_REG;
	q->to.reg = rd;
	q->link = p->link;
	return q;
}

static Prog *
fnret(Prog *q, int rs, int foreign, Prog *p)
{
	q = movrr(q, rs, REGPC, p);
	if(foreign){	// BX rs
		q->as = ABXRET;
		q->from.type = D_NONE;
		q->from.reg = NREG;
		q->to.reg = rs;
	}
	return q;
}

static Prog *
aword(int32 w, Prog *p)
{
	Prog *q;

	q = prg();
	q->as = AWORD;
	q->line = p->line;
	q->from.type = D_NONE;
	q->reg = NREG;
	q->to.type = D_CONST;
	q->to.offset = w;
	q->link = p->link;
	p->link = q;
	return q;
}

static Prog *
adword(int32 w1, int32 w2, Prog *p)
{
	Prog *q;

	q = prg();
	q->as = ADWORD;
	q->line = p->line;
	q->from.type = D_CONST;
	q->from.offset = w1;
	q->reg = NREG;
	q->to.type = D_CONST;
	q->to.offset = w2;
	q->link = p->link;
	p->link = q;
	return q;
}

void
noops(void)
{
	Prog *p, *q, *q1, *q2;
	int o, foreign;

	/*
     ...
	 * expand BECOME pseudo
	 */


	q = P;
	for(...) {
			switch(p->as) {
            ...
			case ADIV:
			case ADIVU:
			case AMOD:
			case AMODU:
				q = p;
				if(prog_div == P)
					initdiv();
				cursym->text->mark &= ~LEAF;
				setdiv(p->as);
				continue;
	
			case ANOP:
                ...
				if(q1 != P)
					q1->mark |= p->mark;
				continue;
	
			case ABX:
				cursym->text->mark &= ~LEAF;
	
			case ABCASE:
			case AB:
	
			case ABEQ:
			case ABNE:
			case ABCS:
			case ABHS:
			case ABCC:
			case ABLO:
			case ABMI:
			case ABPL:
			case ABVS:
			case ABVC:
			case ABHI:
			case ABLS:
			case ABGE:
			case ABLT:
			case ABGT:
			case ABLE:
				q1 = p->cond;
				if(q1 != P) {
					while(q1->as == ANOP) {
						q1 = q1->link;
						p->cond = q1;
					}
				}
				break;
			}
			q = p;
		}
	}

	for(...) {
			o = p->as;
			switch(o) {
			case ATEXT:
				autosize = p->to.offset + 4;
				if(autosize <= 4)
				if(cursym->text->mark & LEAF) {
					p->to.offset = -4;
					autosize = 0;
				}
	
				if(!autosize && !(cursym->text->mark & LEAF)) {
					if(debug['v'])
						Bprint(&bso, "save suppressed in: %s\n",
							cursym->name);
					Bflush(&bso);
					cursym->text->mark |= LEAF;
				}
                ...
                ...
				break;
	
			case ARET:

				if(cursym->text->mark & LEAF) {
					if(!autosize) {

						p->as = AB;
						p->from = zprg.from;
						p->to.type = D_OREG;
						p->to.offset = 0;
						p->to.reg = REGLINK;
						break;
					}
				}

				break;
	
			case ADIV:
			case ADIVU:
			case AMOD:
			case AMODU:
				if(debug['M'])
					break;
				if(p->from.type != D_REG)
					break;
				if(p->to.type != D_REG)
					break;
				q1 = p;
	
				/* MOV a,4(SP) */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = q1->from.reg;
				p->to.type = D_OREG;
				p->to.reg = REGSP;
				p->to.offset = 4;
	
				/* MOV b,REGTMP */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = q1->reg;
				if(q1->reg == NREG)
					p->from.reg = q1->to.reg;
				p->to.type = D_REG;
				p->to.reg = REGTMP;
				p->to.offset = 0;
	
				/* CALL appropriate */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
					p->as = ABL;

				p->line = q1->line;
				p->to.type = D_BRANCH;
				p->cond = p;
				switch(o) {
				case ADIV:
					p->cond = prog_div;
					p->to.sym = sym_div;
					break;
				case ADIVU:
					p->cond = prog_divu;
					p->to.sym = sym_divu;
					break;
				case AMOD:
					p->cond = prog_mod;
					p->to.sym = sym_mod;
					break;
				case AMODU:
					p->cond = prog_modu;
					p->to.sym = sym_modu;
					break;
				}
	
				/* MOV REGTMP, b */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AMOVW;
				p->line = q1->line;
				p->from.type = D_REG;
				p->from.reg = REGTMP;
				p->from.offset = 0;
				p->to.type = D_REG;
				p->to.reg = q1->to.reg;
	
				/* ADD $8,SP */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
				p->as = AADD;
				p->from.type = D_CONST;
				p->from.reg = NREG;
				p->from.offset = 8;
				p->reg = NREG;
				p->to.type = D_REG;
				p->to.reg = REGSP;
	
				/* SUB $8,SP */
				q1->as = ASUB;
				q1->from.type = D_CONST;
				q1->from.offset = 8;
				q1->from.reg = NREG;
				q1->reg = NREG;
				q1->to.type = D_REG;
				q1->to.reg = REGSP;
	
				break;
			case AMOVW:
				break;
			case AMOVB:
			case AMOVBU:
			case AMOVH:
			case AMOVHU:
				break;
			case AMOVM:
				break;
			case AB:
				break;
			case ABL:
			case ABX:
				break;
			}
		}
	}
}

static void
sigdiv(char *n)
{
	Sym *s;

	s = lookup(n, 0);
	if(s->type == STEXT)
		if(s->sig == 0)
			s->sig = SIGNINTERN;
}

void
divsig(void)
{
	sigdiv("_div");
	sigdiv("_divu");
	sigdiv("_mod");
	sigdiv("_modu");
}

void
initdiv(void)
{
	Sym *s2, *s3, *s4, *s5;

	if(prog_div != P)
		return;
	sym_div = s2 = lookup("_div", 0);
	sym_divu = s3 = lookup("_divu", 0);
	sym_mod = s4 = lookup("_mod", 0);
	sym_modu = s5 = lookup("_modu", 0);
	prog_div = s2->text;
	prog_divu = s3->text;
	prog_mod = s4->text;
	prog_modu = s5->text;
	if(prog_div == P) {
		diag("undefined: %s", s2->name);
		prog_div = cursym->text;
	}
	if(prog_divu == P) {
		diag("undefined: %s", s3->name);
		prog_divu = cursym->text;
	}
	if(prog_mod == P) {
		diag("undefined: %s", s4->name);
		prog_mod = cursym->text;
	}
	if(prog_modu == P) {
		diag("undefined: %s", s5->name);
		prog_modu = cursym->text;
	}
}

static void
setdiv(int as)
{
	Prog *p = nil;

	switch(as){
	case ADIV: p = prog_div; break;
	case ADIVU: p = prog_divu; break;
	case AMOD: p = prog_mod; break;
	case AMODU: p = prog_modu; break;
	}
}
