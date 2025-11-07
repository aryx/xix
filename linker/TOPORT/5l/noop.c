
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
			setarch(p);
	
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
			setarch(p);

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
#ifdef CALLEEBX
				if(p->from.sym->foreign){
					if(thumb)
						// don't allow literal pool to seperate these
						p = adword(0xe28f7001, 0xe12fff17, p); // arm add 1, pc, r7 and bx r7
						// p = aword(0xe12fff17, aword(0xe28f7001, p)); // arm add 1, pc, r7 and bx r7
					else
						p = aword(0x4778, p);	// thumb bx pc and 2 bytes padding
				}
#endif
                ...
	
				if(thumb){
					if(!(cursym->text->mark & LEAF)){
						q = movrr(nil, REGLINK, REGTMPT-1, p);
						p->link = q;
						q1 = prg();
						q1->as = AMOVW;
						q1->line = p->line;
						q1->from.type = D_REG;
						q1->from.reg = REGTMPT-1;
						q1->to.type = D_OREG;
						q1->to.name = D_NONE;
						q1->to.reg = REGSP;
						q1->to.offset = 0;
						q1->link = q->link;
						q->link = q1;
					}
					if(autosize){
						q2 = prg();
						q2->as = ASUB;
						q2->line = p->line;
						q2->from.type = D_CONST;
						q2->from.offset = autosize;
						q2->to.type = D_REG;
						q2->to.reg = REGSP;
						q2->link = p->link;
						p->link = q2;
					}
					break;
				} 
                ...
				break;
	
			case ARET:
				foreign = seenthumb && (cursym->foreign || cursym->fnptr);

				if(cursym->text->mark & LEAF) {
					if(!autosize) {
						if(thumb){
							p = fnret(p, REGLINK, foreign, p);
							break;
						}

						p->as = foreign ? ABXRET : AB;
						p->from = zprg.from;
						p->to.type = D_OREG;
						p->to.offset = 0;
						p->to.reg = REGLINK;
						break;
					}
				}
				if(thumb){
					if(cursym->text->mark & LEAF){
						if(autosize){
							p->as = AADD;
							p->from.type = D_CONST;
							p->from.offset = autosize;
							p->to.type = D_REG;
							p->to.reg = REGSP;
							q = nil;
						}
						else
							q = p;
						q = fnret(q, REGLINK, foreign, p);
						if(q != p)
							p->link = q;
					}
					else{
						p->as = AMOVW;
						p->from.type = D_OREG;
						p->from.name = D_NONE;
						p->from.reg = REGSP;
						p->from.offset = 0;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						if(autosize){
							q = prg();
							q->as = AADD;
							q->from.type = D_CONST;
							q->from.offset = autosize;
							q->to.type = D_REG;
							q->to.reg = REGSP;
							q->link = p->link;
							p->link = 	q;
						}
						else
							q = p;
						q1 = fnret(nil, REGTMPT-1, foreign, p);
						q1->link = q->link;
						q->link = q1;
					}
					break;
				}
				if(foreign) {
#define	R	1
					p->as = AMOVW;
					p->from.type = D_OREG;
					p->from.name = D_NONE;
					p->from.reg = REGSP;
					p->from.offset = 0;
					p->to.type = D_REG;
					p->to.reg = R;
					q = prg();
					q->as = AADD;
					q->scond = p->scond;
					q->line = p->line;
					q->from.type = D_CONST;
					q->from.offset = autosize;
					q->to.type = D_REG;
					q->to.reg = REGSP;
					q->link = p->link;
					p->link = q;
					q1 = prg();
					q1->as = ABXRET;
					q1->scond = p->scond;
					q1->line = p->line;
					q1->to.type = D_OREG;
					q1->to.offset = 0;
					q1->to.reg = R;
					q1->link = q->link;
					q->link = q1;
#undef	R
				}
				else {
                  ...

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
				p->to.reg = prog_div->from.sym->thumb ? REGTMPT : REGTMP;
				p->to.offset = 0;
	
				/* CALL appropriate */
				q = prg();
				q->link = p->link;
				p->link = q;
				p = q;
	
#ifdef CALLEEBX
				p->as = ABL;
#else
				if(prog_div->from.sym->thumb)
					p->as = thumb ? ABL : ABX;
				else
					p->as = thumb ? ABX : ABL;
#endif
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
				p->from.reg = prog_div->from.sym->thumb ? REGTMPT : REGTMP;
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
				if(thumb){
					Adr *a = &p->from;
	
					if(a->type == D_CONST && ((a->name == D_NONE && a->reg == REGSP) || a->name == D_AUTO || a->name == D_PARAM) && (a->offset & 3))
						diag("SP offset not multiple of 4");
				}
				break;
			case AMOVB:
			case AMOVBU:
			case AMOVH:
			case AMOVHU:
				if(thumb){
					if(p->from.type == D_OREG && (p->from.name == D_AUTO || p->from.name == D_PARAM || (p->from.name == D_CONST && p->from.reg == REGSP))){
						q = prg();
						*q = *p;
						if(p->from.name == D_AUTO)
							q->from.offset += autosize;
						else if(p->from.name == D_PARAM)
							q->from.offset += autosize+4;
						q->from.name = D_NONE;
						q->from.reg = REGTMPT;
						p = movrr(p, REGSP, REGTMPT, p);
						q->link = p->link;
						p->link = q;
					}
					if(p->to.type == D_OREG && (p->to.name == D_AUTO || p->to.name == D_PARAM || (p->to.name == D_CONST && p->to.reg == REGSP))){
						q = prg();
						*q = *p;
						if(p->to.name == D_AUTO)
							q->to.offset += autosize;
						else if(p->to.name == D_PARAM)
							q->to.offset += autosize+4;
						q->to.name = D_NONE;
						q->to.reg = REGTMPT;
						p = movrr(p, REGSP, REGTMPT, p);
						q->link = p->link;
						p->link = q;
						if(q->to.offset < 0 || q->to.offset > 255){	// complicated
							p->to.reg = REGTMPT+1;			// mov sp, r8
							q1 = prg();
							q1->line = p->line;
							q1->as = AMOVW;
							q1->from.type = D_CONST;
							q1->from.offset = q->to.offset;
							q1->to.type = D_REG;
							q1->to.reg = REGTMPT;			// mov $o, r7
							p->link = q1;
							q1->link = q;
							q1 = prg();
							q1->line = p->line;
							q1->as = AADD;
							q1->from.type = D_REG;
							q1->from.reg = REGTMPT+1;
							q1->to.type = D_REG;
							q1->to.reg = REGTMPT;			// add r8, r7
							p->link->link = q1;
							q1->link = q;
							q->to.offset = 0;				// mov* r, 0(r7)
							/* phew */
						}
					}
				}
				break;
			case AMOVM:
				if(thumb){
					if(p->from.type == D_OREG){
						if(p->from.offset == 0)
							p->from.type = D_REG;
						else
							diag("non-zero AMOVM offset");
					}
					else if(p->to.type == D_OREG){
						if(p->to.offset == 0)
							p->to.type = D_REG;
						else
							diag("non-zero AMOVM offset");
					}
				}
				break;
			case AB:
				if(thumb && p->to.type == D_OREG){
					if(p->to.offset == 0){
						p->as = AMOVW;
						p->from.type = D_REG;
						p->from.reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGPC;
					}
					else{
						p->as = AADD;
						p->from.type = D_CONST;
						p->from.offset = p->to.offset;
						p->reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						q = prg();
						q->as = AMOVW;
						q->line = p->line;
						q->from.type = D_REG;
						q->from.reg = REGTMPT-1;
						q->to.type = D_REG;
						q->to.reg = REGPC;
						q->link = p->link;
						p->link = q;
					}
				}
				if(seenthumb && !thumb && p->to.type == D_OREG && p->to.reg == REGLINK){
					// print("warn %s:	b	(R%d)	assuming a return\n", cursym->name, p->to.reg);
					p->as = ABXRET;
				}
				break;
			case ABL:
			case ABX:
				if(thumb && p->to.type == D_OREG){
					if(p->to.offset == 0){
						p->as = o;
						p->from.type = D_NONE;
						p->to.type = D_REG;
					}
					else{
						p->as = AADD;
						p->from.type = D_CONST;
						p->from.offset = p->to.offset;
						p->reg = p->to.reg;
						p->to.type = D_REG;
						p->to.reg = REGTMPT-1;
						q = prg();
						q->as = o;
						q->line = p->line;
						q->from.type = D_NONE;
						q->to.type = D_REG;
						q->to.reg = REGTMPT-1;
						q->link = p->link;
						p->link = q;
					}
				}
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
	if(thumb != p->from.sym->thumb)
		p->from.sym->foreign = 1;
}
