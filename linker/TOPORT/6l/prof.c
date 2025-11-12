void
doprof1(void)
{
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
        ...
		p->as = AADDL;
		p->from.type = D_CONST;
		p->from.offset = 1;
		p->to.type = D_EXTERN;
		p->to.sym = s;
		p->to.offset = n*4 + 4;

	}
}

void
doprof2(void)
{
	Sym *s2, *s4;
	Prog *p, *q, *ps2, *ps4;

	s2 = lookup("_profin", 0);
	s4 = lookup("_profout", 0);
	if(s2->type != STEXT || s4->type != STEXT) {
		diag("_profin/_profout not defined");
		return;
	}

	ps2 = P;
	ps4 = P;
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		p = cursym->text;
		if(p->from.sym == s2) {
			p->from.scale = 1;
			ps2 = p;
		}
		if(p->from.sym == s4) {
			p->from.scale = 1;
			ps4 = p;
		}
	}
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		p = cursym->text;

		if(p->from.scale & NOPROF)	/* dont profile */
			continue;

		/*
		 * JMPL	profin
		 */
		q = prg();
		q->line = p->line;
		q->pc = p->pc;
		q->link = p->link;
		p->link = q;
		p = q;
		p->as = ACALL;
		p->to.type = D_BRANCH;
		p->pcond = ps2;
		p->to.sym = s2;

		for(; p; p=p->link) {
			if(p->as == ARET) {
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
				p->as = ACALL;
				p->from = zprg.from;
				p->to = zprg.to;
				p->to.type = D_BRANCH;
				p->pcond = ps4;
				p->to.sym = s4;
	
				p = q;
			}
		}
	}
}
