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
		setarch(p);
		if(cursym == s2) {
			ps2 = p;
			p->reg = 1;
		}
		if(cursym == s4) {
			ps4 = p;
			p->reg = 1;
		}
	}
	for(cursym = textp; cursym != nil; cursym = cursym->next)
	for(p = cursym->text; p != P; p = p->link) {
		setarch(p);
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
			 * BL	profin, R2
			 */
			q = prg();
			q->line = p->line;
			q->pc = p->pc;
			q->link = p->link;
			p->link = q;
			p = q;
			p->as = ABL;
			p->to.type = D_BRANCH;
			p->cond = ps2;
			p->to.sym = s2;

			continue;
		}
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
			 * BL	profout
			 */
			p->as = ABL;
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
