Prog*
brchain(Prog *p)
{
	int i;

	for(i=0; i<20; i++) {
		if(p == P || p->as != AB)
			return p;
		p = p->cond;
	}
	return P;
}

int
relinv(int a)
{
	switch(a) {
	case ABEQ:	return ABNE;
	case ABNE:	return ABEQ;
	case ABCS:	return ABCC;
	case ABHS:	return ABLO;
	case ABCC:	return ABCS;
	case ABLO:	return ABHS;
	case ABMI:	return ABPL;
	case ABPL:	return ABMI;
	case ABVS:	return ABVC;
	case ABVC:	return ABVS;
	case ABHI:	return ABLS;
	case ABLS:	return ABHI;
	case ABGE:	return ABLT;
	case ABLT:	return ABGE;
	case ABGT:	return ABLE;
	case ABLE:	return ABGT;
	}
	diag("unknown relation: %s", anames[a]);
	return a;
}


static void
xfol(Prog *p, Prog **last)
{
	Prog *q, *r;
	int a, i;

loop:
	if(p == P)
		return;
	setarch(p);
	a = p->as;
	if(a == AB) {
		q = p->cond;
		if(q != P && q->as != ATEXT) {
			p->mark |= FOLL;
			p = q;
			if(!(p->mark & FOLL))
				goto loop;
		}
	}
	if(p->mark & FOLL) {
		for(i=0,q=p; i<4; i++,q=q->link) {
			if(q == *last || q == nil)
				break;
			a = q->as;
			if(a == ANOP) {
				i--;
				continue;
			}
			if(a == AB || (a == ARET && q->scond == 14) || a == ARFE)
				goto copy;
			if(q->cond == P || (q->cond->mark&FOLL))
				continue;
			if(a != ABEQ && a != ABNE)
				continue;
		copy:
			for(;;) {
				r = prg();
				*r = *p;
				if(!(r->mark&FOLL))
					print("cant happen 1\n");
				r->mark |= FOLL;
				if(p != q) {
					p = p->link;
					(*last)->link = r;
					*last = r;
					continue;
				}
				(*last)->link = r;
				*last = r;
				if(a == AB || (a == ARET && q->scond == 14) || a == ARFE)
					return;
				r->as = ABNE;
				if(a == ABNE)
					r->as = ABEQ;
				r->cond = p->link;
				r->link = p->cond;
				if(!(r->link->mark&FOLL))
					xfol(r->link, last);
				if(!(r->cond->mark&FOLL))
					print("cant happen 2\n");
				return;
			}
		}
		a = AB;
		q = prg();
		q->as = a;
		q->line = p->line;
		q->to.type = D_BRANCH;
		q->to.offset = p->pc;
		q->cond = p;
		p = q;
	}
	p->mark |= FOLL;
	(*last)->link = p;
	*last = p;
	if(a == AB || (a == ARET && p->scond == 14) || a == ARFE){
		return;
	}
	if(p->cond != P)
	if(a != ABL && a != ABX && p->link != P) {
		q = brchain(p->link);
		if(a != ATEXT && a != ABCASE)
		if(q != P && (q->mark&FOLL)) {
			p->as = relinv(a);
			p->link = p->cond;
			p->cond = q;
		}
		xfol(p->link, last);
		q = brchain(p->cond);
		if(q == P)
			q = p->cond;
		if(q->mark&FOLL) {
			p->cond = q;
			return;
		}
		p = q;
		goto loop;
	}
	p = p->link;
	goto loop;
}

void
patch(void)
{
	int32 c, vexit;
	Prog *p, *q;
	Sym *s;
	int a;

	mkfwd();
	s = lookup("exit", 0);
	vexit = s->value;
	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			setarch(p);
			a = p->as;

			if((a == ABL || a == ABX || a == AB || a == ARET) &&
			   p->to.type != D_BRANCH && p->to.sym != S) {
				s = p->to.sym;
				switch(s->type) {
				default:
					diag("undefined: %s", s->name);
					s->type = STEXT;
					s->value = vexit;
					continue;	// avoid more error messages
				case STEXT:
					p->to.offset = s->value;
					p->to.type = D_BRANCH;
					break;
				}
			}
			if(p->to.type != D_BRANCH)
				continue;
			c = p->to.offset;
			for(q = textp->text; q != P;) {
				if(c == q->pc)
					break;
				if(q->forwd != P && c >= q->forwd->pc)
					q = q->forwd;
				else
					q = q->link;
			}
			if(q == P) {
				diag("branch out of range %d\n%P", c, p);
				p->to.type = D_NONE;
			}
			p->cond = q;
		}
	}

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		for(p = cursym->text; p != P; p = p->link) {
			setarch(p);
			a = p->as;

			if(p->cond != P) {
				p->cond = brloop(p->cond);
				if(p->cond != P)
				if(p->to.type == D_BRANCH)
					p->to.offset = p->cond->pc;
			}
		}
	}
}

Prog*
brloop(Prog *p)
{
	Prog *q;
	int c;

	for(c=0; p!=P;) {
		if(p->as != AB)
			return p;
		q = p->cond;
		if(q <= p) {
			c++;
			if(q == p || c > 5000)
				break;
		}
		p = q;
	}
	return P;
}


int32
rnd(int32 v, int32 r)
{
	int32 c;

	if(r <= 0)
		return v;
	v += r - 1;
	c = v % r;
	if(c < 0)
		c += r;
	v -= c;
	return v;
}
