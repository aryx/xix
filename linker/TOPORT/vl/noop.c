void
noops(void)
{
	for(...) {

		switch(p->as) {
		case ATEXT:

			p->mark |= LABEL|SYNC|...;
			if(p->link)
				p->link->mark |= LABEL;
            ...
			break;

		/* too hard, just leave alone */
		case AMOVW:
			if(p->to.type == D_FCREG ||
			   p->to.type == D_MREG) {
				p->mark |= LABEL|SYNC;
				break;
			}
			if(p->from.type == D_FCREG ||
			   p->from.type == D_MREG) {
				p->mark |= LABEL|SYNC;
				addnop(p);
				addnop(p);
				nop.mfrom.count += 2;
				nop.mfrom.outof += 2;
				break;
			}
			break;

		/* too hard, just leave alone */
		case ACASE:
		case ASYSCALL:
		case AWORD:
		case ATLBWR:
		case ATLBWI:
		case ATLBP:
		case ATLBR:
			p->mark |= LABEL|SYNC;
			break;

		case ANOR:
			if(p->to.type == D_REG && p->to.reg == REGZERO)
				p->mark |= LABEL|SYNC;
			break;

		case ARET:

			if(p->link != P)
				p->link->mark |= LABEL;
			break;

		case ANOP:
            ...
			continue;

		case ABCASE:
			p->mark |= LABEL|SYNC;
			goto dstlab;

		case AJMP:
		case ABEQ:
		case ABGEZ:
		case ABGTZ:
		case ABLEZ:
		case ABLTZ:
		case ABNE:
		case ABFPT:
		case ABFPF:
			p->mark |= BRANCH;

		dstlab:
			q1 = p->cond;
			if(q1 != P) {
				while(q1->as == ANOP) {
					q1 = q1->link;
					p->cond = q1;
				}
				if(!(q1->mark & LEAF))
					q1->mark |= LABEL;
			} else
				p->mark |= LABEL;
			q1 = p->link;
			if(q1 != P)
				q1->mark |= LABEL;
			break;
		}
		q = p;
	}

	for(...) {
		switch(o) {
		case ATEXT:
            ...
			if(autosize) {
				q = prg();
				q->as = AADD;
				q->line = p->line;
				q->from.type = D_CONST;
				q->from.offset = -autosize;
				q->to.type = D_REG;
				q->to.reg = REGSP;

				q->link = p->link;
				p->link = q;
			} else

            // else (not LEAF)
			q1 = prg();
			q1->as = AMOVW;
			q1->line = p->line;
			q1->from.type = D_REG;
			q1->from.reg = REGLINK;
			q1->to.type = D_OREG;
			q1->from.offset = 0;
			q1->to.reg = REGSP;

			q1->link = q->link;
			q->link = q1;
			break;

		case ARET:

			if(curtext->mark & LEAF) {

				if(!autosize) {
					p->as = AJMP;
					p->from = zprg.from;
					p->to.type = D_OREG;
					p->to.offset = 0;
					p->to.reg = REGLINK;
					p->mark |= BRANCH;
					break;
				}
                // else

				p->as = AADD;
				p->from.type = D_CONST;
				p->from.offset = autosize;
				p->to.type = D_REG;
				p->to.reg = REGSP;

				q = prg();
				q->as = AJMP;
				q->line = p->line;
				q->to.type = D_OREG;
				q->to.offset = 0;
				q->to.reg = REGLINK;
				q->mark |= BRANCH;

				q->link = p->link;
				p->link = q;
				break;
			}
            // else
			p->as = AMOVW;
			p->from.type = D_OREG;
			p->from.offset = 0;
			p->from.reg = REGSP;
			p->to.type = D_REG;
			p->to.reg = 2;

			q = p;
			if(autosize) {
				q = prg();
				q->as = AADD;
				q->line = p->line;
				q->from.type = D_CONST;
				q->from.offset = autosize;
				q->to.type = D_REG;
				q->to.reg = REGSP;

				q->link = p->link;
				p->link = q;
			}

			q1 = prg();
			q1->as = AJMP;
			q1->line = p->line;
			q1->to.type = D_OREG;
			q1->to.offset = 0;
			q1->to.reg = 2;
			q1->mark |= BRANCH;

			q1->link = q->link;
			q->link = q1;
			break;

	}

	curtext = P;
	q = P;		/* p - 1 */
	q1 = firstp;	/* top of block */


	o = 0;		/* count of instructions */
	for(p = firstp; p != P; p = p1) {
		p1 = p->link;
		o++;
		if(p->mark & NOSCHED){
			if(q1 != p){
				sched(q1, q);
			}
			for(; p != P; p = p->link){
				if(!(p->mark & NOSCHED))
					break;
				q = p;
			}
			p1 = p;
			q1 = p;
			o = 0;
			continue;
		}
		if(p->mark & (LABEL|SYNC)) {
			if(q1 != p)
				sched(q1, q);
			q1 = p;
			o = 1;
		}
		if(p->mark & (BRANCH|SYNC)) {
			sched(q1, p);
			q1 = p1;
			o = 0;
		}
		if(o >= NSCHED) {
			sched(q1, p);
			q1 = p1;
			o = 0;
		}
		q = p;
	}
}

void
addnop(Prog *p)
{
	Prog *q;

	q = prg();
	q->as = ANOR;
	q->line = p->line;
	q->from.type = D_REG;
	q->from.reg = REGZERO;
	q->to.type = D_REG;
	q->to.reg = REGZERO;

	q->link = p->link;
	p->link = q;
}
