
vlong
argsize(void)
{
	Type *t;
	int32 s;

	s = align(0, thisfn->link, Aarg0);
	for(t=thisfn->down; t!=T; t=t->down) {
		switch(t->etype) {
		case TVOID:
			break;
		case TDOT:
			yyerror("function takes ... without textflag NOSPLIT");
			s += 64;
			break;
		default:
			s = align(s, t, Aarg1);
			s = align(s, t, Aarg2);
			break;
		}
	}
	if(thechar == '6')
		s = (s+7) & ~7;
	else
		s = (s+3) & ~3;
	return s;
}

void
codgen(...)
{
	cursafe = 0;
    ...
    //TODO: do not do p = gtext(...), WEIRD
    // pointer change from 0xaaaa to 0xffff, weird
    ///gtext(n1->sym, stkoff);

	/*
	 * isolate first argument
	 */
	if(REGARG >= 0) {
		if(typesuv[thisfn->link->etype]) {
			nod1 = *nodret->left;
			nodreg(&nod, &nod1, REGARG);
			gmove(&nod, &nod1);
		} else
		if(firstarg && typechlp[firstargtype->etype]) {
			nod1 = *nodret->left;
			nod1.sym = firstarg;
			nod1.type = firstargtype;
			nod1.xoffset = align(0, firstargtype, Aarg1);
			nod1.etype = firstargtype->etype;
			nodreg(&nod, &nod1, REGARG);
			gmove(&nod, &nod1);
		}
	}

	retok = 0;

	canreach = 1;
	warnreach = 1;

	///gen(n);

	if(canreach && thisfn->link->etype != TVOID)
		warn(Z, "no return at end of function: %s", n1->sym->name);

	noretval(3);

	///gbranch(ORETURN);

	if(!debug['N'] || debug['R'] || debug['P'])
		regopt(sp);

	if(thechar=='6' || thechar=='7')	/* [sic] */
		maxargsafe = xround(maxargsafe, 8);
	///sp->to.offset += maxargsafe;
}

void
supgen(Node *n)
{
	int owarn;
	long spc;
	Prog *sp;

	if(n == Z)
		return;
	suppress++;
	owarn = warnreach;
	warnreach = 0;
	spc = pc;
	sp = lastp;

	gen(n);

	lastp = sp;
	pc = spc;
	sp->link = nil;
	suppress--;
	warnreach = owarn;
}


// added for 7c
Node*
uncomma(Node *n)
{
	while(n != Z && n->op == OCOMMA) {
		cgen(n->left, Z);
		n = n->right;
	}
	return n;
}

void
gen(Node *n)
{
	Case *cn;
	long sbc, scc;
	int snbreak, sncontin;
	int f, oldreach;
    ...

loop:

	if(!canreach) {
		switch(o) {
		case OLABEL:
		case OCASE:
		case OLIST:
		case OBREAK:
		case OFOR:
		case OWHILE:
		case ODWHILE:
			/* all handled specially - see switch body below */
			break;
		default:
			if(warnreach) {
				warn(n, "unreachable code %O", o);
				warnreach = 0;
			}
		}
	}

	switch(o) {

	default:
		complex(n);
		///cgen(n, Z);
		///break;

	case ORETURN:
		canreach = 0;
		warnreach = !suppress;
		complex(n);

		l = n->left;
		if(l == Z) {
			noretval(3);
			///gbranch(ORETURN);
			///break;
		}
		if(typecmplx[n->type->etype]) {
			sugen(l, nodret, n->type->width);
			noretval(3);
			gbranch(ORETURN);
			break;
		}
		///regret(&nod, n);
		///cgen(l, &nod);
		///regfree(&nod);

		if(typefd[n->type->etype])
			noretval(1);
		else
			noretval(2);
		///gbranch(ORETURN);
		break;

	case OLABEL:
		canreach = 1;
        ...
		goto rloop;

	case OGOTO:
		canreach = 0;
		warnreach = !suppress;
        ...
		if(suppress)
			return;
        ...

	case OCASE:
		canreach = 1;
		l = n->left;
		if(cases == C)
			diag(n, "case/default outside a switch");
		if(l == Z) {
			cas();
			cases->val = 0;
			cases->def = 1;
			cases->label = pc;
			cases->isv = 0;
			goto rloop;
		}
		complex(l);
		if(l->type == T)
			goto rloop;
		if(l->op == OCONST)
		if(typeword[l->type->etype] && l->type->etype != TIND) {
			cas();
			cases->val = l->vconst;
			cases->def = 0;
			cases->label = pc;
			cases->isv = typev[l->type->etype];
			goto rloop;
		}
		diag(n, "case expression must be integer constant");
		goto rloop;

	case OSWITCH:
		l = n->left;
		complex(l);
		if(l->type == T)
			break;
		if(!typeword[l->type->etype] || l->type->etype == TIND) {
			diag(n, "switch expression must be integer");
			break;
		}

		gbranch(OGOTO);		/* entry */
		sp = p;

		cn = cases;
		cases = C;
		cas();

		sbc = breakpc;
		breakpc = pc;
		snbreak = nbreak;
		nbreak = 0;
		gbranch(OGOTO);
		spb = p;

		gen(n->right);		/* body */
		if(canreach){
			gbranch(OGOTO);
			patch(p, breakpc);
			nbreak++;
		}

		patch(sp, pc);
		regalloc(&nod, l, Z);
		/* always signed */
		if(typev[l->type->etype])
			nod.type = types[TVLONG];
		else
			nod.type = types[TLONG];
		cgen(l, &nod);
		doswit(&nod);
		regfree(&nod);
		patch(spb, pc);

		cases = cn;
		breakpc = sbc;
		canreach = nbreak!=0;
		if(canreach == 0)
			warnreach = !suppress;
		nbreak = snbreak;
		break;

	case OWHILE:
	case ODWHILE:
        ...
		///bcomplex(l, Z);		/* test */
		///patch(p, breakpc);
		if(l->op != OCONST || vconst(l) == 0)
			nbreak++;
        ...
		canreach = nbreak!=0;
		if(canreach == 0)
			warnreach = !suppress;
		nbreak = snbreak;
		break;

	case OFOR:
		///l = n->left;
		if(!canreach && l->right->left && warnreach) {
			warn(n, "unreachable code FOR");
			warnreach = 0;
		}
		///gen(l->right->left);	/* init */
		///gbranch(OGOTO);		/* entry */
		///sp = p;

		/*
		 * if there are no incoming labels in the
		 * body and the top's not reachable, warn
		 */
		if(!canreach && warnreach && deadheads(n)) {
			warn(n, "unreachable code %O", o);
			warnreach = 0;
		}
        ...
		canreach = 1;
        ...
		if(canreach){
			gbranch(OGOTO);
			patch(p, continpc);
			ncontin++;
		}
		if(!ncontin && l->right->right && warnreach) {
			warn(l->right->right, "unreachable FOR inc");
			warnreach = 0;
		}

		patch(spb, pc);
		canreach = nbreak!=0;
		if(canreach == 0)
			warnreach = !suppress;
		nbreak = snbreak;
		ncontin = sncontin;
		break;

	case OCONTINUE:
        ...
		ncontin++;
		canreach = 0;
		warnreach = !suppress;
		break;

	case OBREAK:
        ...
		/*
		 * Don't complain about unreachable break statements.
		 * There are breaks hidden in yacc's output and some people
		 * write return; break; in their switch statements out of habit.
		 * However, don't confuse the analysis by inserting an
		 * unreachable reference to breakpc either.
		 */
		if(!canreach)
			break;
        ...
		nbreak++;
		canreach = 0;
		warnreach = !suppress;
		break;

	case OIF:
		l = n->left;
		if(bcomplex(l, n->right)) {
			if(typefd[l->type->etype])
				f = !l->fconst;
			else
				f = !l->vconst;
			if(debug['c'])
				print("%L const if %s\n", nearln, f ? "false" : "true");
			if(f) {
				canreach = 1;
				supgen(n->right->left);
				oldreach = canreach;
				canreach = 1;
				gen(n->right->right);
				/*
				 * treat constant ifs as regular ifs for
				 * reachability warnings.
				 */
				if(!canreach && oldreach && debug['w'] < 2)
					warnreach = 0;
			}
			else {
				canreach = 1;
				gen(n->right->left);
				oldreach = canreach;
				canreach = 1;
				supgen(n->right->right);
				/*
				 * treat constant ifs as regular ifs for
				 * reachability warnings.
				 */
				if(!oldreach && canreach && debug['w'] < 2)
					warnreach = 0;
				canreach = oldreach;
			}
		}
		else {
			sp = p;
			canreach = 1;
			if(n->right->left != Z)
				gen(n->right->left);
			oldreach = canreach;
			canreach = 1;
			if(n->right->right != Z) {
				gbranch(OGOTO);
				patch(sp, pc);
				sp = p;
				gen(n->right->right);
			}
			patch(sp, pc);
			canreach = canreach || oldreach;
			if(canreach == 0)
				warnreach = !suppress;
		}
		break;

	case OSET:
	case OUSED:
		usedset(n->left, o);
		break;
	}
}

void
usedset(Node *n, int o)
{
	if(n->op == OLIST) {
		usedset(n->left, o);
		usedset(n->right, o);
		return;
	}
	complex(n);
	switch(n->op) {
	case OADDR:	/* volatile */
		gins(ANOP, n, Z);
		break;
	case ONAME:
		if(o == OSET)
			gins(ANOP, Z, n);
		else
			gins(ANOP, n, Z);
		break;
	}
}

int
bcomplex(Node *n, Node *c)
{
	Node *b, nod;

	complex(n);
	if(n->type != T)
	if(tcompat(n, T, n->type, tnot))
		n->type = T;
	if(n->type == T) {
		gbranch(OGOTO);
		return 0;
	}
	if(c != Z && n->op == OCONST && deadheads(c))
		return 1;
	if(typev[n->type->etype] && machcap(Z)) {
		b = &nod;
		b->op = ONE;
		b->left = n;
		b->right = new(0, Z, Z);
		*b->right = *nodconst(0);
		b->right->type = n->type;
		b->type = types[TLONG];
		cgen(b, Z);
		return 0;
	}
	bool64(n);
	boolgen(n, 1, Z);
	return 0;
}
