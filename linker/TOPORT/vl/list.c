
int
Pconv(Fmt *fp)
{
...
		if(p->mark & NOSCHED)
			s += sprint(s, "*");
}


int
Dconv(Fmt *fp)
{
	switch(a->type) {
	case D_OCONST:
		sprint(str, "$*$%N", a);
		if(a->reg != NREG)
			sprint(str, "%N(R%d)(CONST)", a, a->reg);
		break;

    ...
	case D_MREG:
		sprint(str, "M%d", a->reg);
		if(a->name != D_NONE || a->sym != S)
			sprint(str, "%N(R%d)(REG)", a, a->reg);
		break;

	case D_FCREG:
		sprint(str, "FC%d", a->reg);
		if(a->name != D_NONE || a->sym != S)
			sprint(str, "%N(R%d)(REG)", a, a->reg);
		break;

	case D_LO:
		sprint(str, "LO");
		if(a->name != D_NONE || a->sym != S)
			sprint(str, "%N(LO)(REG)", a);
		break;

	case D_HI:
		sprint(str, "HI");
		if(a->name != D_NONE || a->sym != S)
			sprint(str, "%N(HI)(REG)", a);
		break;

	case D_BRANCH:	/* botch */
		if(curp->cond != P) {
			v = curp->cond->pc;
			if(v >= INITTEXT)
				v -= INITTEXT-HEADR;
			if(a->sym != S)
				sprint(str, "%s+%.5lux(BRANCH)", a->sym->name, v);
			else
				sprint(str, "%.5lux(BRANCH)", v);
		} else
			if(a->sym != S)
				sprint(str, "%s+%ld(APC)", a->sym->name, a->offset);
			else
				sprint(str, "%ld(APC)", a->offset);
		break;

	case D_FCONST:
		sprint(str, "$%e", ieeedtod(a->ieee));
		break;
	}
}
