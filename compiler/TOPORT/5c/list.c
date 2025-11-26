int
Bconv(Fmt *fp)
{
	char str[STRINGSZ], ss[STRINGSZ], *s;
	Bits bits;
	int i;

	str[0] = 0;
	bits = va_arg(fp->args, Bits);
	while(bany(&bits)) {
		i = bnum(bits);
		if(str[0])
			strcat(str, " ");
		if(var[i].sym == S) {
			sprint(ss, "$%d", var[i].offset);
			s = ss;
		} else
			s = var[i].sym->name;
		if(strlen(str) + strlen(s) + 1 >= STRINGSZ)
			break;
		strcat(str, s);
		bits.b[i/32] &= ~(1L << (i%32));
	}
	return fmtstrcpy(fp, str);
}

char *extra [] = {
	".EQ", ".NE", ".CS", ".CC",
	".MI", ".PL", ".VS", ".VC",
	".HI", ".LS", ".GE", ".LT",
	".GT", ".LE", "", ".NV",
};

int
Pconv(Fmt *fp)
{
    ...
	strcpy(sc, extra[s & C_SCOND]);
	if(s & C_SBIT)
		strcat(sc, ".S");
	if(s & C_PBIT)
		strcat(sc, ".P");
	if(s & C_WBIT)
		strcat(sc, ".W");
	if(s & C_UBIT)		/* ambiguous with FBIT */
		strcat(sc, ".U");
    ...
}


int
Dconv(Fmt *fp)
{
	char str[STRINGSZ];
	Adr *a;
	char *op;
	int v;

	a = va_arg(fp->args, Adr*);
	switch(a->type) {

	case D_CONST2:
		sprint(str, "$%d-%d", a->offset, a->offset2);
		break;

	case D_SHIFT:
		v = a->offset;
		op = "<<>>->@>" + (((v>>5) & 3) << 1);
		if(v & (1<<4))
			sprint(str, "R%d%c%cR%d", v&15, op[0], op[1], (v>>8)&15);
		else
			sprint(str, "R%d%c%c%d", v&15, op[0], op[1], (v>>7)&31);
		if(a->reg != NREG)
			sprint(str+strlen(str), "(R%d)", a->reg);
		break;

	case D_PSR:
		sprint(str, "PSR");
		if(a->name != D_NONE || a->sym != S)
			sprint(str, "%N(PSR)(REG)", a);
		break;

	case D_BRANCH:
		sprint(str, "%d(PC)", a->offset-pc);
		break;
    ...
	}
	return fmtstrcpy(fp, str);
}

int
Rconv(Fmt *fp)
{
	char str[STRINGSZ];
	Adr *a;
	int i, v;

	a = va_arg(fp->args, Adr*);
	sprint(str, "GOK-reglist");
	switch(a->type) {
    ...
	case D_CONST:
	case D_CONST2:
		if(a->reg != NREG)
			break;
		if(a->sym != S)
			break;
		v = a->offset;
		strcpy(str, "");
		for(i=0; i<NREG; i++) {
			if(v & (1<<i)) {
				if(str[0] == 0)
					strcat(str, "[R");
				else
					strcat(str, ",R");
				sprint(strchr(str, 0), "%d", i);
			}
		}
		strcat(str, "]");
	}
	return fmtstrcpy(fp, str);
}
