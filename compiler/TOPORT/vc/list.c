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
			sprint(ss, "$%ld", var[i].offset);
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

int
Dconv(Fmt *fp)
{
	char str[STRINGSZ];
	Adr *a;

	a = va_arg(fp->args, Adr*);
	switch(a->type) {

	case D_FCREG:
		sprint(str, "FCR%d", a->reg);
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
   }
}
