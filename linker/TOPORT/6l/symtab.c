
char *elfstrdat;
int elfstrsize;
int maxelfstr;

void genasmsym(void (*put)(char*, int, vlong, vlong, int, Sym*));

int
putelfstr(char *s)
{
	int off, n;

	if(elfstrsize == 0 && s[0] != 0) {
		// first entry must be empty string
		putelfstr("");
	}

	n = strlen(s)+1;
	if(elfstrsize+n > maxelfstr) {
		maxelfstr = 2*(elfstrsize+n+(1<<20));
		elfstrdat = realloc(elfstrdat, maxelfstr);
	}
	off = elfstrsize;
	elfstrsize += n;
	memmove(elfstrdat+off, s, n);
	return off;
}

void
putelfsymb(char *s, int t, vlong addr, vlong size, int ver, Sym *go)
{
	int bind, type, shndx, stroff;
	
	bind = STB_GLOBAL;
	switch(t) {
	default:
		return;
	case 'T':
		type = STT_FUNC;
		shndx = elftextsh + 0;
		break;
	case 'D':
		type = STT_OBJECT;
		shndx = elftextsh + 1;
		break;
	case 'B':
		type = STT_OBJECT;
		shndx = elftextsh + 2;
		break;
	}
	
	stroff = putelfstr(s);
	lputl(stroff);	// string
	cput((bind<<4)|(type&0xF));
	cput(0);
	wputl(shndx);
	vputl(addr);
	vputl(size);
}

void
asmelfsym(void)
{
	genasmsym(putelfsymb);
}


void
putsymb(char *s, int t, vlong v, vlong size, int ver, Sym *go)
{
	int i, f, l;
	vlong gv;

	if(t == 'f')
		s++;
	l = 4;
	if(!debug['8']){
		lputb(v>>32);
		l = 8;
	}
	lputb(v);
	if(ver)
		t += 'a' - 'A';
	cput(t+0x80);			/* 0x80 is variable length */

	if(t == 'Z' || t == 'z') {
		cput(s[0]);
		for(i=1; s[i] != 0 || s[i+1] != 0; i += 2) {
			cput(s[i]);
			cput(s[i+1]);
		}
		cput(0);
		cput(0);
		i++;
	}
	else {
		for(i=0; s[i]; i++)
			cput(s[i]);
		cput(0);
	}
	gv = 0;
	if(go) {
		if(!go->reachable)
			diag("unreachable type %s", go->name);
		gv = go->value+INITDAT;
	}
	if(l == 8)
		lputb(gv>>32);
	lputb(gv);
	symsize += l + 1 + i+1 + l;

	if(debug['n']) {
		if(t == 'z' || t == 'Z') {
			Bprint(&bso, "%c %.8llux ", t, v);
			for(i=1; s[i] != 0 || s[i+1] != 0; i+=2) {
				f = ((s[i]&0xff) << 8) | (s[i+1]&0xff);
				Bprint(&bso, "/%x", f);
			}
			Bprint(&bso, "\n");
			return;
		}
		if(ver)
			Bprint(&bso, "%c %.8llux %s<%d> %s (%.8llux)\n", t, v, s, ver, go ? go->name : "", gv);
		else
			Bprint(&bso, "%c %.8llux %s %s (%.8llux)\n", t, v, s, go ? go->name : "", gv);
	}
}

void
genasmsym(void (*put)(char*, int, vlong, vlong, int, Sym*))
{
	Auto *a;
	Sym *s;
	int h;

	s = lookup("etext", 0);
	if(s->type == STEXT)
		put(s->name, 'T', s->value, s->size, s->version, 0);

	for(h=0; h<NHASH; h++) {
		for(s=hash[h]; s!=S; s=s->hash) {
			switch(s->type) {
			case SCONST:
			case SRODATA:
			case SDATA:
			case SELFDATA:
				if(!s->reachable)
					continue;
				put(s->name, 'D', symaddr(s), s->size, s->version, s->gotype);
				continue;

			case SBSS:
				if(!s->reachable)
					continue;
				put(s->name, 'B', symaddr(s), s->size, s->version, s->gotype);
				continue;

			case SFIXED:
				put(s->name, 'B', s->value, s->size, s->version, s->gotype);
				continue;

			case SFILE:
				put(s->name, 'f', s->value, 0, s->version, 0);
				continue;
			}
		}
	}

	for(s = textp; s != nil; s = s->next) {
		/* filenames first */
		for(a=s->autom; a; a=a->link)
			if(a->type == D_FILE)
				put(a->asym->name, 'z', a->aoffset, 0, 0, 0);
			else
			if(a->type == D_FILE1)
				put(a->asym->name, 'Z', a->aoffset, 0, 0, 0);

		put(s->name, 'T', s->value, s->size, s->version, s->gotype);

		/* frame, auto and param after */
		put(".frame", 'm', s->text->to.offset+8, 0, 0, 0);

		for(a=s->autom; a; a=a->link)
			if(a->type == D_AUTO)
				put(a->asym->name, 'a', -a->aoffset, 0, 0, a->gotype);
			else
			if(a->type == D_PARAM)
				put(a->asym->name, 'p', a->aoffset, 0, 0, a->gotype);
	}
	if(debug['v'] || debug['n'])
		Bprint(&bso, "symsize = %ud\n", symsize);
	Bflush(&bso);
}

void
asmsym(void)
{
	genasmsym(putsymb);
}

