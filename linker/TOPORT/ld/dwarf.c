// Copyright 2010 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include	"l.h"
#include	"lib.h"
#include	"../ld/dwarf.h"
#include	"../ld/dwarf_defs.h"
#include	"../ld/elf_.h"
#include	"../ld/macho.h"

/*
 * Offsets and sizes of the debug_* sections in the cout file.
 */

static vlong abbrevo;
static vlong abbrevsize;
static vlong lineo;
static vlong linesize;
static vlong infoo;
static vlong infosize;
static vlong frameo;
static vlong framesize;

/*
 *  Basic I/O
 */

static void
addrput(vlong addr)
{
	switch(PtrSize) {
	case 4:
		LPUT(addr);
		break;
	case 8:
		VPUT(addr);
		break;
	}
}


static int
uleb128enc(uvlong v, char* dst)
{
	uint8 c, len;

	len = 0;
	do {
		c = v & 0x7f;
		v >>= 7;
		if (v)
			c |= 0x80;
		if (dst)
			*dst++ = c;
		len++;
	} while (c & 0x80);
	return len;
};


static int
sleb128enc(vlong v, char *dst)
{
	uint8 c, s, len;

	len = 0;
	do {
		c = v & 0x7f;
		s = v & 0x40;
		v >>= 7;
		if ((v != -1 || !s) && (v != 0 || s))
			c |= 0x80;
		if (dst)
			*dst++ = c;
		len++;
	} while(c & 0x80);
	return len;
}

static void
uleb128put(vlong v)
{
	char buf[10];
	strnput(buf, uleb128enc(v, buf));
}

static void
sleb128put(vlong v)
{
	char buf[10];
	strnput(buf, sleb128enc(v, buf));
}

/*
 * Defining Abbrevs.  This is hardcoded, and there will be
 * only a handful of them.  The DWARF spec places no restriction on
 * the ordering of atributes in the Abbrevs and DIEs, and we will
 * always write them out in the order of declaration in the abbrev.
 * This implementation relies on tag, attr < 127, so they serialize as
 * a char, hence we do not support user-defined tags or attributes.
 */
typedef struct DWAttrForm DWAttrForm;
struct DWAttrForm {
	uint8 attr;
	uint8 form;
};

// index into the abbrevs table below.
enum
{
	DW_ABRV_NULL,
	DW_ABRV_COMPUNIT,
	DW_ABRV_FUNCTION,
	DW_NABRV
};

typedef struct DWAbbrev DWAbbrev;
struct DWAbbrev {
	uint8 tag;
	uint8 children;
	DWAttrForm attr[30];
} abbrevs[DW_NABRV] = {
	/* The mandatory DW_ABRV_NULL entry. */
	{ 0 },
	/* COMPUNIT */
	{
		DW_TAG_compile_unit, DW_CHILDREN_yes,
		DW_AT_name,	 DW_FORM_string,
		DW_AT_language,	 DW_FORM_data1,
		DW_AT_low_pc,	 DW_FORM_addr,
		DW_AT_high_pc,	 DW_FORM_addr,
		DW_AT_stmt_list, DW_FORM_data4,
		0, 0
	},
	/* FUNCTION */
	{
		DW_TAG_subprogram, DW_CHILDREN_no,
		DW_AT_name,	 DW_FORM_string,
		DW_AT_low_pc,	 DW_FORM_addr,
		DW_AT_high_pc,	 DW_FORM_addr,
		0, 0
	},
};

static void
writeabbrev(void)
{
	int i, n;

	abbrevo = cpos();
	for (i = 1; i < DW_NABRV; i++) {
		// See section 7.5.3
		uleb128put(i);
		uleb128put(abbrevs[i].tag);
		cput(abbrevs[i].children);
		// 0 is not a valid attr or form, so we can treat this as
		// a string
		n = strlen((char*)abbrevs[i].attr) / 2;
		strnput((char*)abbrevs[i].attr,
			(n+1) * sizeof(DWAttrForm));
	}
	cput(0);
	abbrevsize = cpos() - abbrevo;
}

/*
 * Debugging Information Entries and their attributes
 */

// For DW_CLS_string and _block, value should contain the length, and
// data the data, for all others, value is the whole thing and data is
// null.

typedef struct DWAttr DWAttr;
struct DWAttr {
	DWAttr *link;
	uint8 atr;  // DW_AT_
	uint8 cls;  // DW_CLS_
	vlong value;
	char *data;
};

typedef struct DWDie DWDie;
struct DWDie {
	int abbrev;
	DWDie *link;
	DWDie *child;
	DWAttr *attr;
};

// top level compilation unit DIE's
static DWDie *dwinfo;

static DWDie*
newdie(DWDie *link, int abbrev)
{
	DWDie *die;

	die = mal(sizeof *die);
	die->abbrev = abbrev;
	die->link = link;
	return die;
}

static DWAttr*
newattr(DWDie *die, uint8 attr, int cls, vlong value, char *data)
{
	DWAttr *a;

	a = mal(sizeof *a);
	a->link = die->attr;
	die->attr = a;
	a->atr = attr;
	a->cls = cls;
	a->value = value;
	a->data = data;
	return a;
}

static void
putattr(int form, int cls, vlong value, char *data)
{
	switch(form) {
	case DW_FORM_addr:	// address
		addrput(value);
		break;

	case DW_FORM_block1:	// block
		value &= 0xff;
		cput(value);
		while(value--)
			cput(*data++);
		break;

	case DW_FORM_block2:	// block
		value &= 0xffff;
		WPUT(value);
		while(value--)
			cput(*data++);
		break;

	case DW_FORM_block4:	// block
		value &= 0xffffffff;
		LPUT(value);
		while(value--)
			cput(*data++);
		break;

	case DW_FORM_block:	// block
		uleb128put(value);
		while(value--)
			cput(*data++);
		break;

	case DW_FORM_data1:	// constant
		cput(value);
		break;

	case DW_FORM_data2:	// constant
		WPUT(value);
		break;

	case DW_FORM_data4:	// constant, {line,loclist,mac,rangelist}ptr
		LPUT(value);
		break;

	case DW_FORM_data8:	// constant, {line,loclist,mac,rangelist}ptr
		VPUT(value);
		break;

	case DW_FORM_sdata:	// constant
		sleb128put(value);
		break;

	case DW_FORM_udata:	// constant
		uleb128put(value);
		break;

	case DW_FORM_string:	// string
		strnput(data, value+1);
		break;

	case DW_FORM_flag:	// flag
		cput(value?1:0);
		break;

	case DW_FORM_strp:	// string
	case DW_FORM_ref_addr:	// reference
	case DW_FORM_ref1:	// reference
	case DW_FORM_ref2:	// reference
	case DW_FORM_ref4:	// reference
	case DW_FORM_ref8:	// reference
	case DW_FORM_ref_udata:	// reference
	case DW_FORM_indirect:	// (see Section 7.5.3)
	default:
		diag("Unsupported atribute form %d / class %d", form, cls);
		errorexit();
	}
}

static void
putattrs(int abbrev, DWAttr* attr)
{
	DWAttr *attrs[DW_AT_recursive + 1];
	DWAttrForm* af;

	memset(attrs, 0, sizeof attrs);
	for( ; attr; attr = attr->link)
		attrs[attr->atr] = attr;
	for(af = abbrevs[abbrev].attr; af->attr; af++)
		if (attrs[af->attr])
			putattr(af->form,
				attrs[af->attr]->cls,
				attrs[af->attr]->value,
				attrs[af->attr]->data);
		else
			putattr(af->form, 0, 0, 0);
}

static void putdie(DWDie* die);

static void
putdies(DWDie* die)
{
	for(; die; die = die->link)
		putdie(die);
}

static void
putdie(DWDie* die)
{
	uleb128put(die->abbrev);
	putattrs(die->abbrev, die->attr);
	if (abbrevs[die->abbrev].children) {
		putdies(die->child);
		cput(0);
	}
}

static void
reverselist(DWDie** list)
{
	DWDie *curr, * prev;

	curr = *list;
	prev = 0;
	while(curr) {
		DWDie* next = curr->link;
		curr->link = prev;
		prev = curr;
		curr = next;
	}
	*list = prev;
}

static void
reversetree(DWDie** list)
{
	 DWDie *die;

	 reverselist(list);
	 if (*list != nil && abbrevs[(*list)->abbrev].children)
		 for (die = *list; die != nil; die = die->link)
			 reversetree(&die->child);
}

/*
 * Filename fragments for the line history stack.
 */

static char **ftab;
static int ftabsize;

void
dwarfaddfrag(int n, char *frag)
{
	int s;

	if (n >= ftabsize) {
		s = ftabsize;
		ftabsize = 1 + n + (n >> 2);
		ftab = realloc(ftab, ftabsize * sizeof(ftab[0]));
		memset(ftab + s, 0, (ftabsize - s) * sizeof(ftab[0]));
	}

	if (*frag == '<')
		frag++;
	ftab[n] = frag;
}

// Returns a malloc'ed string, piecewise copied from the ftab.
static char *
decodez(char *s)
{
	int len, o;
	char *ss, *f;
	char *r, *rb, *re;

	len = 0;
	ss = s + 1;	// first is 0
	while((o = ((uint8)ss[0] << 8) | (uint8)ss[1]) != 0) {
		if (o < 0 || o >= ftabsize) {
			diag("corrupt z entry");
			return 0;
		}
		f = ftab[o];
		if (f == nil) {
			diag("corrupt z entry");
			return 0;
		}
		len += strlen(f) + 1;	// for the '/'
		ss += 2;
	}

	if (len == 0)
		return 0;

	r = malloc(len + 1);
	rb = r;
	re = rb + len + 1;

	s++;
	while((o = ((uint8)s[0] << 8) | (uint8)s[1]) != 0) {
		f = ftab[o];
		if (rb == r || rb[-1] == '/')
			rb = seprint(rb, re, "%s", f);
		else
			rb = seprint(rb, re, "/%s", f);
		s += 2;
	}
	return r;
}

/*
 * The line history itself
 */

static char **histfile;	   // [0] holds "<eof>", DW_LNS_set_file arguments must be > 0.
static int  histfilesize;
static int  histfilecap;

static void
clearhistfile(void)
{
	int i;

	// [0] holds "<eof>"
	for (i = 1; i < histfilesize; i++)
		free(histfile[i]);
	histfilesize = 0;
}

static int
addhistfile(char *zentry)
{
	char *fname;

	if (histfilesize == histfilecap) {
		histfilecap = 2 * histfilecap + 2;
		histfile = realloc(histfile, histfilecap * sizeof(char*));
	}
	if (histfilesize == 0)
		histfile[histfilesize++] = "<eof>";

	fname = decodez(zentry);
	if (fname == 0)
		return -1;
	// Don't fill with duplicates (check only top one).
	if (strcmp(fname, histfile[histfilesize-1]) == 0) {
		free(fname);
		return histfilesize - 1;
	}
	histfile[histfilesize++] = fname;
	return histfilesize - 1;
}

// Go's runtime C sources are sane, and Go sources nest only 1 level,
// so 16 should be plenty.
static struct {
	int file;
	vlong line;
} includestack[16];
static int includetop;
static vlong absline;

typedef struct Linehist Linehist;
struct Linehist {
	Linehist *link;
	vlong absline;
	vlong line;
	int file;
};

static Linehist *linehist;

static void
checknesting(void)
{
	int i;

	if (includetop < 0) {
		diag("corrupt z stack");
		errorexit();
	}
	if (includetop >= nelem(includestack)) {
		diag("nesting too deep");
		for (i = 0; i < nelem(includestack); i++)
			diag("\t%s", histfile[includestack[i].file]);
		errorexit();
	}
}

/* find z and Z entries in the Auto list (of a Prog), and reset the history stack */
static char *
inithist(Auto *a)
{
	char *unitname;
	Linehist *lh;

	for (; a; a = a->link)
		if (a->type == D_FILE)
			break;
	if (a==nil)
		return 0;

	// We have a new history.  They are guaranteed to come completely
	// at the beginning of the compilation unit.
	if (a->aoffset != 1) {
		diag("stray 'z' with offset %d", a->aoffset);
		return 0;
	}

	unitname = decodez(a->asym->name);

	// Clear the history.
	clearhistfile();
	includetop = 0;
	includestack[includetop].file = 0;
	includestack[includetop].line = -1;
	absline = 0;
	while (linehist != nil) {
		lh = linehist->link;
		free(linehist);
		linehist = lh;
	}

	// Construct the new one.
	for (; a; a = a->link) {
		if (a->type == D_FILE) {  // 'z'
			int f = addhistfile(a->asym->name);
			if (f < 0) {	   // pop file
				includetop--;
				checknesting();
			} else if(f != includestack[includetop].file) { // pushed a new file
				includestack[includetop].line += a->aoffset - absline;
				includetop++;
				checknesting();
				includestack[includetop].file = f;
				includestack[includetop].line = 1;

			}
			absline = a->aoffset;
		} else if (a->type == D_FILE1) {  // 'Z'
			// We could just fixup the current
			// linehist->line, but there doesn't appear to
			// be a guarantee that every 'Z' is preceded
			// by it's own 'z', so do the safe thing and
			// update the stack and push a new Linehist
			// entry
			includestack[includetop].line =	 a->aoffset;
		} else
			continue;
		if (linehist == 0 || linehist->absline != absline) {
			Linehist* lh = malloc(sizeof *lh);
			lh->link = linehist;
			lh->absline = absline;
			linehist = lh;
		}
		linehist->file = includestack[includetop].file;
		linehist->line = includestack[includetop].line;
	}
	return unitname;
}

static Linehist *
searchhist(vlong absline)
{
	Linehist *lh;

	for (lh = linehist; lh; lh = lh->link)
		if (lh->absline <= absline)
			break;
	return lh;
}

static int
guesslang(char *s)
{
	if(strlen(s) >= 3 && strcmp(s+strlen(s)-3, ".go") == 0)
		return DW_LANG_Go;

	return DW_LANG_C;
}

/*
 * Generate short opcodes when possible, long ones when neccesary.
 * See section 6.2.5
 */

enum {
	LINE_BASE = -1,
	LINE_RANGE = 4,
	OPCODE_BASE = 5
};

static void
putpclcdelta(vlong delta_pc, vlong delta_lc)
{
	if (LINE_BASE <= delta_lc && delta_lc < LINE_BASE+LINE_RANGE) {
		vlong opcode = OPCODE_BASE + (delta_lc - LINE_BASE) + (LINE_RANGE * delta_pc);
		if (OPCODE_BASE <= opcode && opcode < 256) {
			cput(opcode);
			return;
		}
	}

	if (delta_pc) {
		cput(DW_LNS_advance_pc);
		sleb128put(delta_pc);
	}

	cput(DW_LNS_advance_line);
	sleb128put(delta_lc);
	cput(DW_LNS_copy);
}


/*
 * Walk prog table, emit line program and build DIE tree.
 */

// flush previous compilation unit.
static void
flushunit(vlong pc, vlong unitstart)
{
	vlong here;

	if (dwinfo != nil && pc != 0) {
		newattr(dwinfo, DW_AT_high_pc, DW_CLS_ADDRESS, pc+1, 0);
	}

	if (unitstart >= 0) {
		cput(0);  // start extended opcode
		uleb128put(1);
		cput(DW_LNE_end_sequence);
		cflush();

		here = cpos();
		seek(cout, unitstart, 0);
		LPUT(here - unitstart - sizeof(int32));
		cflush();
		seek(cout, here, 0);
	}
}

static void
writelines(void)
{
	Prog *q;
	Sym *s;
	char *unitname;
	vlong unitstart;
	vlong pc, epc, lc, llc, lline;
	int currfile;
	int i;
	Linehist *lh;

	unitstart = -1;
	epc = pc = 0;
	lc = 1;
	llc = 1;
	currfile = -1;
	lineo = cpos();

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		s = cursym;

		// Look for history stack.  If we find one,
		// we're entering a new compilation unit
		if((unitname = inithist(s->autom)) != 0) {
			flushunit(epc, unitstart);
			unitstart = cpos();
			if(debug['v'] > 1) {
				print("dwarf writelines found %s\n", unitname);
				Linehist* lh;
				for (lh = linehist; lh; lh = lh->link)
					print("\t%8lld: [%4lld]%s\n",
					      lh->absline, lh->line, histfile[lh->file]);
			}
			dwinfo = newdie(dwinfo, DW_ABRV_COMPUNIT);
			newattr(dwinfo, DW_AT_name, DW_CLS_STRING, strlen(unitname), unitname);
			newattr(dwinfo, DW_AT_language, DW_CLS_CONSTANT, guesslang(unitname), 0);
			newattr(dwinfo, DW_AT_stmt_list, DW_CLS_PTR, unitstart - lineo, 0);
			newattr(dwinfo, DW_AT_low_pc, DW_CLS_ADDRESS, s->text->pc, 0);
			// Write .debug_line Line Number Program Header (sec 6.2.4)
			// Fields marked with (*) must be changed for 64-bit dwarf
			LPUT(0);   // unit_length (*), will be filled in later.
			WPUT(3);   // version
			LPUT(11);  // header_length (*)
			cput(1);   // minimum_instruction_length
			cput(1);   // default_is_stmt
			cput(LINE_BASE);     // line_base
			cput(LINE_RANGE);    // line_range
			cput(OPCODE_BASE);   // opcode_base (we only use 1..4)
			cput(0);   // standard_opcode_lengths[1]
			cput(1);   // standard_opcode_lengths[2]
			cput(1);   // standard_opcode_lengths[3]
			cput(1);   // standard_opcode_lengths[4]
			cput(0);   // include_directories  (empty)
			cput(0);   // file_names (empty) (emitted by DW_LNE's below)
			for (i=1; i < histfilesize; i++) {
				cput(0);  // start extended opcode
				uleb128put(1 + strlen(histfile[i]) + 4);
				cput(DW_LNE_define_file);
				strnput(histfile[i], strlen(histfile[i]) + 4);
				// 4 zeros: the string termination + 3 fields.
			}

			epc = pc = s->text->pc;
			currfile = 1;
			lc = 1;
			llc = 1;

			cput(0);  // start extended opcode
			uleb128put(1 + PtrSize);
			cput(DW_LNE_set_address);
			addrput(pc);
		}
		if (!s->reachable)
			continue;

		if (unitstart < 0) {
			diag("reachable code before seeing any history: %P", s->text);
			continue;
		}

		dwinfo->child = newdie(dwinfo->child, DW_ABRV_FUNCTION);
		newattr(dwinfo->child, DW_AT_name, DW_CLS_STRING, strlen(s->name), s->name);
		newattr(dwinfo->child, DW_AT_low_pc, DW_CLS_ADDRESS, s->value, 0);
		epc = s->value + s->size;
		newattr(dwinfo->child, DW_AT_high_pc, DW_CLS_ADDRESS, epc, 0);

		for(q = s->text; q != P; q = q->link) {
			lh = searchhist(q->line);
			if (lh == nil) {
				diag("corrupt history or bad absolute line: %P", q);
				continue;
			}
			if (lh->file < 1) {  // 0 is the past-EOF entry.
				//diag("instruction with line number past EOF in %s: %P", unitname, q);
				continue;
			}

			lline = lh->line + q->line - lh->absline;
			if (debug['v'] > 1)
				print("%6llux %s[%lld] %P\n", q->pc, histfile[lh->file], lline, q);

			if (q->line == lc)
				continue;
			if (currfile != lh->file) {
				currfile = lh->file;
				cput(DW_LNS_set_file);
				uleb128put(currfile);
			}
			putpclcdelta(q->pc - pc, lline - llc);
			pc  = q->pc;
			lc  = q->line;
			llc = lline;
		}
	}

	flushunit(epc, unitstart);
	linesize = cpos() - lineo;
}

/*
 *  Emit .debug_frame
 */
enum
{
	CIERESERVE = 16,
	DATAALIGNMENTFACTOR = -4,	// TODO -PtrSize?
	FAKERETURNCOLUMN = 16		// TODO gdb6 doesnt like > 15?
};

static void
putpccfadelta(vlong deltapc, vlong cfa)
{
	if (deltapc < 0x40) {
		cput(DW_CFA_advance_loc + deltapc);
	} else if (deltapc < 0x100) {
		cput(DW_CFA_advance_loc1);
		cput(deltapc);
	} else if (deltapc < 0x10000) {
		cput(DW_CFA_advance_loc2);
		WPUT(deltapc);
	} else {
		cput(DW_CFA_advance_loc4);
		LPUT(deltapc);
	}

	cput(DW_CFA_def_cfa_offset_sf);
	sleb128put(cfa / DATAALIGNMENTFACTOR);
}

static void
writeframes(void)
{
	Prog *p, *q;
	Sym *s;
	vlong fdeo, fdesize, pad, cfa, pc;

	frameo = cpos();

	// Emit the CIE, Section 6.4.1
	LPUT(CIERESERVE);  // initial length, must be multiple of PtrSize
	LPUT(0xffffffff);  // cid.
	cput(3);	// dwarf version
	cput(0);	// augmentation ""
	uleb128put(1);	// code_alignment_factor
	sleb128put(DATAALIGNMENTFACTOR); // guess
	uleb128put(FAKERETURNCOLUMN); // return_address_register

	cput(DW_CFA_def_cfa);
	uleb128put(DWARFREGSP);	// register SP (**ABI-dependent, defined in l.h)
	uleb128put(PtrSize);	// offset

	cput(DW_CFA_offset + FAKERETURNCOLUMN);	 // return address
	uleb128put(-PtrSize / DATAALIGNMENTFACTOR);	// at cfa - x*4

	// 4 is to exclude the length field.
	pad = CIERESERVE + frameo + 4 - cpos();
	if (pad < 0) {
		diag("CIERESERVE too small by %lld bytes.", -pad);
		errorexit();
	}
	strnput("", pad);

	for(cursym = textp; cursym != nil; cursym = cursym->next) {
		s = cursym;
		if (!s->reachable)
			continue;

		fdeo = cpos();
		// Emit a FDE, Section 6.4.1, starting wit a placeholder.
		LPUT(0);	// length, must be multiple of PtrSize
		LPUT(0);	// Pointer to the CIE above, at offset 0
		addrput(0);	// initial location
		addrput(0);	// address range

		cfa = PtrSize;	// CFA starts at sp+PtrSize
		p = s->text;
		pc = p->pc;

		for(q = p; q->link != P; q = q->link) {
			if (q->spadj == 0)
				continue;

			cfa += q->spadj;
			putpccfadelta(q->link->pc - pc, cfa);
			pc = q->link->pc;
		}

		fdesize = cpos() - fdeo - 4;	// exclude the length field.
		pad = rnd(fdesize, PtrSize) - fdesize;
		strnput("", pad);
		fdesize += pad;
		cflush();

		// Emit the FDE header for real, Section 6.4.1.
		seek(cout, fdeo, 0);
		LPUT(fdesize);
		LPUT(0);
		addrput(p->pc);
		addrput(s->size);

		cflush();
		seek(cout, fdeo + 4 + fdesize, 0);
	}

	cflush();
	framesize = cpos() - frameo;
}

/*
 *  Walk DWarfDebugInfoEntries, and emit .debug_info
 */
static void
writeinfo(void)
{
	DWDie *compunit;
	vlong unitstart;

	reversetree(&dwinfo);

	infoo = cpos();

	for (compunit = dwinfo; compunit; compunit = compunit->link) {
		unitstart = cpos();

		// Write .debug_info Compilation Unit Header (sec 7.5.1)
		// Fields marked with (*) must be changed for 64-bit dwarf
		LPUT(0);   // unit_length (*), will be filled in later.
		WPUT(3);   // version
		LPUT(0);   // debug_abbrev_offset (*)
		cput(PtrSize);	 // address_size

		putdie(compunit);

		cflush();
		vlong here = cpos();
		seek(cout, unitstart, 0);
		LPUT(here - unitstart - sizeof(int32));
		cflush();
		seek(cout, here, 0);
	}

	cflush();
	infosize = cpos() - infoo;
}

void
dwarfemitdebugsections(void)
{
	writeabbrev();
	writelines();
	writeframes();
	writeinfo();
}

/*
 *  Elf.
 */
enum
{
	ElfStrDebugAbbrev,
	ElfStrDebugAranges,
	ElfStrDebugFrame,
	ElfStrDebugInfo,
	ElfStrDebugLine,
	ElfStrDebugLoc,
	ElfStrDebugMacinfo,
	ElfStrDebugPubNames,
	ElfStrDebugPubTypes,
	ElfStrDebugRanges,
	ElfStrDebugStr,
	NElfStrDbg
};

vlong elfstrdbg[NElfStrDbg];

void
dwarfaddshstrings(Sym *shstrtab)
{
	elfstrdbg[ElfStrDebugAbbrev]   = addstring(shstrtab, ".debug_abbrev");
	elfstrdbg[ElfStrDebugAranges]  = addstring(shstrtab, ".debug_aranges");
	elfstrdbg[ElfStrDebugFrame]    = addstring(shstrtab, ".debug_frame");
	elfstrdbg[ElfStrDebugInfo]     = addstring(shstrtab, ".debug_info");
	elfstrdbg[ElfStrDebugLine]     = addstring(shstrtab, ".debug_line");
	elfstrdbg[ElfStrDebugLoc]      = addstring(shstrtab, ".debug_loc");
	elfstrdbg[ElfStrDebugMacinfo]  = addstring(shstrtab, ".debug_macinfo");
	elfstrdbg[ElfStrDebugPubNames] = addstring(shstrtab, ".debug_pubnames");
	elfstrdbg[ElfStrDebugPubTypes] = addstring(shstrtab, ".debug_pubtypes");
	elfstrdbg[ElfStrDebugRanges]   = addstring(shstrtab, ".debug_ranges");
	elfstrdbg[ElfStrDebugStr]      = addstring(shstrtab, ".debug_str");
}

void
dwarfaddelfheaders(void)
{
	ElfShdr *sh;

	sh = newElfShdr(elfstrdbg[ElfStrDebugAbbrev]);
	sh->type = SHT_PROGBITS;
	sh->off = abbrevo;
	sh->size = abbrevsize;
	sh->addralign = 1;

	sh = newElfShdr(elfstrdbg[ElfStrDebugLine]);
	sh->type = SHT_PROGBITS;
	sh->off = lineo;
	sh->size = linesize;
	sh->addralign = 1;

	sh = newElfShdr(elfstrdbg[ElfStrDebugFrame]);
	sh->type = SHT_PROGBITS;
	sh->off = frameo;
	sh->size = framesize;
	sh->addralign = 1;

	sh = newElfShdr(elfstrdbg[ElfStrDebugInfo]);
	sh->type = SHT_PROGBITS;
	sh->off = infoo;
	sh->size = infosize;
	sh->addralign = 1;
}

/*
 * Macho
 */
void
dwarfaddmachoheaders(void)
{
	MachoSect *msect;
	MachoSeg *ms;

	vlong fakestart;

	// Zero vsize segments won't be loaded in memory, even so they
	// have to be page aligned in the file.
	fakestart = abbrevo & ~0xfff;

	ms = newMachoSeg("__DWARF", 4);
	ms->fileoffset = fakestart;
	ms->filesize = abbrevo-fakestart + abbrevsize+linesize+framesize+infosize;

	msect = newMachoSect(ms, "__debug_abbrev");
	msect->off = abbrevo;
	msect->size = abbrevsize;

	msect = newMachoSect(ms, "__debug_line");
	msect->off = lineo;
	msect->size = linesize;

	msect = newMachoSect(ms, "__debug_frame");
	msect->off = frameo;
	msect->size = framesize;

	msect = newMachoSect(ms, "__debug_info");
	msect->off = infoo;
	msect->size = infosize;
}
