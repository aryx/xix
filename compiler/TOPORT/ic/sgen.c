#include "gc.h"

//goken: not in original, but added to imitate others
Prog*
gtext(Sym *s, int32 stkoff)
{
	//int32 a;
	//
	//a = 0;
	//if(!(textflag & NOSPLIT))
	//	a = argsize();
	//else if(stkoff >= 128)
	//	yyerror("stack frame too large for NOSPLIT function");

	gpseudo(ATEXT, s, nodconst(stkoff));
	//p->to.type = D_CONST2;
	//p->to.offset2 = a;
	return p;
}



void
noretval(int n)
{

	if(n & 1) {
		gins(ANOP, Z, Z);
		p->to.type = D_REG;
		p->to.reg = REGRET;
	}
	if(n & 2) {
		gins(ANOP, Z, Z);
		p->to.type = D_FREG;
		p->to.reg = FREGRET;
	}
}

/*
 *	calculate addressability as follows
 *		CONST ==> 20		$value
 *		NAME ==> 10		name
 *		REGISTER ==> 11		register
 *		INDREG ==> 12		*[(reg)+offset]
 *		&10 ==> 2		$name
 *		ADD(2, 20) ==> 2	$name+offset
 *		ADD(3, 20) ==> 3	$(reg)+offset
 *		&12 ==> 3		$(reg)+offset
 *		*11 ==> 11		??
 *		*2 ==> 10		name
 *		*3 ==> 12		*(reg)+offset
 *	calculate complexity (number of registers)
 */
void
xcom(Node *n)
{
	Node *l, *r;
	int t;

	if(n == Z)
		return;
	l = n->left;
	r = n->right;
	n->addable = 0;
	n->complex = 0;
	switch(n->op) {
	case OCONST:
		n->addable = 20;
		return;

	case OREGISTER:
		n->addable = 11;
		return;

	case OINDREG:
		n->addable = 12;
		return;

	case ONAME:
		n->addable = 10;
		return;

	case OADDR:
		xcom(l);
		if(l->addable == 10)
			n->addable = 2;
		if(l->addable == 12)
			n->addable = 3;
		break;

	case OIND:
		xcom(l);
		if(l->addable == 11)
			n->addable = 12;
		if(l->addable == 3)
			n->addable = 12;
		if(l->addable == 2)
			n->addable = 10;
		break;

	case OADD:
		xcom(l);
		xcom(r);
		if(l->addable == 20) {
			if(r->addable == 2)
				n->addable = 2;
			if(r->addable == 3)
				n->addable = 3;
		}
		if(r->addable == 20) {
			if(l->addable == 2)
				n->addable = 2;
			if(l->addable == 3)
				n->addable = 3;
		}
		break;

	case OASLMUL:
	case OASMUL:
		xcom(l);
		xcom(r);
		t = vlog(r);
		if(t >= 0) {
			n->op = OASASHL;
			r->vconst = t;
			r->type = types[TINT];
			goto shiftassign;
		}
		break;

	case OMUL:
	case OLMUL:
		xcom(l);
		xcom(r);
		t = vlog(l);
		if(t >= 0) {
			n->left = r;
			n->right = l;
			l = r;
			r = n->right;
		}
		t = vlog(r);
		if(t >= 0) {
			n->op = OASHL;
			r->vconst = t;
			r->type = types[TINT];
			simplifyshift(n);
		}
		break;

	case OASLDIV:
		xcom(l);
		xcom(r);
		t = vlog(r);
		if(t >= 0) {
			n->op = OASLSHR;
			r->vconst = t;
			r->type = types[TINT];
			goto shiftassign;
		}
		break;

	case OLDIV:
		xcom(l);
		xcom(r);
		t = vlog(r);
		if(t >= 0) {
			n->op = OLSHR;
			r->vconst = t;
			r->type = types[TINT];
			simplifyshift(n);
		}
		break;

	case OASLMOD:
		xcom(l);
		xcom(r);
		t = vlog(r);
		if(t >= 0) {
			n->op = OASAND;
			r->vconst--;
		}
		break;

	case OLMOD:
		xcom(l);
		xcom(r);
		t = vlog(r);
		if(t >= 0) {
			n->op = OAND;
			r->vconst--;
		}
		break;

	case OLSHR:
	case OASHL:
	case OASHR:
		xcom(l);
		xcom(r);
		simplifyshift(n);
		break;

	case OASLSHR:
	case OASASHL:
	case OASASHR:
		xcom(l);
		xcom(r);
	shiftassign:
		if(typev[l->type->etype] && !machcap(n) && !typev[r->type->etype]){
			if(r->op == OCONST)
				r->type = types[TVLONG];
			else{
				n->right = r = new1(OCAST, r, Z);
				r->type = types[TVLONG];
				xcom(r);
			}
		}
		break;

	default:
		if(l != Z)
			xcom(l);
		if(r != Z)
			xcom(r);
		break;
	}
	if(n->addable >= 10)
		return;

	if(l != Z)
		n->complex = l->complex;
	if(r != Z) {
		if(r->complex == n->complex)
			n->complex = r->complex+1;
		else
		if(r->complex > n->complex)
			n->complex = r->complex;
	}
	if(n->complex == 0)
		n->complex++;

	if(thechar == 'i' && com64(n))
		return;

	switch(n->op) {
	case OFUNC:
		n->complex = FNX;
		break;

	case OADD:
	case OXOR:
	case OAND:
	case OOR:
	case OEQ:
	case ONE:
		/*
		 * immediate operators, make const on right
		 */
		if(l->op == OCONST) {
			n->left = r;
			n->right = l;
		}
		break;

	case OLT:
	case OLE:
	case OGT:
	case OGE:
	case OLO:
	case OLS:
	case OHI:
	case OHS:
		/*
		 * comparison operators, make const on right
		 */
		break;
	}
}

