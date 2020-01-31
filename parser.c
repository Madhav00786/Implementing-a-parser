/*
* File name: parser.c
* Compiler: Visual Studio 2019
* Author: Isha Isha, Madhav Sachdeva
* Course: CST 8152 - Compilers, Lab Section: 021, 023
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose:   It builds the parser
* Function List: parser(), match(), match_successful(), syn_eh(), syn_printe(),
gen_incode(), program(), opt_statements(), statements(), statement(), statements_prime(),
assignment(), assignment_exp(), arithmetic_exp(), unary_arithmetic_exp(),
additive_arithmetic_exp(), additive_arithmetic_exp_prime(), multiplicative_arithmetic_exp(),
multiplicative_arithmetic_exp_prime(), primary_arithmetic_exp(), selection(), conditional_exp(),
or_expression(), and_expression(), relational_expression(), a_relational_exp(),
a_relational_exp_prime(), s_relational_exp(), s_relational_exp_prime(), string_exp(),
string_exp_prime(), primary_string_exp(), or_expression_prime(), and_expression_prime(),
iteration(), input(), variable_list(), variable_identifier(), variable_list_prime(),
output(), output_list()
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#include "parser.h"

/*
Purpose:			To start the parser
Author:				Sv. Ranev
History/Versions:	1.0 Decemeber 3, 2019
Called Function:	malar_next_token(), program(), match(), get_incode()
Parameters:			
Return Value:		
Algorithm:			
*/
void parser(void) {
	lookahead = malar_next_token();
	program();
	match(SEOF_T, NO_ATTR);
	gen_incode("PLATY: Source file parsed");
}

/*
Purpose:			To match the current input token and the token required by parser
Author:				Isha Isha, Madhav Sachdeva
History/Versions:	1.0 - Decemeber 3rd, 2019
Called Function:	syn_eh(), syn_printe(), malar_next_token()
Parameters:			pr_token_code, pr_token_attribute
Return Value:		
Algorithm:
*/
void match(int pr_token_code, int pr_token_attribute) {
	if (lookahead.code != pr_token_code) {
		syn_eh(pr_token_code);
		return;
	}
	switch (pr_token_code) {
	case KW_T:
		if (pr_token_attribute != lookahead.attribute.kwt_idx) {
			if (pr_token_attribute == FALSE || pr_token_attribute == TRUE) {
				break;
			}
			syn_eh(pr_token_code);
			return;
		}
		break;

	case LOG_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code);
			return;
		}
		break;

	case ART_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code);
			return;
		}
		break;

	case REL_OP_T:
		if (pr_token_attribute != lookahead.attribute.get_int) {
			syn_eh(pr_token_code);
			return;
		}
		break;
	}

	if (lookahead.code != SEOF_T) {
		lookahead = malar_next_token();
		if (lookahead.code == ERR_T) {
			syn_printe();
			lookahead = malar_next_token();
			synerrno++;
		}
	}
}

/*
Purpose:			To handle the errors
Author:				Isha Isha, Madhav Sachdeva
History/Versions:	1.0 - Decemeber 3rd, 2019
Called Function:	syn_printe(), malar_next_token(), exit()
Parameters:			sync_token_code
Return Value:		
Algorithm:			runs an endless loop until it finds a token code matching the one required by the parser or
                    until it reaches the end of file
*/
void syn_eh(int sync_token_code) {
	syn_printe();
	synerrno++;

	do {
		lookahead = malar_next_token();
		if (lookahead.code == SEOF_T && sync_token_code != SEOF_T) {
			exit(synerrno);
		}
	} while (lookahead.code != sync_token_code);

	if (lookahead.code != SEOF_T) {
		lookahead = malar_next_token();
		return;
	}
	return;
}

/*
Purpose:			
Author:				Svillen Ranev
History/Versions:	1.0 - Decemeber 3rd, 2019
Called Function:	
Parameters:			
Return Value:		
Algorithm:			
*/
/* error printing function for Assignment 3 (Parser), F19 */
void syn_printe() {
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n", line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch (t.code) {
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n", t.attribute.err_lex);
		break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("SEOF_T\t\t%d\t\n", t.attribute.seof);
		break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T:/* SVID_T    3  String Variable identifier token */
		printf("%s\n", t.attribute.vid_lex);
		break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n", t.attribute.flt_value);
		break;
	case INL_T: /* INL_T      5   Integer literal token */
		printf("%d\n", t.attribute.get_int);
		break;
	case STR_T:/* STR_T     6   String literal token */
		b_mark(str_LTBL, t.attribute.str_offset);
		printf("%s\n", b_location(str_LTBL));
		break;

	case SCC_OP_T: /* 7   String concatenation operator token */
		printf("NA\n");
		break;

	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n");
		break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */
		printf("%d\n", t.attribute.get_int);
		break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n", t.attribute.get_int);
		break;

	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n");
		break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
		printf("NA\n");
		break;
	case LBR_T: /*    14   Left brace token */
		printf("NA\n");
		break;
	case RBR_T: /*    15  Right brace token */
		printf("NA\n");
		break;

	case KW_T: /*     16   Keyword token */
		printf("%s\n", kw_table[t.attribute.get_int]);
		break;

	case COM_T: /* 17   Comma token */
		printf("NA\n");
		break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
		printf("NA\n");
		break;
	default:
		printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
	}/*end switch*/
}/* end syn_printe()*/

/*
Purpose: To print the parsed statements
Author : Isha Isha, Madhav Sachdeva
History / Versions : 1.0 3 December 2019
Called Function : printf()
Parameters : string 
Return Value : 
Algorithm : 
*/
void gen_incode(char* string) {
	printf("%s\n", string);
}


/*
Author: Isha Isha, Madhav Sachdeva
<program>->PLATYPUS {<opt_statements>}
FIRST(<program>) = { KW_T(PLATYPUS) }
*/
void program(void) {
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("PLATY: Program parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<opt_statements> -> <statements> | ϵ
FIRST(<opt_statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ϵ}
*/
void opt_statements(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		statements();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default:
		gen_incode("PLATY: Opt_statements parsed");
	}
	
}

/*
 Author: Isha Isha, Madhav Sachdeva
<statements> -> <statement><statements’>
FIRST(<statements>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statements(void) {
	statement();
	statements_prime();
}

/*
 Author: Isha Isha, Madhav Sachdeva
<statement> -> <assignment statement> | <selection statement> | <iteration statement> | <input statement> | <output statement>
FIRST(<statement>) = {FIRST(<assignment statement>), FIRST(<selection statement>), FIRST(<ieration statement>), FIRST(<input statement>), FIRST(<output statement>), }
FIRST(<statement>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE)}
*/
void statement(void) {
	switch (lookahead.code) {
	case AVID_T:
		assignment();
		break;
	case SVID_T:
		assignment();
		break;
	case KW_T:
		if (lookahead.attribute.get_int == IF)
		{
			selection();
		}
		else if (lookahead.attribute.get_int == WHILE)
		{
			iteration();
		}
		else if (lookahead.attribute.get_int == INPUT) 
		{
			input();
		}
		else if (lookahead.attribute.get_int == WRITE)
		{
			output();
		}
		break;

	default:
		syn_printe();
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<statements’> -> <statement><statements’> | ϵ
FIRST(<statements’>) = {AVID_T, SVID_T, KW_T(IF), KW_T(WHILE), KW_T(READ), KW_T(WRITE), ϵ}
*/
void statements_prime(void) {
	switch (lookahead.code)
	{
	case AVID_T:
		statement();
		statements_prime();
		break;
	case SVID_T:
		statement();
		statements_prime();
		break;
	case KW_T:
		if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE
			&& lookahead.attribute.get_int != THEN && lookahead.attribute.get_int != REPEAT
			&& lookahead.attribute.get_int != TRUE && lookahead.attribute.get_int != FALSE) {
			statements();
			break;
		}
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<assignment statement> -> <assignment expression>;
FIRST(<assignment statement>) = {AVID_T, SVID_T}
*/
void assignment(void) {
	assignment_exp();
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Assignment statement parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<assignment expression> ->  AVID = <arithmetic expression> | SVID = <string expression>
FIRST(<assignment expression>) = {AVID_T, SVID_T}
*/
void assignment_exp(void) {
	switch (lookahead.code)
	{
	case AVID_T:
		match(AVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		arithmetic_exp();
		gen_incode("PLATY: Assignment expression (arithmetic) parsed");
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		match(ASS_OP_T, EQ);
		string_exp();
		gen_incode("PLATY: Assignment expression (string) parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<arithmetic expression> - > <unary arithmetic expression> | <additive arithmetic expression>
FIRST(<arithmetic expression>) = {ART_OP_T, AVID_T, FPL_T, INL_T, LPR_T}
*/
void arithmetic_exp(void) {
	switch (lookahead.code)
	{
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) {
			unary_arithmetic_exp();
		}
		else
		{
			syn_printe();
		}
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
	case LPR_T:
		additive_arithmetic_exp();
		gen_incode("PLATY: Arithmetic expression parsed");
		break;
	default:
		break;
	}
}

/*
 Author:Isha Isha, Madhav Sachdeva 
<unary arithmetic expression> -> - <primary arithmetic expression> | + <primary arithmetic expression>
FIRST(<unary arithmetic expression>) = {ART_OP_T(-), ART_OP_T(+)}
*/
void unary_arithmetic_exp(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_exp();
		}
		else {
			syn_printe();
		}
		break;
	default:
		break;
	}
	gen_incode("PLATY: Unary arithmetic expression parsed");
}


/*
 Author: Isha Isha, Madhav Sachdeva
<additive arithmetic expression> -> <multiplicative arithmetic expression><additive arithmetic expression’>
FIRST(<additive arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void additive_arithmetic_exp(void) {
	multiplicative_arithmetic_exp();
	additive_arithmetic_exp_prime();
}

/*
 Author: Isha Isha, Madhav Sachdeva
<additive arithmetic expression’> -> + <multiplicative arithmetic expression><additive arithmetic expression'>
| - <multiplicative arithmetic expression><additive arithmetic expression'> | ϵ
FIRST(<additive arithmetic expression>) = {ART_OP_T(+), ART_OP_T(-), ϵ}
*/
void additive_arithmetic_exp_prime(void) {
	switch (lookahead.code)
	{
	case ART_OP_T:
		if (lookahead.attribute.arr_op == PLUS || lookahead.attribute.arr_op == MINUS) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			multiplicative_arithmetic_exp();
			additive_arithmetic_exp_prime();
			gen_incode("PLATY: Additive arithmetic expression parsed");
		}
		else {
			syn_printe();
		}
		break;
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<multiplicative arithmetic expression> -> <primary arithmetic expression><multiplicative arithmetic expression’>
FIRST(<multiplicative arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void multiplicative_arithmetic_exp(void) {
	primary_arithmetic_exp();
	multiplicative_arithmetic_exp_prime();
}

/*
 Author: Isha Isha, Madhav Sachdeva
<multiplicative arithmetic expression’> -> * <primary arithmetic expression><multiplicative arithmetic expression’>
| / <primary arithmetic expression><multiplicative arithmetic expression’> | ϵ
FIRST(<multiplicative arithmetic expression’>) = {ART_OP_T{*}, ART_OP_T(/), ϵ }
*/
void multiplicative_arithmetic_exp_prime(void) {
	switch (lookahead.code) {
	case ART_OP_T:
		if (lookahead.attribute.arr_op == MULT || lookahead.attribute.arr_op == DIV) {
			match(ART_OP_T, lookahead.attribute.arr_op);
			primary_arithmetic_exp();
			multiplicative_arithmetic_exp_prime();
			gen_incode("PLATY: Multiplicative arithmetic expression parsed");
		}
		break;
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary arithmetic expression> -> AVID_T | FPL_T | INL_T | (<arithmetic expression>)
FIRST(<primary arithmetic expression>) = {AVID_T, FPL_T, INL_T, LPR_T}
*/
void primary_arithmetic_exp(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		break;
	case LPR_T:
		match(LPR_T, NO_ATTR);
		arithmetic_exp();
		match(RPR_T, NO_ATTR);
		break;
	default:
		break;
	}
	gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<selection statement> -> IF <pre-condition>  (<conditional expression>) THEN { <opt_statements> } ELSE { <opt_statements> } ;
FIRST(<selection statement>) = {KW_T(IF)}
*/
void selection(void) {
	match(KW_T, IF);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);

	conditional_exp();
	match(RPR_T, NO_ATTR);

	match(KW_T, THEN);
	match(LBR_T, NO_ATTR);

	opt_statements();
	match(RBR_T, NO_ATTR);

	match(KW_T, ELSE);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Selection statement parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<conditional expression> -> <logical OR  expression>
FIRST(<conditional expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void conditional_exp(void) {
	or_expression();
	gen_incode("PLATY: Conditional expression parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<logical OR expression> -> <logical AND expression><logical OR expression’>
FIRST(<logical OR expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void or_expression(void) {
	and_expression();
	or_expression_prime();
}

/*
 Author: Isha Isha, Madhav Sachdeva
<logical AND expression> -> <relational expression><logical AND expression’>
FIRST(<logical AND expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/

void and_expression(void) {
	relational_expression();
	and_expression_prime();
}

/*
 Author: Isha Isha, Madhav Sachdeva
<relational expression> -> <primary a_relational expression><primary a_relational expression’>
| <primary s_relational expression><primary s_relational expression’>
FIRST(<relational expression>) = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
*/
void relational_expression(void) {
	switch (lookahead.code)
	{
	case AVID_T:
    case FPL_T:
	case INL_T:
		a_relational_exp();
		a_relational_exp_prime();
		break;
	case SVID_T:
	case STR_T:
		s_relational_exp();
		s_relational_exp_prime();
		break;

	default:
		syn_printe();
		break;
	}
	gen_incode("PLATY: Relational expression parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary a_relational expression> -> AVID_T | FPL_T | INL_T
FIRST(<primary a_relational expression>) = {AVID_T, FPL_T, INL_T}
*/
void a_relational_exp(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case FPL_T:
		match(FPL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case INL_T:
		match(INL_T, NO_ATTR);
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	case SVID_T:
	case STR_T:
		syn_printe();
		gen_incode("PLATY: Primary a_relational expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary a_relational expression’> == <primary a_relational expression> | <> <primary a_relational expression>
| > <primary a_relational expression> | < <primary a_relational expression>
FIRST(<primary a_relational expression’>) = {REL(OP), REL_OP(NE), REL_OP(GT), REL_OP(LT)}
*/
void a_relational_exp_prime(void) {
	switch (lookahead.code)
	{
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ) {
			match(REL_OP_T, EQ);
			a_relational_exp();
		}
		else if (lookahead.attribute.rel_op == NE) {
			match(REL_OP_T, NE);
			a_relational_exp();
		}
		else if (lookahead.attribute.rel_op == GT) {
			match(REL_OP_T, GT);
			a_relational_exp();
		}
		else if (lookahead.attribute.rel_op == LT) {
			match(REL_OP_T, LT);
			a_relational_exp();
		}
		else {
			syn_printe();
		}
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary s_relational expression> -> <primary string expression>
FIRST(<primary s_relational expression>) = {SVID_T, STR_T}
*/
void s_relational_exp(void) {
	switch (lookahead.code) {
	case SVID_T:
	case STR_T:
		primary_string_exp();
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;
	case AVID_T:
	case FPL_T:
	case INL_T:
		syn_printe();
		gen_incode("PLATY: Primary s_relational expression parsed");
		break;
	default:
		syn_printe();
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary s_relational expression’> == <primary s_relational expression>	| <> <primary s_relational expression>
| > <primary s_relational expression> | < <primary s_relational expression>
FIRST(<primary s_relational expression’>) = {REL(OP), REL_OP(NE), REL_OP(GT), REL_OP(LT)}
*/
void s_relational_exp_prime(void) {
	switch (lookahead.code) {
	case REL_OP_T:
		if (lookahead.attribute.rel_op == EQ) {
			match(REL_OP_T, EQ);
			s_relational_exp();
		}
		else if (lookahead.attribute.rel_op == NE) {
			match(REL_OP_T, NE);
			s_relational_exp();
		}
		else if (lookahead.attribute.rel_op == GT) {
			match(REL_OP_T, GT);
			s_relational_exp();
		}
		else if (lookahead.attribute.rel_op == LT) {
			match(REL_OP_T, LT);
			s_relational_exp();
		}
		else {
			syn_printe();
		}
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<string expression> -> <primary string expression><string expression’>
FIRST(<string expression>) = {SVID_T, STR_T}
*/
void string_exp(void) {
	primary_string_exp();
	string_exp_prime();
	gen_incode("PLATY: String expression parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<string expression’> -> << <primary string expression><string expression’> | ϵ
FIRST(<string expression’>) = {SCC_OP_T(<<), ϵ}
*/
void string_exp_prime(void) {
	switch (lookahead.code)
	{
	case SCC_OP_T:
		match(SCC_OP_T, NO_ATTR);
		primary_string_exp();
		string_exp_prime();
		break;
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<primary string expression> -> SVID_T | STR_T
FIRST(<primary string expression>) = {SVID_T, STR_T}
*/
void primary_string_exp(void) {
	switch (lookahead.code) {
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		break;
	default:
		break;
	}
	gen_incode("PLATY: Primary string expression parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<logical OR expression’> -> .OR.<logical OR expression><logical OR expression’> | ϵ
FIRST(<logical OR expression’>) = {LOG_OP_T(.OR.), ϵ}
*/
void or_expression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		if (lookahead.attribute.log_op == OR) {
			match(LOG_OP_T, OR);
			and_expression();
			or_expression_prime();
			gen_incode("PLATY: Logical OR expression parsed");
			break;
		}
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<logical AND expression’> -> .AND. <relational expression><logical AND expression’>|ϵ
FIRST(<logical AND expression’>) = {LOG_OP_T(.AND.), ϵ }
*/
void and_expression_prime(void) {
	switch (lookahead.code) {
	case LOG_OP_T:
		if (lookahead.attribute.log_op == AND) {
			match(LOG_OP_T, AND);
			relational_expression();
			and_expression_prime();
			gen_incode("PLATY: Logical AND expression parsed");
		}
		break;
	default:
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<iteration statement> -> WHILE <pre-condition> (<conditional expression>) REPEAT { <statements>};
FIRST(<iteration statement>) = {KW_T(WHILE)}
*/
void iteration(void) {
	match(KW_T, WHILE);
	match(KW_T, TRUE);
	match(LPR_T, NO_ATTR);
	conditional_exp();
	match(RPR_T, NO_ATTR);

	match(KW_T, REPEAT);
	match(LBR_T, NO_ATTR);

	statements();
	match(RBR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);

	gen_incode("PLATY: Iteration statement parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<input statement> -> READ (<variable list>);
FIRST(<input statement>) = {KW_T(READ)}
*/

void input(void) {
	match(KW_T, INPUT);
	match(LPR_T, NO_ATTR);
	variable_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Input statement parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<variable list> -> <variable identifier><variable list’>
FIRST(<variable list>) = {FIRST(<variable identifier>)} = {AVID_T, SVID_T}
*/

void variable_list(void) {
	variable_identifier();
	variable_list_prime();
	gen_incode("PLATY: Variable list parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
{FIRST(<variable identifier>)} = {AVID_T, SVID_T}
*/
void variable_identifier(void) {
	switch (lookahead.code) {
	case AVID_T:
		match(AVID_T, NO_ATTR);
		break;
	case SVID_T:
		match(SVID_T, NO_ATTR);
		break;
	default:
		syn_printe();
		break;
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<variable list’> -> <variable list’><variable identifier> | ϵ
FIRST(<variable list>) = {COM_T, ϵ}
*/
void variable_list_prime(void) {
	switch (lookahead.code) {
	case COM_T:
		match(COM_T, NO_ATTR);
		variable_identifier();
		variable_list_prime();
	}
}

/*
 Author: Isha Isha, Madhav Sachdeva
<output statement> -> WRITE(<output list>)
FIRST(<output statement>) = {KW_T(WRITE)}
*/
void output(void) {
	match(KW_T, WRITE);
	match(LPR_T, NO_ATTR);
	output_list();
	match(RPR_T, NO_ATTR);
	match(EOS_T, NO_ATTR);
	gen_incode("PLATY: Output statement parsed");
}

/*
 Author: Isha Isha, Madhav Sachdeva
<output list> -> (variable list) | STR_T | ϵ
FIRST(<output list>) = {AVID_T, SVID_T, STR_T, ϵ}
*/
void output_list(void) {
	switch (lookahead.code) {
	case AVID_T:
	case SVID_T:
		variable_list();
		break;
	case STR_T:
		match(STR_T, NO_ATTR);
		gen_incode("PLATY: Output list (string literal) parsed");
		break;
	default:
		gen_incode("PLATY: Output list (empty) parsed");
		break;
	}
}