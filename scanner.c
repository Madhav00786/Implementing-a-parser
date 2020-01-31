/*
* File name: scanner.c
* Compiler: Visual Studio 2019
* Author: Isha Isha, Madhav Sachdeva
* Course: CST 8152 - Compilers, Lab Section: 021, 023
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose:   It builds the scanner
* Function List: scanner_init, malar_next_token, get_next_state, char_class, aa_func02, aa_func03, aa_func08,
				 aa_func05, aa_func10, aa_func12, iskeyword
*/

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

 /*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern pBuffer str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static pBuffer lex_buf;/*pointer to temporary lexeme buffer*/
static pBuffer sc_buf; /*pointer to input source buffer*/
/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c); /* character class function */
static int get_next_state(int, char); /* state machine function */
static int iskeyword(char* kw_lexeme); /*keywords lookup functuion */


/*Initializes scanner */
int scanner_init(pBuffer psc_buf) {
	if (b_isempty(psc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_rewind(psc_buf);
	b_clear(str_LTBL);
	line = 1;
	sc_buf = psc_buf;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}



/*
* Purpose: This function performs the token recognition.
* Author: Isha Isha, Madhav Sachdeva
* History/Versions: 1 10 November, 2019
* Called functions: b_getc, b_addc, b_mark, get_next_state, b_retract, b_getcoffset,
					b_reset, b_allocate, b_free, b_location
* Parameters: void
* Return Value: Token
* Algorithm:  It reads the lexeme from the input stream one character at a time, and
			  returns a token structure any time it finds a token pattern which matches
			  the lexeme found in the stream of input symbols.
*/
Token malar_next_token(void) {


	Token t = { 0 }; /* token to return after pattern recognition. Set all structure members to 0 */
	unsigned char c; /* input symbol */
	int state = 0; /* initial state of the FSM */
	short lexstart;  /*start offset of a lexeme in the input char buffer (array) */
	short lexend;    /*end   offset of a lexeme in the input char buffer (array)*/

	while (1) { /* endless loop broken by token returns it will generate a warning */

		c = b_getc(sc_buf);

		switch (c) {
		case SEOF:
			t.code = SEOF_T;
			t.attribute.seof = SEOF_EOF;
			return t;

		case '0xFF':
			t.code = SEOF_T;
			t.attribute.seof = SEOF_EOF;
			return t;

		case 255:
			t.code = SEOF_T;
			t.attribute.seof = SEOF_EOF;
			return t;

		case 'EOF':
			t.code = SEOF_T;
			t.attribute.seof = SEOF_EOF;
			return t;

		case '\n':
			line++;
			break;

		case '\t':
			break;

		case ' ':
			break;

		case '\v':
			break;

		case '\r':
			line++;
			break;

		case '\f':
			break;

		case '/':
			t.code = ART_OP_T;
			t.attribute.arr_op = DIV;
			return t;

		case '(':
			t.code = LPR_T;
			return t;

		case ')':
			t.code = RPR_T;
			return t;

		case '{':
			t.code = LBR_T;
			return t;

		case '}':
			t.code = RBR_T;
			return t;

		case ',':
			t.code = COM_T;
			return t;

		case ';':
			t.code = EOS_T;
			return t;

		case '+':
			t.code = ART_OP_T;
			t.attribute.arr_op = PLUS;
			return t;

		case '>':
			t.code = REL_OP_T;
			t.attribute.rel_op = GT;
			return t;

		case '=':
			c = b_getc(sc_buf);
			if (c == '=') {
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.code = ASS_OP_T;
				return t;
			}

		case '-':
			t.code = ART_OP_T;
			t.attribute.arr_op = MINUS;
			return t;

		case '*':
			t.code = ART_OP_T;
			t.attribute.arr_op = MULT;
			return t;


		case '<':
			c = b_getc(sc_buf);
			if (c == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE;
				return t;
			}
			else if (c == '<') {
				t.code = SCC_OP_T;
				return t;
			}
			else {
				b_retract(sc_buf);
				t.attribute.rel_op = LT;
				t.code = REL_OP_T;
				return t;
			}

		case '.':
			b_mark(sc_buf, b_getcoffset(sc_buf));
			c = b_getc(sc_buf);
			if (c == 'A') {
				c = b_getc(sc_buf);
				if (c == 'N') {
					c = b_getc(sc_buf);
					if (c == 'D') {
						c = b_getc(sc_buf);
						if (c == '.') {
							t.code = LOG_OP_T;
							t.attribute.log_op = AND;
							return t;
						}
					}
				}
			}

			else if (c == 'O') {
				c = b_getc(sc_buf);
				if (c == 'R') {
					c = b_getc(sc_buf);
					if (c == '.') {
						t.code = LOG_OP_T;
						t.attribute.log_op = OR;
						return t;
					}
				}
			}

			t.code = ERR_T;
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = SEOF;
			b_reset(sc_buf);
			return t;

		case '!':
			c = b_getc(sc_buf);
			if (c == '!') {
				c = b_getc(sc_buf);
				
				do {
					c = b_getc(sc_buf);
				} while (c != 255 && c != SEOF && c != '\n');
				line++;
				break;
			}
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = '!';
				t.attribute.err_lex[1] = c;
				t.attribute.err_lex[2] = SEOF;
				while (c != 255 && c != SEOF && c != '\n') {
					c = b_getc(sc_buf);
				}
				line++;
				return t;
			}

		default:
			lexstart = b_mark(sc_buf, b_getcoffset(sc_buf) - 1);
			state = get_next_state(state, c);
			while (as_table[state] == NOAS) {
				c = b_getc(sc_buf);
				
				state = get_next_state(state, c);
			}
			if (as_table[state] == ASWR) {
				b_retract(sc_buf);
			}
			lexend = b_getcoffset(sc_buf);
			b_retract(sc_buf);
			lex_buf = b_allocate((lexend - lexstart) + 1, 0, 'f');
			if (!lex_buf) {
				scerrnum = 100;
				t.code = RTE_T;
				strcpy(t.attribute.err_lex, "RUN TIME ERROR: ");
				return t;
			}
			b_reset(sc_buf);
			for (int i = 0; i < (lexend - lexstart); i++) {
				c = b_getc(sc_buf);
				b_addc(lex_buf, c);
			}
			b_addc(lex_buf, '\0');
			t = aa_table[state](b_location(lex_buf));
			b_free(lex_buf);
			return t;
		}

	}// end while()
}




int get_next_state(int state, char c)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];

#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif
	/*
	The assert(int test) macro can be used to add run-time diagnostic to programs
	and to "defend" from producing unexpected results.
	assert() is a macro that expands to an if statement;
	if test evaluates to false (zero) , assert aborts the program
	(by calling abort()) and sends the following message on stderr:

	Assertion failed: test, file filename, line linenum

	The filename and linenum listed in the message are the source file name
	and line number where the assert macro appears.
	If you place the #define NDEBUG directive ("no debugging")
	in the source code before the #include <assert.h> directive,
	the effect is to comment out the assert statement.
	*/
	assert(next != IS);

	/*
	The other way to include diagnostics in a program is to use
	conditional preprocessing as shown bellow. It allows the programmer
	to send more details describing the run-time problem.
	Once the program is tested thoroughly #define DEBUG is commented out
	or #undef DEBUF is used - see the top of the file.
	*/
#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	return next;
}

/*
* Purpose: This function returns the column number
* Author: Madhav Sachdeva
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char c
* Return Value: int
* Algorithm: compares the input character and returns column number accordingly
*/
int char_class(char c)
{
	int val; /*variable for column number*/

	/* Column 1 of transition table [a-z A-Z] */
	if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
		val = 0;
	}
	/* Column 2 of transition table 0 */
	else if (c == '0') {
		val = 1;
	}
	/* Column 3 of transition table [1-9] */
	else if (c >= '1' && c <= '9') {
		val = 2;
	}
	/* Column 4 of transition table . */
	else if (c == '.') {
		val = 3;
	}
	/* Column 5 of transition table @ */
	else if (c == '@') {
		val = 4;
	}
	/* Column 7 of transition table " */
	else if (c == '"') {
		val = 6;
	}
	/* Column 8 of transition table SEOF */
	else if (c == SEOF) {
		val = 7;
	}
	else {
		val = 5;
	}
	return val;
}

/*
* Purpose: Accepting state function for arithmentic variable identifier and keywords
* Author: Isha Isha
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func02(char lexeme[]) {

	/*WHEN CALLED THE FUNCTION MUST
	1. CHECK IF THE LEXEME IS A KEYWORD.
	IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
	FOR THE KEYWORD.THE ATTRIBUTE CODE FOR THE KEYWORD
	IS ITS INDEX IN THE KEYWORD LOOKUP TABLE(kw_table in table.h).
	IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.

	2. SET a AVID TOKEN.
	IF THE lexeme IS LONGER than VID_LEN(see token.h) CHARACTERS,
	ONLY FIRST VID_LEN CHARACTERS ARE STORED
	INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
	ADD \0 AT THE END TO MAKE A C - type STRING.*/

	Token t;
	int number = iskeyword(lexeme);
	int i;

	if (number != -1) {
		t.code = KW_T;
		t.attribute.kwt_idx = number;
		return t;
	}
	else {
		t.code = AVID_T;
	}

	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN; ++i) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {
		strcpy(t.attribute.vid_lex, lexeme);
	}
	return t;
}

/*
* Purpose: Accepting state function for string variable identifier
* Author: Madhav Sachdeva
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func03(char lexeme[]) {

	/*	1. SET a SVID TOKEN.
		IF THE lexeme IS LONGER than VID_LEN characters,
		ONLY FIRST VID_LEN - 1 CHARACTERS ARE STORED
		INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
		AND THEN THE @ CHARACTER IS APPENDED TO THE NAME.
		ADD \0 AT THE END TO MAKE A C - type STRING.*/

	Token t;
	int i = 0;
	int j = 0;

	t.code = SVID_T;
	if (strlen(lexeme) > VID_LEN) {
		for (i = 0; i < VID_LEN - 1; i++) {
			t.attribute.vid_lex[i] = lexeme[i];
		}
		int length = strlen(lexeme);
		t.attribute.vid_lex[i] = lexeme[length - 1];
		t.attribute.vid_lex[i + 2] = '@';
		t.attribute.vid_lex[i + 1] = SEOF;
		return t;
	}
	else {
		for (j = 0; j <= strlen(lexeme); j++) {
			t.attribute.vid_lex[j] = lexeme[j];
		}
		t.attribute.vid_lex[j++] = '@';
		t.attribute.vid_lex[j] = SEOF;
		return t;
	}
}


/*
* Purpose: Accepting state function for floating-point literal
* Author: Madhav Sachdeva
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func08(char lexeme[]) {
	/*THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
		WHICH IS THE ATTRIBUTE FOR THE TOKEN.
		THE VALUE MUST BE IN THE SAME RANGE AS the value of 4 - byte float in C.
		IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/

	Token t;
	double value;

	value = strtod(lexeme, NULL);
	if ((value > 0 && (value > FLT_MAX || value < FLT_MIN))) {
		return aa_func11(lexeme);
	}
	t.code = FPL_T;
	t.attribute.flt_value = (float)value;
	return t;
}


/*
* Purpose: Accepting state function for integer literal
* Author: Madhav Sachdeva
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func05(char lexeme[]) {

	/*THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
		TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
		THE VALUE MUST BE IN THE SAME RANGE AS the value of 2 - byte integer in C.
		IN CASE OF ERROR(OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
		THE ERROR TOKEN ATTRIBUTE IS  lexeme.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/
	Token t;
	long value;

	value = atol(lexeme);
	if ((value != 0 && lexeme[0] == '0') || (value > SHRT_MAX || value < SHRT_MIN)) {
		return aa_func11(lexeme);
	}
	t.code = INL_T;
	t.attribute.int_value = (int)value;
	return t;
}


/*
* Purpose: Accepting state function for string literal
* Author: Isha Isha
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func10(char lexeme[]) {

	/*	THE FUNCTION MUST STORE THE lexeme PARAMETER CONTENT INTO THE STRING LITERAL TABLE(str_LTBL)
			FIRST THE ATTRIBUTE FOR THE TOKEN MUST BE SET.
			THE ATTRIBUTE OF THE STRING TOKEN IS THE OFFSET FROM
			THE BEGINNING OF THE str_LTBL char buffer TO THE LOCATION
			WHERE THE FIRST CHAR OF THE lexeme CONTENT WILL BE ADDED TO THE BUFFER.
			USING b_addc(..)COPY THE lexeme content INTO str_LTBL.
			THE OPENING AND CLOSING " MUST BE IGNORED DURING THE COPING PROCESS.
			ADD '\0' AT THE END MAKE THE STRING C - type string
			IF THE STING lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
			SET THE STRING TOKEN CODE.*/

	Token t;
	int i;
	int length;
	length = strlen(lexeme);
	t.attribute.str_offset = b_limit(str_LTBL);
	for (i = 1; i <= length - 2; i++) {
		b_addc(str_LTBL, lexeme[i]);
		if (lexeme[i] == '\n') {
			line++;
		}
	}
	b_addc(str_LTBL, SEOF);
	t.code = STR_T;

	return t;
}


/*
* Purpose: Accepting state function for error token
* Author: Isha Isha
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char lexeme[]
* Return Value: Token
* Algorithm:
*/
Token aa_func11(char lexeme[]) {
	/*THE FUNCTION SETS THE ERROR TOKEN.lexeme[] CONTAINS THE ERROR
		THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme CONTENT ITSELF
		AND IT MUST BE STORED in err_lex.IF THE ERROR lexeme IS LONGER
		than ERR_LEN characters, ONLY THE FIRST ERR_LEN - 3 characters ARE
		STORED IN err_lex.THEN THREE DOTS ... ARE ADDED TO THE END OF THE
		err_lex C - type string.
		IF THE ERROR lexeme CONTAINS line terminators THE line COUNTER MUST BE INCTREMENTED.
		BEFORE RETURNING THE FUNCTION MUST SET THE APROPRIATE TOKEN CODE*/

	Token t;
	unsigned int i = 0;
	t.code = ERR_T;
	if (strlen(lexeme) > ERR_LEN) {
		for (i = 0; i < strlen(lexeme); i++) {
			if (i < ERR_LEN - 3) {
				t.attribute.err_lex[i] = lexeme[i];
			}
			if (lexeme[i] == '\n') {
				line++;
			}
		}
		t.attribute.err_lex[ERR_LEN - 3] = '.';
		t.attribute.err_lex[ERR_LEN - 2] = '.';
		t.attribute.err_lex[ERR_LEN - 1] = '.';
		t.attribute.err_lex[ERR_LEN] = '\0';
		return t;
	}
	for (i = 0; i < strlen(lexeme); i++) {
		t.attribute.err_lex[i] = lexeme[i];
		if (lexeme[i] == '\n') {
			line++;
		}

	}
	t.attribute.err_lex[i] = SEOF;
	return t;


}

/*
* Purpose: This functions checks if the lexeme is a keyword
* Author: Isha Isha
* History/Versions: 1 10 November, 2019
* Called functions:
* Parameters: char* kw_lexeme
* Return Value: int
* Algorithm:
*/
int iskeyword(char* kw_lexeme) {
	int i;
	if (kw_lexeme == NULL) {
		return -1;
	}

	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_table[i], kw_lexeme) == 0) {
			return i;
		}
	}
	return -1;
}