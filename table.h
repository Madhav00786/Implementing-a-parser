/*
* File name: table.h
* Compiler: Visual Studio 2019
* Author: Isha Isha, Madhav Sachdeva
* Course: CST 8152 - Compilers, Lab Section: 021, 023
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose:  It contains transition table declarations necessary for the scanner
* Function List:
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or one of 255,0xFF,EOF
 */

#define SEOF '\0'

 /*  Special case tokens processed separately one by one
  *  in the token-driven part of the scanner
  *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' , ';',
  *  white space
  *  !!comment , ',' , ';' , '-' , '+' , '*' , '/', << ,
  *  .AND., .OR. , SEOF,
  */

#define ES  11 /* Error state  with no retract */
#define ER  12 /* Error state  with retract */
#define IS -1    /* Inavalid state */

  /* State transition table definition */

#define TABLE_COLUMNS 8
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/* State 0 */  {1, 6, 4, ES, ES, ES, 9, ES},
	/* State 1 */  {1, 1, 1, 2, 3, 2, ES, ES},
	/* State 2 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 3 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 4 */  {ES, 4, 4, 7, 5, 5, ES, ES},
	/* State 5 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 6 */  {ES, 6, ES, 7, ES, 5, ES, ES},
	/* State 7 */  {8, 7, 7, 8, 8, 8, ES, ES},
	/* State 8 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 9 */  {9, 9, 9, 9, 9, 9, 10, ES},
	/* State 10 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 11 */  {IS, IS, IS, IS, IS, IS, IS, IS},
	/* State 12 */  {IS, IS, IS, IS, IS, IS, IS, IS},
};


/* Accepting state table definition */

#define ASWR     0 /* accepting state with retract */
#define ASNR      1  /* accepting state with no retract */
#define NOAS      2  /* not accepting state */

int as_table[] = { NOAS,
				   NOAS,
				   ASWR,
				   ASNR,
				   NOAS,
				   ASWR,
				   NOAS,
				   NOAS,
				   ASWR,
				   NOAS,
				   ASNR,
				   ASNR,
				   ASWR,
};

/* Accepting action function declarations */

Token aa_func02(char* lexeme);
Token aa_func03(char* lexeme);
Token aa_func05(char* lexeme);
Token aa_func08(char* lexeme);
Token aa_func10(char* lexeme);
Token aa_func11(char* lexeme);

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char* lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {  /* State 0 */ NULL,
/* State 1 */ NULL,
/* State 2 */ aa_func02,
/* State 3 */ aa_func03,
/* State 4 */ NULL,
/* State 5 */ aa_func05,
/* State 6 */ NULL,
/* State 7 */ NULL,
/* State 8 */ aa_func08,
/* State 9 */ NULL,
/* State 10 */ aa_func10,
/* State 11 */ aa_func11,
/* State 12 */ aa_func11,

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  10

char* kw_table[KWT_SIZE] =
{
"ELSE",
"FALSE",
"IF",
"PLATYPUS",
"READ",
"REPEAT",
"THEN",
"TRUE",
"WHILE",
"WRITE"
};

#endif

