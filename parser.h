/*
* File name: parser.h
* Compiler: Visual Studio 2019
* Author: Isha Isha, Madhav Sachdeva
* Course: CST 8152 - Compilers, Lab Section: 021, 023
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose:  It contains all the macro definitions and function declarations for parser.c
* Function List:
*/

#include <stdlib.h>
#include "buffer.h"
#include "token.h"

#define NO_ATTR -1
#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define INPUT 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE 9

#define KWT_SIZE 10

static Token lookahead;
int synerrno;
extern Buffer* str_LTBL;
extern Token malar_next_token();
extern char* kw_table[KWT_SIZE];
extern int line;


void parser(void); /* parser function */
void match(int, int);  /* match function */
void syn_eh(int); /* syn_eh function */
void syn_printe(); /* syn_printe function */
void gen_incode(char*); /* gen_incode function */
void program(void);  /* program function */
void opt_statements(void); /* opt statement function */
void statements(void);  /* statements function */
void statement(void); /* statement function */
void statements_prime(void); /* statement prime function */
void assignment(void); /* assignment statement function */
void assignment_exp(void); /* assignment expression function */
void arithmetic_exp(void); /* arithmetic expression function */
void unary_arithmetic_exp(void); /* unary arithmetic expression function */
void additive_arithmetic_exp(void); /* additive arithmetic expression function */
void additive_arithmetic_exp_prime(void); /* additive arithmetic expression prime function */
void multiplicative_arithmetic_exp(void); /* multiplicative arithmetic expression function */
void multiplicative_arithmetic_exp_prime(void); /* multiplicative arithmetic expression prime function */
void primary_arithmetic_exp(void); /* primary arithmetic expression function */
void selection(void); /* selection statement function */
void conditional_exp(void); /* conditional expression function */
void or_expression(void); /* logical or expression function */
void and_expression(void); /* logical and expression function */
void relational_expression(void); /* relational expression function */
void a_relational_exp(void); /*primary a relational expression function */
void a_relational_exp_prime(void); /* primary a relational expression prime function */
void s_relational_exp(void); /* primary s relational expression function */
void s_relational_exp_prime(void); /* primary s relational expression prime function */
void string_exp(void); /* string expression function */
void string_exp_prime(void); /* string expresion prime function */
void primary_string_exp(void); /* primary string expression function */
void or_expression_prime(void); /* logical or expression prime function */
void and_expression_prime(void); /* logical and expression prime function */
void iteration(void); /* iteration statement function */
void input(void); /* input statement function */
void variable_list(void); /* variable list function */
void variable_identifier(void); /* variable identifier function */
void variable_list_prime(void); /* variable identifier prime function */
void output(void); /* output statement function */
void output_list(void); /* output list function */
