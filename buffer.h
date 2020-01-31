/*
* File name: buffer.h
* Compiler: Visual Studio 2019
* Author: Isha Isha
* Course: CST 8152 - Compilers, Lab Section: 021
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose: To declare all the constant definitions, data type and function declarations
* Function List: b_allocate, b_addc, b_clear, b_free, b_isfull, b_limit, b_capacity,
				 b_mark, b_mode, b_incfactor, b_load, b_isempty, b_getc, b_eob, b_print,
				 b_compact, b_rflag, b_retract, b_reset, b_getcoffset, b_rewind, b_location

*/
#pragma once
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
#define RT_FAIL_1 (-1)         /* operation failure return value 1 */
#define RT_FAIL_2 (-2)         /* operation failure return value 2 */
#define LOAD_FAIL (-2)         /* load fail return value */

#define DEFAULT_INIT_CAPACITY 200   /* default initial buffer capacity */
#define DEFAULT_INC_FACTOR 15       /* default increment factor */



/* You should add your own constant definitions here */
#define ADD_MODE 1            /*Additive mode*/
#define FIXED_MODE 0          /*Fixed mode*/
#define MUL_MODE -1           /*Multiplicative mode*/
#define ZERO 0                /*Constant for zero*/
#define MAX_ALLOWED_VALUE SHRT_MAX-1  /*Maximum allowed positive value*/
#define MIN_ADDFACTOR 1       /*Minimum value of additive inc factor*/
#define MAX_ADDFACTOR 255     /*Maximum value of additive inc factor*/
#define MIN_MULFACTOR 1       /*Minimum value of multiplicative inc factor*/
#define MAX_MULFACTOR 100     /*Maximum value of multiplicative inc factor*/
#define TRUE 1                /*Constant definition*/
#define FALSE 0               /*Constant definition*/
#define SET_TO_ZERO 0         /*Constant definition to set something to 0*/
#define ERROR_INCFACTOR 0x100 /*operation failure in case of increment factor return value*/

/* Add your bit-masks constant definitions here */
#define DEFAULT_FLAGS  0xFFFC  /*1111 1111 1111 1100*/
#define SET_EOB        0x0002  /*0000 0000 0000 0010*/
#define RESET_EOB      0xFFFD  /*1111 1111 1111 1101*/
#define CHECK_EOB      0x0002  /*0000 0000 0000 0010*/
#define SET_R_FLAG     0x0001  /*0000 0000 0000 0001*/
#define RESET_R_FLAG   0xFFFE  /*1111 1111 1111 1110*/
#define CHECK_R_FLAG   0x0001  /*0000 0000 0000 0001*/

/* user data type declarations */
typedef struct BufferDescriptor {
	char* cb_head;   /* pointer to the beginning of character array (character buffer) */
	short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
	short addc_offset;  /* the offset (in chars) to the add-character location */
	short getc_offset;  /* the offset (in chars) to the get-character location */
	short markc_offset; /* the offset (in chars) to the mark location */
	char  inc_factor; /* character array increment factor */
	char  mode;       /* operational mode indicator*/
	unsigned short flags;     /* contains character array reallocation flag and end-of-buffer flag */
} Buffer, * pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */

Buffer* b_allocate(short, char, char);
pBuffer b_addc(pBuffer const, char);
int b_clear(Buffer* const);
void b_free(Buffer* const);
int b_isfull(Buffer* const);
short b_limit(Buffer* const);
short b_capacity(Buffer* const);
short b_mark(pBuffer const, short);
int b_mode(Buffer* const);
size_t b_incfactor(Buffer* const);
int b_load(FILE* const, Buffer* const);
int b_isempty(Buffer* const);
char b_getc(Buffer* const);
int b_eob(Buffer* const);
int b_print(Buffer* const, char);
Buffer* b_compact(Buffer* const, char);
char b_rflag(Buffer* const);
short b_retract(Buffer* const);
short b_reset(Buffer* const);
short b_getcoffset(Buffer* const);
int b_rewind(Buffer* const);
char* b_location(Buffer* const);
/*
Do not include the function header comments here.
Place them in the buffer.c file
*/

#endif

