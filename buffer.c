/*
* File name: buffer.c
* Compiler: Visual Studio 2019
* Author: Isha Isha, 040912862
* Course: CST 8152 - Compilers, Lab Section: 021
* Assignment: 3
* Date: 5 December,2019
* Professor: Sv. Ranev
* Purpose: To implement a buffer that can operate in three different modes: fixed,
		additive self-incrementing and multiplicative self-incrementing buffer.
* Function List: b_allocate, b_addc, b_clear, b_free, b_isfull, b_limit, b_capacity,
				b_mark, b_mode, b_incfactor, b_load, b_isempty, b_getc, b_eob, b_print,
				b_compact, b_rflag, b_retract, b_reset, b_getcoffset, b_rewind, b_location

*/
#include <stdlib.h>
#include <limits.h>
#include "buffer.h"

/*
* Purpose: This function creates a new buffer in memory.
* Author: Isha Isha
* History/Versions: 1 24 Sep, 2019
* Called functions: calloc(), malloc(), free()
* Parameters: short init_capacity, char inc_factor, char o_mode
* Return Value: pBuffer, NULL
* Algorithm: Allocate memory for one Buffer structure.
			Allocate memory for one dynamic character buffer (character array)
			Set the buffer structure's operational mode indicator mode and the inc_factor.
*/

Buffer* b_allocate(short init_capacity, char inc_factor, char o_mode) {
	/*pointer to the buffer*/
	pBuffer pBD;

	/*Check for the value of init_capacity*/
	if (init_capacity < ZERO || init_capacity > MAX_ALLOWED_VALUE) {
		return NULL;
	}

	/*Allocate memory for one buffer structure*/
	pBD = (Buffer*)calloc(1, sizeof(Buffer));

	/*Check for a run time error*/
	if (pBD == NULL) {
		return NULL;
	}


	/*If the init_capacity is 0*/
	if (init_capacity == ZERO) {
		/*set the init_capacity to 200*/
		init_capacity = DEFAULT_INIT_CAPACITY;

		/*checks the o_mode and depending on that sets the inc_factor*/
		if (o_mode == 'a' || o_mode == 'm') {
			inc_factor = DEFAULT_INC_FACTOR;

		}
		else {
			inc_factor = ZERO;
		}

	}

	/*Allocate memory for one dynamic buffer character*/
	pBD->cb_head = (char*)malloc(sizeof(char) * init_capacity);

	/*if the alocation is not successful free the pBD and return NULL*/
	if (pBD->cb_head == NULL) {
		free(pBD);
		return NULL;
	}


	/*sets the Buffer structure operational mode indicator mode and inc factor*/
	if (o_mode == 'f') {
		pBD->mode = FIXED_MODE;
		pBD->inc_factor = ZERO;

	}
	else if (inc_factor == ZERO && init_capacity != ZERO) {
		pBD->mode = FIXED_MODE;
		pBD->inc_factor = ZERO;
	}
	else if (o_mode == 'a' && (((unsigned char)inc_factor >= MIN_ADDFACTOR) && ((unsigned char)inc_factor <= MAX_ADDFACTOR))) {
		pBD->mode = ADD_MODE;
		pBD->inc_factor = inc_factor;

	}
	else if (o_mode == 'm' && (((unsigned char)inc_factor >= MIN_MULFACTOR) && ((unsigned char)inc_factor <= MAX_MULFACTOR))) {
		pBD->mode = MUL_MODE;
		pBD->inc_factor = inc_factor;
	}
	/*if the allocation is not successful free the pBD and return NULL*/
	else {
		free(pBD);
		return NULL;
	}

	/*sets the buffer capacity equal to the init_capacity*/
	pBD->capacity = init_capacity;
	/*sets the flags field to the default value*/
	pBD->flags = DEFAULT_FLAGS;
	return pBD;
}

/*
* Purpose: Using a bitwise operation the function resets the flags field r_flag bit
		to 0 and tries to add the character symbol to the character array of the
		given buffer pointed by pBD.
* Author: Isha Isha
* History/Versions: 1 24 Sep, 2019
* Called functions: realloc()
* Parameters: pBuffer const pBD, char symbol
* Return Value: pBuffer, NULL
* Algorithm: Check if the buffer is operational and not full then the symbol can be stored
			in the character buffer. Therefore, adds the character to the character buffer.
			If the character buffer is full, resize the buffer
			Depending on different operational modes the function changes the current capacity
			in different ways.

*/
pBuffer b_addc(pBuffer const pBD, char symbol) {

	/*variable for the new capacity*/
	short newCapacity = 0;
	/*variable for available space and the increment*/
	short availableSpace, newIncrement;
	/*variable to store the old pointer*/
	char* oldCapacity;
	/*variable for the temporary pointer*/
	char* temporaryCapacity;

	/*reset the R_FLAG bit to 0*/
	pBD->flags = pBD->flags & RESET_R_FLAG;

	/*if the buffer is NULL return NULL*/
	if (pBD == NULL) {
		return NULL;
	}

	/*check if the buffer is full*/
	if (pBD->addc_offset == pBD->capacity) {

		/*if it's full and the mode is FIXED_MODE*/
		if (pBD->mode == FIXED_MODE) {
			return NULL;
		}

		/*if the mode is ADD_MODE*/
		if (pBD->mode == ADD_MODE) {

			/*increase the current capacity to a new capacity by adding inc_factor to capacity*/
			newCapacity = pBD->capacity + (unsigned char)pBD->inc_factor;

			if (newCapacity > ZERO && newCapacity < MAX_ALLOWED_VALUE) {
				//check for the newCapacity
			}
			/*if the newCapacity is greater than MAX_ALLOWED_VALUE*/
			else if (newCapacity > MAX_ALLOWED_VALUE) {
				newCapacity = MAX_ALLOWED_VALUE;
			}

			else {
				return NULL;
			}
		}
		/*if the mode is MUL_MODE*/
		else if (pBD->mode == MUL_MODE) {

			/*if the capacity is equal to the MAX_ALLOWED_VALUE*/
			if (pBD->capacity == MAX_ALLOWED_VALUE) {
				return NULL;
			}

			availableSpace = MAX_ALLOWED_VALUE - pBD->capacity;
			newIncrement = (short)(availableSpace * ((long)pBD->inc_factor) / 100);
			newCapacity = pBD->capacity + newIncrement;

			/*if the new capacity is zero*/
			if (newIncrement == ZERO && pBD->capacity < MAX_ALLOWED_VALUE) {
				/*put newCapacity equal to MAX_ALLOWED_VALUE*/
				newCapacity = MAX_ALLOWED_VALUE;
			}
			/*otherwise add the increment to the capacity and put it equal to newCapacity*/
			else {
				newCapacity = pBD->capacity + newIncrement;
			}
		}
		/*if none of the previous mode is selected return NULL*/
		else {
			return NULL;
		}
		/*oldCapacity stores the pointer*/
		oldCapacity = pBD->cb_head;
		/*allocating memory for temporaryCapacity using realloc*/
		temporaryCapacity = (char*)realloc(pBD->cb_head, sizeof(char) * (unsigned short)newCapacity);
		/*if temporaryCapacity is NULL*/
		if (temporaryCapacity == NULL) {
			return NULL;
		}

		/*if not, assign the pointer to the temporaryCapacity*/
		pBD->cb_head = temporaryCapacity;
		pBD->capacity = newCapacity;
		/*if oldCapacity is not equal to temporaryCapacity then only set the R_FLAG bit*/
		if (oldCapacity != temporaryCapacity) {
			pBD->flags = pBD->flags | SET_R_FLAG;
		}

	}

	/*add the character symbol to the buffer content and increment the addc_offset*/
	pBD->cb_head[pBD->addc_offset++] = symbol;
	/*return the pointer*/
	return pBD;
}

/*
* Purpose: The function retains the memory space currently allocated to the buffer, but
		re-initializes all appropriate data members of the given Buffer structure
* Author: Isha Isha
* History/Versions: 1 24 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: TRUE(1), RT_FAIL_1(-1)
* Algorithm: Reset the buffer

*/
int b_clear(Buffer* const pBD) {

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*re-initializing the data members of the buffer structure*/
	pBD->addc_offset = SET_TO_ZERO;
	pBD->getc_offset = SET_TO_ZERO;
	pBD->markc_offset = SET_TO_ZERO;
	pBD->flags = pBD->flags & RESET_EOB;
	pBD->flags = pBD->flags & RESET_R_FLAG;

	/*return TRUE on success*/
	return TRUE;

}

/*
* Purpose: The function de-allocates (frees) the memory occupied by the character
		buffer and the Buffer structure.
* Author: Isha Isha
* History/Versions: 1 24 Sep, 2019
* Called functions: free()
* Parameters: Buffer* const pBD
* Return Value: NULL
* Algorithm: Free the memory occupied by the character buffer and the Buffer structure.
*/
void b_free(Buffer* const pBD) {

	/*in case of run-time error return NULL*/
	if (pBD == NULL) {
		return NULL;
	}

	/*de-allocates the memory occupied by the character buffer and buffer structure*/
	free(pBD->cb_head);
	free(pBD);
}

/*
* Purpose: This function tells if the character buffer is full
* Author: Isha Isha
* History/Versions: 1 24 Sep, 2019
* Called functions: b_capacity()
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), TRUE(1), FALSE(0)
* Algorithm: Compare the addc_offset with the capacity of the character buffer
*/
int b_isfull(Buffer* const pBD) {

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*compares addc_offset to the capacity of the buffer to see if it's full*/
	if (pBD->addc_offset = b_capacity(pBD)) {

		/*return TRUE if it is full*/
		return TRUE;
	}

	/*otherwise return FALSE*/
	return FALSE;
}

/*
* Purpose: The function returns the current limit of the character buffer
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), addc_offset(short)
* Algorithm:
*/
short b_limit(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	/*return the value of addc_offset*/
	return pBD->addc_offset;
}

/*
* Purpose: The function returns the current capacity of the character buffer
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), capacity(short)
* Algorithm:
*/
short b_capacity(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*return the capacity of the buffer*/
	return pBD->capacity;
}

/*
* Purpose: The function sets markc_offset to mark
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: pBuffer const pBD, short mark
* Return Value: RT_FAIL_1(-1), markc_offset(short)
* Algorithm:
*/
short b_mark(pBuffer const pBD, short mark) {

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*check for the value mark and see if it's within the range*/
	if (mark < ZERO || mark > pBD->addc_offset) {
		/*if not return RT_FAIL_1*/
		return RT_FAIL_1;
	}

	/*sets markc_offset to mark*/
	pBD->markc_offset = mark;
	/*return the currently set markc_offset*/
	return pBD->markc_offset;
}

/*
* Purpose: The function returns the value of mode to the calling function
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_2(-2), mode(int)
* Algorithm:
*/
int b_mode(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_2*/
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	/*return the mode of the buffer*/
	return pBD->mode;
}

/*
* Purpose: The function returns the non-negative value of inc_factor to the calling function
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: ERROR_INCFACTOR(0X100), inc_factor(size_t)
* Algorithm:
*/
size_t b_incfactor(Buffer* const pBD) {
	/*in case of run-time error return ERROR-INCFACTOR*/
	if (pBD == NULL) {
		return ERROR_INCFACTOR;
	}

	/*return the non-negative value of inc_factor*/
	return (size_t)(unsigned char)pBD->inc_factor;
}

/*
* Purpose: The function loads (reads) an open input file specified by fi into a buffer specified by pBD
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: feof(), fgetc(), ungetc(), b_addc()
* Parameters: FILE* const fi, Buffer* const pBD
* Return Value: RT_FAIL_1(-1), LOAD_FAIL(-2), num_char(int)
* Algorithm: Use fgetc(fi) to read one character at a time and b_addc()
			to add the character to the buffer.
			If the character cannot be added use ungetc() and return a value
			Repeat this until end-of-file is detected.
			In case of run-time error return -1.
			Return the number of characters added if the load function is successful
*/
int b_load(FILE* const fi, Buffer* const pBD) {

	/*variable to store the buffer character*/
	char character_buffer;
	/*variable to store the number of characters*/
	int num_char = 0;

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL || fi == NULL) {
		return RT_FAIL_1;
	}

	/*loop to go through the file*/
	while (TRUE) {
		/*stores the character in the variable (one at a time)*/
		character_buffer = (char)fgetc(fi);

		/*if the end of file is reached break the loop*/
		if (feof(fi)) {
			break;
		}

		/*if the character cannot be added*/
		if (b_addc(pBD, character_buffer) == NULL) {

			/*return the character to the file stream*/
			ungetc(character_buffer, fi);
			/*return LOAD_FAIL*/
			return LOAD_FAIL;
		}

		/*counts the number of characters added to the buffer*/
		num_char++;

	}
	/*returns the number*/
	return num_char;

}

/*
* Purpose: The function checks if the buffer is empty.
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), TRUE(1), FALSE(0)
* Algorithm:
*/
int b_isempty(Buffer* const pBD) {

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*if addc_offset is zero*/
	if (pBD->addc_offset == ZERO) {
		/*return TRUE*/
		return TRUE;
	}
	/*otherwise return FALSE*/
	return FALSE;
}

/*
* Purpose: This function is used to read the buffer
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_2(-2), ZERO(0), cb_head[](char)
* Algorithm:
*/
char b_getc(Buffer* const pBD) {

	/*in case of run-time error return RT_FAIL_2*/
	if (pBD == NULL) {
		return RT_FAIL_2;
	}

	/*if getc_offset is equal to addc_offset*/
	if (pBD->getc_offset == pBD->addc_offset) {
		/*sets the flags field eob bit to 1 and return 0*/
		pBD->flags = pBD->flags | SET_EOB;
		return ZERO;
	}
	/*otherwise set the eob bit to 0*/
	else {
		pBD->flags = pBD->flags & RESET_EOB;
	}

	/*increments the getc_offset 1 and return the character located at getc_offset*/
	return pBD->cb_head[pBD->getc_offset++];
}

/*
* Purpose: The function returns the value of the flags field determined only by the eob bit
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), int
* Algorithm:
*/
int b_eob(Buffer* const pBD) {

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return pBD->flags & CHECK_EOB;
}

/*
* Purpose:  the function prints character by character the contents of the
		character buffer to the standard output
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: b_eob(), b_getc(), printf()
* Parameters: Buffer* const pBD, char nl
* Return Value: RT_FAIL_1(-1), num_char(int)
* Algorithm: While the end of file is not there keep looping and print the content
			of the buffer.
			After the loop ends check if nl is 0 and if it is not print a new line character
			Return the number of characters printed.
			Check for the run time errors as well.
*/
int b_print(Buffer* const pBD, char nl) {
	/*variable to store number of characters*/
	int num_char = 0;
	/*variable for the character*/
	char bufferCharacter;
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL || pBD->cb_head == NULL) {
		return RT_FAIL_1;
	}
	/*keep looping to print the file content*/
	while (TRUE)
	{
		/*bufferCharacter stores the character*/
		bufferCharacter = b_getc(pBD);

		/*if end of file is reached break the loop*/
		if (b_eob(pBD))
		{
			break;
		}

		/*print the characters*/
		printf("%c", bufferCharacter);
		/*increment number of characters*/
		num_char++;
	}

	/*check if nl is 0*/
	if (nl != '0') {
		printf("\n");
	}
	/*return number of characters printed*/
	return num_char;

}

/*
* Purpose: For all operational modes of the buffer the function shrinks
		(or in some cases may expand) the buffer to a new capacity
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: realloc()
* Parameters: Buffer* const pBD, char symbol
* Return Value: NULL, Buffer*
* Algorithm:
*/
Buffer* b_compact(Buffer* const pBD, char symbol) {
	/*variable for the new capacity*/
	short newCapacity;
	/*pointer to store the old cb_head*/
	char* oldCapacity;
	/*pointer to store the new cb_head*/
	char* temporaryCapacity;
	/*check for a run-time error*/
	if (pBD == NULL || pBD->cb_head == NULL) {
		return NULL;
	}

	/*the newCapacity is addc_offset+1*/
	newCapacity = pBD->addc_offset + 1;

	/*if newCapacity is negative or equal to 0*/
	if (newCapacity <= ZERO) {
		return NULL;
	}

	/*storing the cb_head in oldCapacity*/
	oldCapacity = pBD->cb_head;
	/*allocate memory for new pointer*/
	temporaryCapacity = (char*)realloc(pBD->cb_head, sizeof(char) * newCapacity);

	/*if allocation fails*/
	if (temporaryCapacity == NULL) {
		return NULL;
	}
	/*assigning temporaryCapacity to cb_head*/
	pBD->cb_head = temporaryCapacity;
	/*setting the buffer's capacity to newCapacity*/
	pBD->capacity = newCapacity;
	/*adds the character symbol to the buffer and increments the addc_offset)*/
	pBD->cb_head[pBD->addc_offset++] = symbol;
	/*in case there is a change in the cb_head, set the R_FLAG bit to 1*/
	if (oldCapacity != temporaryCapacity) {
		pBD->flags = pBD->flags | SET_R_FLAG;
	}
	/*return the updated pointer*/
	return pBD;

}

/*
* Purpose: The function returns the value of the flags field determined only by the r_flag bit
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), char
* Algorithm:
*/
char b_rflag(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}
	return pBD->flags & CHECK_R_FLAG;
}

/*
* Purpose: The function decrements getc_offset by 1
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), getc_offset(short)
* Algorithm:
*/
short b_retract(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*check if getc_offset is greater than zero*/
	if (pBD->getc_offset > ZERO) {
		/*if it is decrement it by 1*/
		pBD->getc_offset--;
	}

	return pBD->getc_offset;
}

/*
* Purpose: The function sets getc_offset to the value of the current markc_offset
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), getc_offset(short)
* Algorithm:
*/
short b_reset(Buffer* const pBD) {

	/*checking for run-time error and if there's one return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	if (pBD->markc_offset < ZERO || pBD->markc_offset > pBD->capacity) {
		return RT_FAIL_1;
	}

	/*sets the getc_offset to markc_offset*/
	pBD->getc_offset = pBD->markc_offset;

	return pBD->getc_offset;
}

/*
* Purpose: The function returns getc_offset to the calling function
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), getc_offset(short)
* Algorithm:
*/
short b_getcoffset(Buffer* const pBD) {
	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	return pBD->getc_offset;
}

/*
* Purpose: The function set the getc_offset and markc_offset to 0
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: RT_FAIL_1(-1), ZERO(0)
* Algorithm:
*/
int b_rewind(Buffer* const pBD) {

	/*setting getc_offset and markc_offset to 0*/
	pBD->getc_offset = SET_TO_ZERO;
	pBD->markc_offset = SET_TO_ZERO;

	/*in case of run-time error return RT_FAIL_1*/
	if (pBD == NULL) {
		return RT_FAIL_1;
	}

	/*otherwise return 0*/
	return ZERO;
}

/*
* Purpose: The function returns a pointer to a location of the character buffer
		indicated by the current markc_offset
* Author: Isha Isha
* History/Versions: 1 25 Sep, 2019
* Called functions: no called functions
* Parameters: Buffer* const pBD
* Return Value: NULL, char
* Algorithm:
*/
char* b_location(Buffer* const pBD) {
	/*in case of run-time error return NULL*/
	if (pBD == NULL) {
		return NULL;
	}

	if (pBD->markc_offset < ZERO || pBD->markc_offset >= pBD->addc_offset) {
		return NULL;
	}

	return pBD->cb_head + pBD->markc_offset;
}