/*
 * ==== Architecture dependent ML VALUE TOOLS ====
 *		SPARC
 *
 * Copyright (C) 1994 Harlequin Ltd.
 *
 * Description
 * -----------
 * These macros are used by other SPARC assembly language routines in
 * the ML to C interface and the C language routines for loading.
 * They deal with code vectors.
 *
 * $Log: mach_values.h,v $
 * Revision 1.6  1997/05/30 09:29:57  jont
 * [Bug #30076]
 * Modifications to allow stack based parameter passing on the I386
 *
 * Revision 1.5  1995/09/01  13:57:38  nickb
 * Add CCODE_UNIT_SIZE
 *
 * Revision 1.4  1995/08/31  13:43:46  nickb
 * Add INTERCEPT_LENGTH.
 *
 * Revision 1.3  1994/10/05  10:40:12  jont
 * Add change_code_endian for use by loader.c
 *
 * Revision 1.2  1994/08/25  10:08:29  matthew
 * Increase CCODE_NUMBER_BITS to 10
 *
 * Revision 1.1  1994/07/25  15:58:03  jont
 * new file
 *
 */

#ifndef mach_values_h
#define mach_values_h

#define CCODE_NONGC_BITS	16
#define CCODE_SAVES_BITS	0
#define CCODE_NUMBER_BITS	10
#define CCODE_ARGS_BITS         0 /* Only non-zero on I386 */

/* The next few macros calculate the sizes and offsets of the other
fields; they're used in other macros in this file but shouldn't be
used directly.
 */

#define CCODE_INTERCEPT_BITS	(31 - CCODE_NONGC_BITS - CCODE_ARGS_BITS - CCODE_SAVES_BITS - CCODE_NUMBER_BITS)
#define CCODE_LEAF_BIT		(31 - CCODE_NONGC_BITS - CCODE_ARGS_BITS - CCODE_SAVES_BITS)
#define CCODE_NONGC_SHIFT	(32 - CCODE_NONGC_BITS)
#define CCODE_SAVES_SHIFT	(32 - CCODE_NONGC_BITS - CCODE_ARGS_BITS - CCODE_SAVES_BITS)
#define CCODE_INTERCEPT_SHIFT	CCODE_NUMBER_BITS
#define CCODE_INTERCEPT_MASK	((1 << CCODE_INTERCEPT_BITS)-1)
#define CCODE_ARGS_SHIFT	(32-CCODE_NONGC_BITS-CCODE_ARGS_BITS)

/* Now we get on to macros used out there in the runtime system:

CCODE_MAX_<field> is the maximum valid number in a field.
CCODE_NO_INTERCEPT is the value placed in the 'intercept' field of a
code item not compiled for tracing or call-counting.
*/

#define CCODE_MAX_NUMBER	((1 << CCODE_NUMBER_BITS)-1)
#define CCODE_MAX_SAVES		((1 << CCODE_SAVES_BITS)-1)
#define CCODE_MAX_NONGC		((1 << CCODE_NONGC_BITS)-1)
#define CCODE_MAX_ARGS		((1 << CCODE_ARGS_BITS)-1)
#define CCODE_MAX_INTERCEPT	(CCODE_INTERCEPT_MASK -1)
#define CCODE_NO_INTERCEPT	CCODE_INTERCEPT_MASK

#define change_code_endian	1

#define INTERCEPT_LENGTH	SPARC_INTERCEPT_LENGTH

#define CCODE_UNIT_SIZE		4

#endif
