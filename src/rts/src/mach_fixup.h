/*  ==== ASSEMBLY-LANGUAGE FUNCTIONS ====
 *
 *  Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 *  All rights reserved.
 *  
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *  
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  Description
 *  -----------
 *  Declarations for assembly-language functions.
 *
 *  Revision Log
 *  ------------
 *  $Log: mach_fixup.h,v $
 *  Revision 1.2  1994/06/09 14:52:20  nickh
 *  new file
 *
 * Revision 1.1  1994/06/09  11:27:28  nickh
 * new file
 *
 *  Revision 1.1  1992/10/23  16:14:55  richard
 *  Initial revision
 *
 */

#ifndef mach_fixup_h
#define mach_fixup_h

#ifdef MACH_FIXUP

#include "mltypes.h"

/* Fixup function; the core of the GC. Defined in mach_fixup.S, q.v. */

extern mlval *mach_fixup(mlval *to, mlval *what, mlval value);

#endif /* MACH_FIXUP */

#endif /* mach_fixup_h */
