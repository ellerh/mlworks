(*

Result: OK
 
 * Revision Log:
 * -------------
 * $Log: floatarray_duplicate.sml,v $
 * Revision 1.2  1997/05/28 11:55:38  jont
 * [Bug #30090]
 * Remove uses of MLWorks.IO
 *
 *  Revision 1.1  1997/01/07  14:27:17  andreww
 *  new unit
 *  [Bug #1818]
 *  floatarray tests
 *
 *
 *

Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

val a = MLWorks.Internal.FloatArray.arrayoflist(map real [0,1,2,3,4,5,6,7,8,9])

val b = MLWorks.Internal.FloatArray.duplicate a

fun eq(a, b) =
  let
    val l = MLWorks.Internal.FloatArray.length a
  in
    if l <> MLWorks.Internal.FloatArray.length b then
      (print"Fail on unequal length\n";
       false)
    else
      let
	fun compare 0 = true
	  | compare n =
	    let
	      val n' = n-1
	    in
	      (floor (MLWorks.Internal.FloatArray.sub(a, n'))) = 
                 (floor (MLWorks.Internal.FloatArray.sub(b, n'))) andalso
	      compare n'
	    end
      in
	compare l
      end
  end

val _ = print(if eq(a, b) andalso a <> b then "Pass\n" else "Fail\n")
