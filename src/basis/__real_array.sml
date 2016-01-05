(*  ==== INITIAL BASIS : REAL ARRAY ====
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
 *  Implementation
 *  --------------
 *  Real arrays are identified with MLWorks floatarray objects - note that
 *  the basic MONO_ARRAY signature is reduced functionality from our own
 *  FloatArray signature.
 *  
 *
 *  Revision Log
 *  ------------
 *  $Log: __real_array.sml,v $
 *  Revision 1.4  1999/03/20 21:38:31  daveb
 *  [Bug #20125]
 *  Replaced substructure with type.
 *
 *  Revision 1.3  1999/02/17  14:39:59  mitchell
 *  [Bug #190507]
 *  Modify to satisfy CM constraints.
 *
 *  Revision 1.2  1998/02/19  19:55:49  mitchell
 *  [Bug #30349]
 *  Fix to avoid non-unit sequence warnings
 *
 *  Revision 1.1  1997/01/30  16:53:10  andreww
 *  new unit
 *  New RealArray mono array which uses floatarray builtins.
 *
 * 
 *
 *)

require "mono_array";
require "_array_ops";
require "__pre_real";
require "__real_vector";
require "__real_vector_slice";

structure RealArray : MONO_ARRAY where type elem = PreReal.real =
  struct
    type elem = PreReal.real
    type vector = RealVector.vector
    structure F = MLWorks.Internal.FloatArray

    type array = F.floatarray
    val maxLen = F.maxLen

    fun check_size n =
      if n < 0 orelse n > maxLen then raise Size else n

    fun array (i, e) = F.array (check_size i, e)
    fun tabulate (i, f) = F.tabulate (check_size i,f)

    val llength = length (* remember toplevel List.length *)
    val length = F.length
    val sub = F.sub
    val update = F.update
    val cast = MLWorks.Internal.Value.cast

    local
	structure V = RealVector
	structure VS = RealVectorSlice
	structure Arr =
	  struct
	    type 'a array = array
	    val length = length
	    val tabulate = tabulate
	    val array = array
	    val unsafeSub = sub
	    val unsafeUpdate = update
	  end
	structure Vec =
	  struct
	    val tabulate = V.tabulate
	    val unsafeSub = V.sub
	  end
	structure Ops = ArrayOps (type 'a elt = elem
				  type 'a vector = vector
				  structure Arr = Arr
				  structure Vec = Vec
				  structure VecSlice = struct
				      type 'a slice = VS.slice
				      val base = VS.base
				    end)
    in open Ops end


    fun fromList [] = F.array(0,0.0)
      | fromList l =
      if llength l> maxLen then raise Size else F.from_list l

    fun check_slice (array,i,SOME j) =
      if i < 0 orelse j < 0 orelse i + j > length array
        then raise Subscript
      else j
      | check_slice (array,i,NONE) =
        let
          val l = length array
        in
          if i < 0 orelse i > l
            then raise Subscript
          else l - i
        end




    (* In the following, we use casts to convert realarrays into
     * realvectors.  We know that they're both implemented as
     * floatarrays.
     *)

    fun extract(array, i, j): vector =
      let
        val len = check_slice (array,i,j)
      in
        cast(F.tabulate(len, fn n => sub (array, n+i)))
      end


    
    fun copy' { src, si, len, dst, di } =
      let
        val srcLen = length src
        val l = case len
                  of SOME l => l
                   | NONE => srcLen - si
      in
        if si >= 0 
           andalso l >= 0 
           andalso si + l <= srcLen
           andalso di >= 0
           andalso di + l <= length dst
          then F.copy(src, si, si+l, dst, di)
        else raise Subscript
      end

    fun copy { src, dst, di } =
      let
        val l = length src
      in
        if di >= 0 andalso di + l <= length dst
          then F.copy(src, 0, l, dst, di)
        else raise Subscript
      end

    fun copyVec {src, si, len, dst, di} =
         copy' {src=cast src,si=si,len=len,dst=dst,di=di}

    (* this is based on the assumption that real vectors are
     * implemented as floatarrays too.
     *)



    fun app f vector =
      let
	val l = length vector
	fun iterate n =
	  if n = l then
	    ()
	  else
	    (ignore(f(sub(vector, n)));
	     iterate(n+1))
      in
	iterate 0
      end

    fun foldl f b array =
      let
	val l = length array
	fun reduce(n, x) =
	  if n = l then
	    x
	  else
	    reduce(n+1, f(sub(array, n), x))
      in
	reduce(0, b)
      end

    fun foldr f b array =
      let
	val l = length array
	fun reduce(n, x) =
	  if n < 0 then
	    x
	  else
	    reduce(n-1, f(sub(array, n), x))
      in
	reduce(l-1, b)
      end

    fun modify f array =
      let
	val l = length array
	fun iterate n =
	  if n = l then
	    ()
	  else
	    (update(array, n, f(sub(array, n)));
	     iterate(n+1))
      in
	iterate 0
      end

  end
