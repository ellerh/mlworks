(* _array_slice.sml --- ArraySlice functor
 *
 * This code has been placed in the public domain.
 *
 ** Commentary:
 *
 * This functor is used to implement ARRAY_SLICE and various
 * MONO_ARRAY_SLICES.
 *
 *)

require "_slice";

functor ArraySlice (
    type 'a elt
    structure Arr :
	      sig

		  type 'a array
		  val length : 'a array -> int
		  val unsafeSub : 'a array * int -> 'a elt
		  val unsafeUpdate :
		      'a array * int * 'a elt -> unit
		  val unsafeCopy : 'a array * int * int * 'a array * int
				   -> unit
	      end
    type 'a vector
    structure Vec :
	      sig
		  val alloc : int -> 'a vector
		  val unsafeSub : 'a vector * int -> 'a elt
		  val unsafeUpdate :
		      'a vector * int * 'a elt -> unit
	      end
    structure VecSlice :
	      sig type 'a slice
		  val base : 'a slice -> 'a vector * int * int
	      end) =
  struct

    structure Slice = Slice (struct
				type 'a elt = 'a elt
				type 'a seq = 'a Arr.array
				val length = Arr.length
				val unsafeSub = Arr.unsafeSub
			      end)

    open Slice

    fun update (SLICE {seq, start, stop}, i, x) =
	let val j = start + i
	in
	    if start <= j andalso j < stop then Arr.unsafeUpdate (seq, j, x)
	    else raise Subscript
	end

    fun vector (SLICE {seq, start, stop}) =
      let val v = Vec.alloc (stop - start)
	  fun loop i j =
	    if i = stop then v
	    else (Vec.unsafeUpdate (v, j, Arr.unsafeSub (seq, i));
		  loop (i + 1) (j + 1))
      in loop start 0 end

    fun copy {src as SLICE {seq, start, stop}, dst, di} =
      let val len = stop - start
      in
	  if 0 <= di andalso di + len <= Arr.length dst
	  then Arr.unsafeCopy (seq, start, len, dst, di)
	  else raise Subscript
      end

    fun copyVec {src, dst, di} =
	let
	    val (vec, si, len) = VecSlice.base src
	    val stop = si + len
	    fun loop si di =
		if si = stop then ()
		else (Arr.unsafeUpdate (dst, di, Vec.unsafeSub (vec, si));
		      loop (si + 1) (di + 1))
	in
	    if 0 <= di andalso di + len <= Arr.length dst then loop si di
	    else raise Subscript
	end

    fun modifyi f (SLICE {seq, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else let val x = f (i - start, (Arr.unsafeSub (seq, i)))
		     in
			 Arr.unsafeUpdate (seq, i, x);
			 loop (i + 1)
		     end
	in loop start end

    fun modify f (SLICE {seq, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else let val x = f (Arr.unsafeSub (seq, i))
		     in
			 Arr.unsafeUpdate (seq, i, x);
			 loop (i + 1)
		     end
	in loop start end

  end
