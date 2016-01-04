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
    type 'a vector
    structure Arr : sig
		  type 'a array
		  val length : 'a array -> int
		  val unsafeSub : 'a array * int -> 'a elt
		  val unsafeUpdate : 'a array * int * 'a elt -> unit
	      end
    structure Vec : sig
		  val tabulate : int * (int -> 'a elt) -> 'a vector
		  val unsafeSub : 'a vector * int -> 'a elt
	      end
    structure VecSlice : sig
		  type 'a slice
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
      Vec.tabulate (stop - start, fn i => Arr.unsafeSub (seq, start + i))

    fun copy {src as SLICE {seq, start, stop}, dst, di} =
      let val len = stop - start
	  val dstop = di + len
	  fun copyUp s d =
	    if s = stop then ()
	    else (Arr.unsafeUpdate (dst, d, Arr.unsafeSub (seq, s));
		  copyUp (s + 1) (d + 1))
	  fun copyDown s d =
	    if s < start then ()
	    else (Arr.unsafeUpdate (dst, d, Arr.unsafeSub (seq, s));
		  copyDown (s - 1) (d - 1))
      in
	  if di < 0 orelse Arr.length dst < dstop then
	      raise Subscript
	  else if start < di andalso di <= stop then
	      copyDown (stop - 1) (dstop - 1)
	  else
	      copyUp start di
      end

    fun copyVec {src, dst, di} =
	let
	    val (vec, si, len) = VecSlice.base src
	    val stop = si + len
	    fun loop s d =
		if s = stop then ()
		else (Arr.unsafeUpdate (dst, d, Vec.unsafeSub (vec, s));
		      loop (s + 1) (d + 1))
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
