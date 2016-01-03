(* _vector_slice.sml --- VectorSlice functor
 *
 * This code has been placed in the public domain.
 *
 ** Commentary:
 *
 * This functor is used to implement VECTOR_SLICE and various
 * MONO_VECTOR_SLICES.
 *
 *)

require "_slice";

(* unsafeUpdate and unsafeCopy are used to initialize freshly
   allocated vectors.  unsafeAlloc doesn't check whether the length
   argument is negative or too large.
*)
functor VectorSlice
	    (V : sig
		 type 'a elt
		 type 'a seq
		 val length : 'a seq -> int
		 val unsafeSub : 'a seq * int -> 'a elt
		 val unsafeUpdate : 'a seq * int * 'a elt -> unit
		 val alloc : int -> 'a seq
		 val unsafeAlloc : int -> 'a seq
		 val unsafeCopy : 'a seq * int * int * 'a seq * int -> unit
	     end
	    ) =
  struct
    structure S = Slice (struct
			    type 'a elt = 'a V.elt
			    type 'a seq = 'a V.seq
			    val length = V.length
			    val unsafeSub = V.unsafeSub
			  end)
    open S

    fun vector (SLICE {seq, start, stop}) =
      let val len = stop - start
      in
	  if start = 0 andalso len = V.length seq then seq
	  else
	      let val result = V.unsafeAlloc len
		  val _ = V.unsafeCopy (seq, start, len, result, 0);
	      in result end
      end

    fun concat [] = V.alloc 0
      | concat [s] = vector s
      | concat (l : 'a slice list) =
	let fun count [] len = len
	      | count (s :: ss) len = count ss ((length s) + len)
	    val total = count l 0
	    val result = V.alloc total
	    fun copy [] i = result
	      | copy (SLICE {seq, start, stop} :: ss) i =
		let val len = stop - start
		    val _ = V.unsafeCopy (seq, start, len, result, i)
		in copy ss (i + len) end
	in copy l 0 end

    fun mapi f (SLICE {seq, start, stop}) =
      let val len = stop - start
	  val result = V.unsafeAlloc len
	  fun loop i j =
	    if i = stop then result
	    else (V.unsafeUpdate (result, j, f (j, (V.unsafeSub (seq, i))));
		  loop (i + i) (j + 1))
      in loop start 0 end

    fun map f (SLICE {seq, start, stop}) =
      let val len = stop - start
	  val result = V.unsafeAlloc len
	  fun loop i j =
	    if i = stop then result
	    else (V.unsafeUpdate (result, j, f (V.unsafeSub (seq, i)));
		  loop (i + i) (j + 1))
      in loop start 0 end

  end
