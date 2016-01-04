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

(* unsafeUpdate is used to initialize freshly allocated vectors. *)
functor VectorSlice
	    (V : sig
		 type 'a elt
		 type 'a seq
		 val length : 'a seq -> int
		 val unsafeSub : 'a seq * int -> 'a elt
		 val unsafeUpdate : 'a seq * int * 'a elt -> unit
		 val alloc : int -> 'a seq
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

    local
	fun copy (src, start, stop, dst, di) =
	  let fun loop s d =
		if s = stop then ()
		else (V.unsafeUpdate (dst, d, V.unsafeSub (src, s));
		      loop (s + 1) (d + 1))
	  in loop start di end
    in

    fun vector (SLICE {seq, start, stop}) =
      if start = 0 andalso stop = V.length seq then seq
      else
	  let val len = stop - start
	      val result = V.alloc len
	      val _ = copy (seq, start, stop, result, 0);
	  in result end

    fun concat [] = V.alloc 0
      | concat [s] = vector s
      | concat (l : 'a slice list) =
	let fun count [] len = len
	      | count (s :: ss) len = count ss ((length s) + len)
	    val total = count l 0
	    val result = V.alloc total
	    fun init [] i = result
	      | init (SLICE {seq, start, stop} :: ss) i =
		let val _ = copy (seq, start, stop, result, i)
		    val len = stop - start
		in init ss (i + len) end
	in init l 0 end
    end

    fun mapi f (SLICE {seq, start, stop}) =
      let val len = stop - start
	  val result = V.alloc len
	  fun loop s d =
	    if s = stop then result
	    else (V.unsafeUpdate (result, d, f (d, (V.unsafeSub (seq, s))));
		  loop (s + 1) (d + 1))
      in loop start 0 end

    fun map f (SLICE {seq, start, stop}) =
      let val len = stop - start
	  val result = V.alloc len
	  fun loop s d =
	    if s = stop then result
	    else (V.unsafeUpdate (result, d, f (V.unsafeSub (seq, s)));
		  loop (s + 1) (d + 1))
      in loop start 0 end

  end
