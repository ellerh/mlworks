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

functor VectorSlice
	    (V : sig
		 type 'a elt
		 type 'a seq
		 val length : 'a seq -> int
		 val tabulate : int * (int -> 'a elt) -> 'a seq
		 val unsafeSub : 'a seq * int -> 'a elt
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
      if start = 0 andalso stop = V.length seq then seq
      else V.tabulate (stop - start, fn i => V.unsafeSub (seq, start + i))

    fun concat (l as []) = V.tabulate (0, fn i => sub (hd l, 0))
      | concat [s] = vector s
      | concat (l as s :: ss) =
	let fun count [] len = len
	      | count (s :: ss) len = count ss ((length s) + len)
	    val total = count l 0
	    val cache = ref {start = 0, stop = length s, s = s, ss = ss}
	    fun lookup i =
	      let val {start, stop, s, ss} = !cache
	      in
		  if start <= i andalso i < stop then
		      let val SLICE {seq, start=start', ...} = s
		      in V.unsafeSub (seq, start' + (i - start)) end
		  else if i = stop then
		      let val s :: ss = ss
		      in
			  cache := {start = stop, stop = stop + length s,
				    s = s, ss = ss};
			  lookup i
		      end
		  else raise Fail "non-sequential tabulate?"
	      end
	in V.tabulate (total, lookup) end

    fun mapi f (SLICE {seq, start, stop}) =
      V.tabulate (stop - start, fn i => f (i, (V.unsafeSub (seq, start + i))))

    fun map f (SLICE {seq, start, stop}) =
      V.tabulate (stop - start, fn i => f (V.unsafeSub (seq, start + i)))

  end
