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
require "mono_vector";
require "mono_vector_slice";

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
    local
	structure S = Slice (struct
				type 'a elt = 'a V.elt
				type 'a seq = 'a V.seq
				val length = V.length
				val unsafeSub = V.unsafeSub
			      end)
    in open S end

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
		      case ss of
			  s :: ss =>
			  (cache := {start = stop, stop = stop + length s,
				     s = s, ss = ss};
			   lookup i)
			| [] => raise Fail "bug in tabulate"
		  else raise Fail "non-sequential tabulate? nyi"
	      end
	in V.tabulate (total, lookup) end

    fun mapi f (SLICE {seq, start, stop}) =
      V.tabulate (stop - start, fn i => f (i, (V.unsafeSub (seq, start + i))))

    fun map f (SLICE {seq, start, stop}) =
      V.tabulate (stop - start, fn i => f (V.unsafeSub (seq, start + i)))

  end

functor MonoVectorSlice (MV : MONO_VECTOR) : MONO_VECTOR_SLICE =
  struct
    local structure S = VectorSlice (struct
					type 'a elt = MV.elem
					type 'a seq = MV.vector
					val length = MV.length
					val tabulate = MV.tabulate
					val unsafeSub = MV.sub
				      end)
    in
    open S
    type slice = MV.elem S.slice
    type elem = MV.elem
    type vector = MV.vector
    end
  end
