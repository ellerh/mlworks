(* __char_vector_slice.sml --- Basis Library CharVectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "mono_vector_slice";
require "_vector_slice";
require "__char_vector";
require "__pre_basis";

structure CharVectorSlice : MONO_VECTOR_SLICE = struct
    structure V = CharVector

    structure Intrinsics =
      struct
	val unsafeAlloc = MLWorks.Internal.Value.alloc_string
	val alloc = PreBasis.alloc_string

	val unsafeSub = chr o MLWorks.Internal.Value.unsafe_string_sub

	fun unsafeUpdate (v, i, c) =
	  MLWorks.Internal.Value.unsafe_string_update (v, i, ord c)

	fun unsafeCopy (src, si, len, dst, di) =
	  let val stop = si + len
	      fun loop si di =
		if si = stop then ()
		else (unsafeUpdate (dst, di, unsafeSub (src, si));
		      loop (si + 1) (di + 1))
	  in loop si di end

	type 'a elt = V.elem
	type 'a seq = V.vector
	val length = V.length
      end

    structure VS = VectorSlice (Intrinsics)

    open VS
    type elem = V.elem
    type slice = elem slice
    type vector = V.vector
  end
