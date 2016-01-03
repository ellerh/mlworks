(* __vector_slice.sml --- Standard ML Basis Library VectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "vector_slice";
require "_vector_slice";
require "__vector";

structure VectorSlice :> VECTOR_SLICE = struct

    structure V = Vector
    structure I = MLWorks.Internal.Value

    structure Intrinsics =
      struct
	type 'a elt = 'a
	type 'a seq = 'a V.vector

	val length = V.length

	fun unsafeAlloc size : 'a V.vector = I.cast (I.alloc_vector size)

	fun alloc i =
	  if 0 <= i andalso i <= V.maxLen then unsafeAlloc i
	  else raise Size

	val unsafeSub = I.unsafe_record_sub
	val unsafeUpdate = I.unsafe_record_update

	fun unsafeCopy (src, si, len, dst, di) =
	  let val stop = si + len
	      fun loop si di =
		if si = stop then ()
		else (unsafeUpdate (dst, di, unsafeSub (src, si));
		      loop (si + 1) (di + 1))
	  in loop si di end

      end

    structure VS = VectorSlice (Intrinsics)

    open VS

  end
