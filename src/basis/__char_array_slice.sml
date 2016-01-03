(* __char_array_slice.sml --- Basis Library CharArraySlice structure
 *
 * This code has been placed in the public domain.
 *)

require "_array_slice"
require "__char_array"
require "__char_vector"
require "mono_array_slice"
require "__char_vector_slice"
require "__pre_basis";

structure CharArraySlice : MONO_ARRAY_SLICE =
  struct
    structure A = CharArray
    structure V = CharVector
    structure VS = CharVectorSlice

    (* FIXME: find a way to access the underlying ByteArray *)
    structure AIntrinsics =
      struct
	type 'a array = A.array
	val length = A.length
	val unsafeSub = A.sub
	val unsafeUpdate = A.update

	fun unsafeCopy (src, si, len, dst, di) =
	  let val stop = si + len
	      fun loop si di =
		if si = stop then ()
		else (unsafeUpdate (dst, di, unsafeSub (src, si));
		      loop (si + 1) (di + 1))
	  in loop si di end
      end

    structure VIntrinsics =
      struct
	val alloc = PreBasis.alloc_string
	val unsafeSub = chr o MLWorks.Internal.Value.unsafe_string_sub
	fun unsafeUpdate (v, i, c) =
	  MLWorks.Internal.Value.unsafe_string_update (v, i, ord c)
      end

    structure AS = ArraySlice (
	  type 'a elt = A.elem
	  structure Arr = AIntrinsics
	  type 'a vector = V.vector
	  structure Vec = VIntrinsics
	  structure VecSlice =
	    struct
	      type 'a slice = VS.slice
	      val base = VS.base
	    end
      )
    open AS

    type array = A.array
    type elem = A.elem
    type slice = elem slice
    type vector = V.vector
    type vector_slice = VS.slice
  end
