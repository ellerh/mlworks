(* __word8_vector_slice.sml --- Basis Library Word8VectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "mono_vector_slice";
require "_vector_slice";
require "__word8_vector";
require "__word8";

structure Word8VectorSlice : MONO_VECTOR_SLICE = struct
    structure V = Word8Vector
    structure I = MLWorks.Internal.Value

    structure Vec =
      struct
	fun check_size n = if n < 0 orelse n > V.maxLen then raise Size else n
	fun unsafeAlloc size : V.vector = I.cast (I.alloc_string (size + 1))
	fun alloc size = unsafeAlloc (check_size size)

	fun unsafeSub (v : V.vector, i) =
	  Word8.fromInt (I.unsafe_string_sub (I.cast v, i))

	fun unsafeUpdate (v : V.vector, i, c) =
	  I.unsafe_string_update (I.cast v, i, Word8.toInt c)

	type 'a elt = V.elem
	type 'a seq = V.vector
	val length = V.length
      end

    structure VS = VectorSlice (Vec)
    open VS
    type elem = V.elem
    type slice = elem slice
    type vector = V.vector
  end
