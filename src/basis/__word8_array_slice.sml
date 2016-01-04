(* __word8_array_slice.sml --- Basis Library Word8ArraySlice structure
 *
 * This code has been placed in the public domain.
 *)

require "_array_slice"
require "__word8_array"
require "__word8_vector"
require "mono_array_slice"
require "__word8_vector_slice"
require "__word8"

structure Word8ArraySlice : MONO_ARRAY_SLICE =
  struct
    structure A = Word8Array
    structure V = Word8Vector
    structure VS = Word8VectorSlice
    structure I = MLWorks.Internal.Value

    (* FIXME: find a way to access the underlying ByteArray *)
    structure Arr =
      struct
	type 'a array = A.array
	val length = A.length
	val unsafeSub = A.sub
	val unsafeUpdate = A.update
      end

    structure Vec =
      struct
	type 'a vector = V.vector
	fun check_size n = if n < 0 orelse n > V.maxLen then raise Size else n
	fun unsafeAlloc len : V.vector = I.cast (I.alloc_string (len + 1))
	fun alloc len = unsafeAlloc (check_size len)

	fun unsafeSub (v : V.vector, i) =
	  Word8.fromInt (I.unsafe_string_sub (I.cast v, i))

	fun unsafeUpdate (v : V.vector, i, c) =
	  I.unsafe_string_update (I.cast v, i, Word8.toInt c)

      end

    structure AS = ArraySlice (
	  type 'a elt = A.elem
	  type 'a vector = V.vector
	  structure Arr = Arr
	  structure Vec = Vec
	  structure VecSlice =
	    struct
	      type 'a slice = VS.slice
	      val base = VS.base
	    end)
    open AS

    type array = A.array
    type elem = A.elem
    type slice = elem slice
    type vector = V.vector
    type vector_slice = VS.slice
  end
