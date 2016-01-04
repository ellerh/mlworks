(* array_slice.sml --- Standard ML Basis Library ArraySlice structure
 *
 * This code has been placed in the public domain.
 *)

require "array_slice";
require "_array_slice";
require "__array";
require "__vector";
require "__vector_slice";

structure ArraySlice : ARRAY_SLICE =
  struct
    structure A = Array
    structure V = Vector
    structure VS = VectorSlice
    structure I = MLWorks.Internal.Value

    structure Arr =
      struct
	type 'a array = 'a A.array
	val length = A.length
	val unsafeSub =  I.unsafe_array_sub
	val unsafeUpdate = I.unsafe_array_update
      end

    structure Vec =
      struct
	fun check_size n = if n < 0 orelse n > V.maxLen then raise Size else n
	fun unsafeAlloc size : 'a V.vector = I.cast (I.alloc_vector size)
	fun alloc size = unsafeAlloc (check_size size)
	val unsafeSub = I.unsafe_record_sub
	val unsafeUpdate = I.unsafe_record_update
      end

    structure S = ArraySlice (type 'a elt = 'a
			      type 'a vector = 'a V.vector
			      structure Arr = Arr
			      structure Vec = Vec
			      structure VecSlice = VS)

    open S
  end
