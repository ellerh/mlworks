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

    structure AIntrinsics =
      struct
	type 'a array = 'a A.array
	val length = A.length
	val unsafeSub =  I.unsafe_array_sub
	val unsafeUpdate = I.unsafe_array_update
	val unsafeCopy = MLWorks.Internal.ExtendedArray.copy
      end

    structure VIntrinsics =
      struct
	fun unsafeAlloc size : 'a V.vector =
	  I.cast
	      (I.alloc_vector size)
	fun alloc i =
	  if 0 <= i andalso i <= V.maxLen then unsafeAlloc i
	  else raise Size
	val unsafeSub = I.unsafe_record_sub
	val unsafeUpdate = I.unsafe_record_update
      end

    structure S = ArraySlice (type 'a elt = 'a
			      structure Arr = AIntrinsics
			      type 'a vector = 'a V.vector
			      structure Vec = VIntrinsics
			      structure VecSlice = VS)

    open S
  end
