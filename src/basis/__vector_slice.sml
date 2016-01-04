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

    structure Vec =
      struct
	type 'a elt = 'a
	type 'a seq = 'a V.vector
	val length = V.length
	val tabulate = V.tabulate
	val unsafeSub = I.unsafe_record_sub
      end

    structure VS = VectorSlice (Vec)
    open VS
  end
