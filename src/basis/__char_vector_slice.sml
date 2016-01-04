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
    structure I = MLWorks.Internal.Value

    structure Vec =
      struct
	type 'a elt = V.elem
	type 'a seq = V.vector
	val length = V.length
	val unsafeSub = chr o I.unsafe_string_sub
	val tabulate = V.tabulate
      end

    structure VS = VectorSlice (Vec)

    open VS
    type elem = V.elem
    type slice = elem slice
    type vector = V.vector
  end
