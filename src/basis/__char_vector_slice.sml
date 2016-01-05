(* __char_vector_slice.sml --- Basis Library CharVectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "mono_vector_slice";
require "_vector_slice";
require "__char_vector";

structure CharVectorSlice :> MONO_VECTOR_SLICE
				 where type elem = char
				 where type vector = CharVector.vector
                       (* FIXME  where type slice = Substring.substring  *)
 = MonoVectorSlice (CharVector)
