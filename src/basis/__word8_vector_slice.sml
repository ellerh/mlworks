(* __word8_vector_slice.sml --- Basis Library Word8VectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "mono_vector_slice";
require "_vector_slice";
require "__word8_vector";
require "__word8";

structure Word8VectorSlice :> MONO_VECTOR_SLICE
				  where type vector = Word8Vector.vector
				  where type elem = Word8.word =
  MonoVectorSlice (Word8Vector)
