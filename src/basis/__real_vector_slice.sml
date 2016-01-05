(* __real_vector_slice.sml --- Basis Library RealVectorSlice structure
 *
 * This code has been placed in the public domain.
 *)

require "mono_vector_slice";
require "_vector_slice";
require "__real_vector";

structure RealVectorSlice :> MONO_VECTOR_SLICE  (* OPTIONAL *)
				 where type vector = RealVector.vector
				 where type elem = real
  = MonoVectorSlice (RealVector)
