(* __char_array_slice.sml --- Basis Library CharArraySlice structure
 *
 * This code has been placed in the public domain.
 *)

require "_array_slice";
require "__char_array";
require "__char_vector";
require "mono_array_slice";
require "__char_vector_slice";
require "__pre_basis";

structure CharArraySlice : MONO_ARRAY_SLICE =
  struct
    structure A = CharArray
    structure V = CharVector
    structure VS = CharVectorSlice
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
	val tabulate = V.tabulate
	val unsafeSub = chr o I.unsafe_string_sub
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
	    end
      )
    open AS

    type array = A.array
    type elem = A.elem
    type slice = elem slice
    type vector = V.vector
    type vector_slice = VS.slice
  end
