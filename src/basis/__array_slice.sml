(* array_slice.sml --- Standard ML Basis Library ArraySlice structure
 *
 * This code has been placed in the public domain.
 *)

require "array_slice";
require "__array";
require "__vector";
require "__vector_slice";

structure ArraySlice :> ARRAY_SLICE = struct

    datatype 'a slice = Slice of {array: 'a Array.array,
				  start: int,
				  stop: int}

    fun length (Slice {start, stop, ...}) = stop - start

    fun sub (Slice {array, start, stop}, i) =
	let val j = start + i
	in
	    if start <= j andalso j < stop then Array.sub (array, j)
	    else raise Subscript
	end

    fun update (Slice {array, start, stop}, i, x) =
	let val j = start + i
	in
	    if start <= j andalso j < stop then Array.update (array, j, x)
	    else raise Subscript
	end

    fun full array =
	Slice {array = array, start = 0, stop = Array.length array}

    fun slice (array, start, size) =
	let
	    val len = Array.length array
	    val stop = case size of
			   NONE => len
			 | SOME l => start + l
	in
	    if 0 <= start andalso start <= stop andalso stop <= len
	    then Slice {array = array, start = start, stop = stop}
	    else raise Subscript
	end

    fun subslice (Slice {array, start, stop}, i, size) =
	let
	    val start' = start + i
	    val stop' = case size of
			    NONE => stop
			  | SOME l => start' + l
	in
	    if start <= start' andalso start' <= stop' andalso stop' <= stop
	    then Slice {array = array, start = start', stop = stop'}
	    else raise Subscript
	end

    fun base (Slice {array, start, stop}) = (array, start, stop - start)

    fun vector (Slice {array, start, stop}) =
	Vector.tabulate (stop - start, fn i => Array.sub (array, start + i))

    fun copy {src as Slice {array, start, stop}, dst, di} =
	if 0 <= di andalso di + (stop - start) <= Array.length dst
	then MLWorks.Internal.ExtendedArray.copy (array, start, stop, dst, di)
	else raise Subscript

    fun copyVec {src, dst, di} =
	let
	    val (vec, start, len) = VectorSlice.base src
	    fun loop i =
		if i = len then ()
		else (Array.update (dst, di + i, Vector.sub (vec, start + i));
		      loop (i + 1))
	in
	    if 0 <= di andalso di + len <= Array.length dst then loop 0
	    else raise Subscript
	end

    fun isEmpty (Slice {start, stop, ...}) = start = stop

    fun getItem (Slice {array, start, stop}) =
	if start = stop then NONE
	else SOME (Array.sub (array, start),
		   Slice {array = array, start = start + 1, stop = stop})

    fun appi (f:(int * 'a) -> unit) (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else (f (i - start, (Array.sub (array, i)));
		      loop (i + 1))
	in loop start end

    fun app (f:'a -> unit) (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else (f (Array.sub (array, i));
		      loop (i + 1))
	in loop start end

    fun modifyi f (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else let val x = f (i - start, (Array.sub (array, i)))
		     in
			 Array.update (array, i, x);
			 loop (i + 1)
		     end
	in loop start end

    fun modify f (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then ()
		else let val x = f (Array.sub (array, i))
		     in
			 Array.update (array, i, x);
			 loop (i + 1)
		     end
	in loop start end

    fun foldli f init (Slice {array, start, stop}) =
	let fun loop i state =
		if i = stop then state
		else loop (i + 1)
			  (f (i - start, Array.sub (array, i), state))
	in loop start init end

    fun foldl f init (Slice {array, start, stop}) =
	let fun loop i state =
		if i = stop then state
		else loop (i + 1) (f (Array.sub (array, i), state))
	in loop start init end

    fun foldri f init (Slice {array, start, stop}) =
	let fun loop i state =
		if i < start then state
		else loop (i - 1)
			  (f (i - start, Array.sub (array, i), state))
	in loop (stop - 1) init end

    fun foldr f init (Slice {array, start, stop}) =
	let fun loop i state =
		if i < start then state
		else loop (i - 1) (f (Array.sub (array, i), state))
	in loop (stop - 1) init end

    fun findi f (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then NONE
		else let val x = Array.sub (array, i)
		     in
			 if f (i, x) then SOME (i, x)
			 else loop (i + 1)
		     end
	in loop 0 end

    fun find f (Slice {array, start, stop}) =
	let fun loop i =
		if i = stop then NONE
		else let val x = Array.sub (array, i)
		     in
			 if f x then SOME x
			 else loop (i + 1)
		     end
	in loop 0 end

    fun exists f (Slice {array, start, stop}) =
	let fun loop i =
		i < stop andalso f (Array.sub (array, i)) orelse loop (i + 1)
	in loop 0 end

    fun all f (Slice {array, start, stop}) =
	let fun loop i =
		i = stop orelse f (Array.sub (array, i)) andalso loop (i + 1)
	in loop 0 end

    fun collate f (Slice {array = v1, start = s1, stop = e1},
		   Slice {array = v2, start = s2, stop = e2}) =
	let fun loop i1 i2 =
		if i1 = e1 then (if i2 = e2 then EQUAL else LESS)
		else if i2 = e2 then GREATER
		else case f (Array.sub (v1, i1),
			     Array.sub (v2, i2)) of
			 EQUAL => loop (i1 + 1) (i2 + 1)
		       | x => x
	in loop s1 s2 end

  end
