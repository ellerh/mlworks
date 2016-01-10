(* _slice.sml --- Slice functor
 *
 * This code has been placed in the public domain.
 *
 ** Commentary:
 *
 * This contains common functionality between VECTOR_SLICE and
 * ARRA_SLICE
 *
 *)

(* unsafeSub is like Vector.sub but doesn't need to perform bound checks. *)
functor Slice (Sequence : sig
		   type 'a elt
		   type 'a seq
		   val length : 'a seq -> int
		   val unsafeSub : 'a seq * int -> 'a elt
	       end) =
  struct
    local
	structure S = Sequence
    in

    datatype 'a slice = SLICE of {seq : 'a S.seq, start : int, stop : int}

    fun length (SLICE {start, stop, ...}) = stop - start

    fun sub (SLICE {seq, start, stop}, i) =
      let val j = start + i
      in
	  if start <= j andalso j < stop then S.unsafeSub (seq, j)
	  else raise Subscript
      end

    fun full seq = SLICE {seq = seq, start = 0, stop = S.length seq}

    fun slice (seq, start, size) =
      let
	  val len = S.length seq
	  val stop = case size of
			 NONE => len
		       | SOME l => start + l
      in
	  if 0 <= start andalso start <= stop andalso stop <= len
	  then SLICE {seq = seq, start = start, stop = stop}
	  else raise Subscript
      end

    fun subslice (SLICE {seq, start, stop}, i, size) =
      let
	  val start' = start + i
	  val stop' = case size of
			  NONE => stop
			| SOME l => start' + l
      in
	  if start <= start' andalso start' <= stop' andalso stop' <= stop
	  then SLICE {seq = seq, start = start', stop = stop'}
	  else raise Subscript
      end

    fun base (SLICE {seq, start, stop}) = (seq, start, stop - start)

    fun isEmpty (SLICE {start, stop, ...}) = start = stop

    fun getItem (SLICE {seq, start, stop}) =
      if start = stop then NONE
      else SOME (S.unsafeSub (seq, start),
		 SLICE {seq = seq, start = start + 1, stop = stop})

    fun appi (f : int * 'a S.elt -> unit) (SLICE {seq, start, stop}) =
      let fun loop i j =
	    if i = stop then ()
	    else (f (j, (S.unsafeSub (seq, i)));
		  loop (i + 1) (j + 1))
      in loop start 0 end

    fun app (f : 'a S.elt -> unit) (SLICE {seq, start, stop}) =
      let fun loop i =
	    if i = stop then ()
	    else (f (S.unsafeSub (seq, i));
		  loop (i + 1))
      in loop start end

    fun foldli f init (SLICE {seq, start, stop}) =
      let fun loop i j state =
	    if i = stop then state
	    else loop (i + 1)
		      (j + 1)
		      (f (j, S.unsafeSub (seq, i), state))
      in loop start 0 init end

    fun foldl f init (SLICE {seq, start, stop}) =
      let fun loop i state =
	    if i = stop then state
	    else loop (i + 1) (f (S.unsafeSub (seq, i), state))
      in loop start init end

    fun foldri f init (SLICE {seq, start, stop}) =
      let fun loop i j state =
	    if i < start then state
	    else loop (i - 1)
		      (j - 1)
		      (f (j, S.unsafeSub (seq, i), state))
      in loop (stop - 1) (stop - start - 1) init end

    fun foldr f init (SLICE {seq, start, stop}) =
      let fun loop i state =
	    if i < start then state
	    else loop (i - 1) (f (S.unsafeSub (seq, i), state))
      in loop (stop - 1) init end

    fun findi f (SLICE {seq, start, stop}) =
      let fun loop i j =
	    if i = stop then NONE
	    else let val x = S.unsafeSub (seq, i)
		 in
		     if f (j, x) then SOME (j, x)
		     else loop (i + 1) (j + 1)
		 end
      in loop start 0 end

    fun find f (SLICE {seq, start, stop}) =
      let fun loop i =
	    if i = stop then NONE
	    else let val x = S.unsafeSub (seq, i)
		 in
		     if f x then SOME x
		     else loop (i + 1)
		 end
      in loop start end

    fun exists f (SLICE {seq, start, stop}) =
      let fun loop i =
	    if i = stop then false
	    else f (S.unsafeSub (seq, i)) orelse loop (i + 1)
      in loop start end

    fun all f (SLICE {seq, start, stop}) =
      let fun loop i =
	    if i = stop then true
	    else f (S.unsafeSub (seq, i)) andalso loop (i + 1)
      in loop start end

    fun collate f (SLICE {seq = v1, start = s1, stop = e1},
		   SLICE {seq = v2, start = s2, stop = e2}) =
      let fun loop i1 i2 =
	    if i1 = e1 then (if i2 = e2 then EQUAL else LESS)
	    else if i2 = e2 then GREATER
	    else case f (S.unsafeSub (v1, i1),
			 S.unsafeSub (v2, i2)) of
		     EQUAL => loop (i1 + 1) (i2 + 1)
		   | x => x
      in loop s1 s2 end

    end
  end
