(* _vector_ops.sml --- VectorOps functor
 *
 * This code has been placed in the public domain.
 *
 ** Commentary:
 *
 * This functor is used to implement various functions in of VECTOR
 * and MONO_VECTOR.
 *
 *)

require "_vector_slice";

functor VectorOps
	    (V : sig
		 type 'a elt
		 type 'a seq
		 val length : 'a seq -> int
		 val alloc : int -> 'a seq
		 val unsafeSub : 'a seq * int -> 'a elt
		 val unsafeUpdate : 'a seq * int * 'a elt -> unit
	     end) =
  struct

    fun fromList l =
      let val len = length l
	  val result = V.alloc len
	  fun loop [] i = result
	    | loop (x :: xs) i =
	      (V.unsafeUpdate (result, i, x);
	       loop xs (i + 1))
      in loop l 0 end

    fun tabulate (len, f) =
      let val result = V.alloc len
	  fun loop i =
	    if i = len then result
	    else (V.unsafeUpdate (result, i, f i);
		  loop (i + 1))
      in loop 0 end

    local
	fun copy (src, start, stop, dst, di) =
	  let fun loop s d =
		if s = stop then ()
		else (V.unsafeUpdate (dst, d, V.unsafeSub (src, s));
		      loop (s + 1) (d + 1))
	  in loop start di end
    in

    fun update (v, i, x) =
      let val len = V.length v
	  val result = V.alloc len
	  val _ = copy (v, 0, len, result, 0)
	  val _ = V.unsafeUpdate (result, i, x)
      in result end

    fun concat [] = V.alloc 0
      | concat [v] = v
      | concat l =
	let fun count [] sum = sum
	      | count (v :: vs) sum = count vs (sum + (V.length v))
	    val total = count l 0
	    val result = V.alloc total
	    fun init [] _ = result
	      | init (v :: vs) i =
		let val len = V.length v
		    val _ = copy (v, 0, len, result, i)
		in init vs (i + len) end
	in init l 0 end
    end

    local
	  structure VS = VectorSlice (V)
    in
    fun appi f v = VS.appi f (VS.full v)
    fun app  f v = VS.app f (VS.full v)
    fun mapi f v = VS.mapi f (VS.full v)
    fun map f v = VS.map f (VS.full v)
    fun foldli f z v = VS.foldli f z (VS.full v)
    fun foldri f z v = VS.foldri f z (VS.full v)
    fun foldl f z v = VS.foldl f z (VS.full v)
    fun foldr f z v = VS.foldr f z (VS.full v)
    fun findi f v = VS.findi f (VS.full v)
    fun find f v = VS.find f (VS.full v)
    fun exists f v = VS.exists f (VS.full v)
    fun all f v = VS.all f (VS.full v)
    fun collate f (v1, v2) = VS.collate f (VS.full v1, VS.full v2)
    end
  end
