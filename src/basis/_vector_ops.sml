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
		 val tabulate : int * (int -> 'a elt) -> 'a seq
		 val unsafeSub : 'a seq * int -> 'a elt
	     end) =
  struct

    fun fromList l =
      let val cache = ref {i = 0, l = l}
	  fun lookup d =
	    let val {i, l} = !cache
	    in if d = i then
		   let val x :: xs = l
		   in cache := {i = i + 1, l = xs};
		      x
		   end
	       else raise Fail "non-sequential tabulate (?)"
	    end
      in V.tabulate (length l, lookup) end

    fun update (v, i, x) =
      V.tabulate (V.length v, fn d => if d = i then x else V.unsafeSub (v, d))

    fun concat (l as []) = V.tabulate (0, fn _ => V.unsafeSub (hd l, 0))
      | concat [v] = v
      | concat (l as v :: vs) =
	let fun count [] sum = sum
	      | count (v :: vs) sum = count vs (sum + (V.length v))
	    val total = count l 0
	    val cache = ref {start = 0, stop = V.length v, v = v, vs = vs}
	    fun lookup i =
	      let val {start, stop, v, vs} = !cache
	      in
		  if start <= i andalso i < stop then
		      V.unsafeSub (v, i - start)
		  else if i = stop then
		      let val v :: vs = vs
		      in
			  cache := {start = stop, stop = stop + V.length v,
				    v = v, vs = vs};
			  lookup i
		      end
		  else raise Fail "non-sequential tabulate?"
	      end
	in V.tabulate (total, lookup) end

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
