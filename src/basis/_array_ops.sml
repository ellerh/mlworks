(* _array_ops.sml --- ArrayOps functor
 *
 * This code has been placed in the public domain.
 *
 ** Commentary:
 *
 * This functor is used to implement various functions in of ARRAY
 * and MONO_ARRAY.
 *
 *)

require "_array_slice";

functor ArrayOps (
    type 'a elt
    type 'a vector
    structure Arr : sig
		  eqtype 'a array
		  val length : 'a array -> int
		  val tabulate : int * (int -> 'a elt) -> 'a array
		  val array : int * 'a elt -> 'a array
		  val unsafeSub : 'a array * int -> 'a elt
		  val unsafeUpdate : 'a array * int * 'a elt -> unit
	      end
    structure Vec : sig
		  val tabulate : int * (int -> 'a elt) -> 'a vector
		  val unsafeSub : 'a vector * int -> 'a elt
	      end
    structure VecSlice : sig
		  type 'a slice
		  val base : 'a slice -> 'a vector * int * int
	      end) =
  struct
    local
	structure A = Arr
	structure V = Vec
	structure AS = ArraySlice (type 'a elt = 'a elt
				   type 'a vector = 'a vector
				   structure Arr = Arr
				   structure Vec = Vec
				   structure VecSlice = VecSlice)
    in

    fun fromList (l as []) = A.tabulate (0, fn _ => hd l)
      | fromList (first :: rest) =
	let val len = 1 + length rest
	    val result = A.array (len, first)
	    fun loop [] i = result
	      | loop (x :: xs) i =
		(A.unsafeUpdate (result, i, x);
		 loop xs (i + 1))
	in loop rest 1 end

    fun vector a = V.tabulate (A.length a, fn i => A.unsafeSub (a, i))

    fun copy {src, dst, di} = AS.copy {src = AS.full src, dst = dst, di = di}

    fun appi f a = AS.appi f (AS.full a)
    fun app  f a = AS.app f (AS.full a)
    fun modifyi f a = AS.modifyi f (AS.full a)
    fun modify f a = AS.modify f (AS.full a)
    fun foldli f z a = AS.foldli f z (AS.full a)
    fun foldri f z a = AS.foldri f z (AS.full a)
    fun foldl f z a = AS.foldl f z (AS.full a)
    fun foldr f z a = AS.foldr f z (AS.full a)
    fun findi f a = AS.findi f (AS.full a)
    fun find f a = AS.find f (AS.full a)
    fun exists f a = AS.exists f (AS.full a)
    fun all f a = AS.all f (AS.full a)
    fun collate f (a1, a2) = AS.collate f (AS.full a1, AS.full a2)
    end
  end