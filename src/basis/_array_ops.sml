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
		  val array0 : unit -> 'a array
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
    structure A = Arr
    structure V = Vec
    structure VS = VecSlice

    fun fromList [] = A.array0 ()
      | fromList (first :: rest) =
	let val len = 1 + length rest
	    val result = A.array (len, first)
	    fun loop [] i = result
	      | loop (x :: xs) i =
		(A.unsafeUpdate (result, i, x);
		 loop xs (i + 1))
	in loop rest 1 end

    fun tabulate (0, _) = A.array0 ()
      | tabulate (len, f) =
	let val result = A.array (len, f 0)
	    fun loop i =
	      if i = len then result
	      else (A.unsafeUpdate (result, i, f i);
		    loop (i + 1))
	in loop 1 end

    fun vector a = V.tabulate (A.length a, fn i => A.unsafeSub (a, i))

    local
	structure AS = ArraySlice (type 'a elt = 'a elt
				   type 'a vector = 'a vector
				   structure Arr = Arr
				   structure Vec = Vec
				   structure VecSlice = VecSlice)
    in
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
