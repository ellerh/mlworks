(* Copyright (C) 1997 The Harlequin Group Limited.  All rights reserved.
 *
 * The signature that (almost) all pointer types should satisfy.
 *
 * Revision Log
 * ------------
 * $Log: mlworks_c_pointer.sml,v $
 * Revision 1.2  1997/07/03 09:40:56  stephenb
 * Automatic checkin:
 * changed attribute _comment to ' *  '
 *
 *)


require "$.basis.__word";

signature MLWORKS_C_POINTER =
  sig
    type 'a ptr


    (*
     * value is the type of object that the pointer points at.
     *)
    type value



    (*
     * size specifies the size of a value in bytes.
     *)
    val size : Word.word



    (*
     * ! dereferences the pointer to return the value it points at.
     *
     * Assuming the existence of the following C type declarations assuming
     * that value is defined elsewhere :-
     *
     *   typedef value * ptr;
     *   ptr   vp;
     *
     * then `:=' is equivalent to doing :-
     *
     *   *vp;
     *
     *)
    val ! : value ptr -> value



    (*
     * `:=' alters the value pointed at by the pointer to be the new value.
     *
     * Assuming the existence of the following C type declarations assuming
     * that `value' is defined elsewhere :-
     *
     *   typedef value * ptr;
     *   value v;
     *   ptr   vp;
     *
     * then `:=' is equivalent to doing :-
     *
     *   *vp= v;
     *
     *)
    val := : value ptr * value -> unit



    (*
     * make allocates (with malloc) enough space for a value and
     * returns a pointer to it.  
     *
     * make returns null if malloc cannot allocate enough space.
     *
     * Assuming the existence of the following C type declarations assuming
     * that value is defined elsewhere :-
     *
     *   typedef value * ptr;
     *
     * then make is equivalent to the following C function :-
     *
     *   ptr * make()
     *   {
     *     return (ptr)malloc(sizeof(value));
     *   }
     *)
    val make : unit -> value ptr



    (* makeArray n
     *
     * makeArray allocates (with malloc) enough space for n values and
     * returns a pointer to the first one.
     *
     * makeArray returns null if malloc cannot allocate enough space or if
     * the requested number is less than or equal to 0.
     *
     * Assuming the existence of the following C type declarations assuming
     * that value is defined elsewhere :-
     *
     *   typedef value * ptr;
     *
     * then makeArray is equivalent to the following C function :-
     *
     *   ptr * makeArray(int n)
     *   {
     *     if (n < 0)
     *       return NULL;
     *     else
     *      return (ptr)malloc(sizeof(value)*n);
     *   }
     *)
    val makeArray : int -> value ptr



    (*
     * free deallocates the space (using free from the C library) pointed
     * to by the pointer.  The result is undefined if :-
     *
     *  - the space pointed to the pointer was not indirectly allocated
     *    with malloc, realloc or calloc from the C library.
     *  - the pointer points to space which has already been passed to
     *    free.
     *)
    val free : value ptr -> unit



    (* next p offset
     * prev p offset
     *
     * next and prev provide a way to perform limited pointer arithmetic.
     * next adds offset to p to form another pointer and sub subtracts
     * offset from p to form another pointer.  The offset is scaled by
     * the size of the value being pointed at so an offset of 0 returns
     * the original pointer, an offset of 1 returns a pointer to the next
     * (previous) value.  When dealing with arrays, next can be combined
     * with ! and := to define equivalents of the sub and update functions
     * that are available in SML arrays :-
     *
     *   fun sub (p, n) = ! (next (p, n))
     *   fun update (p, v, n) = (next (p, n)) := v
     *)
    val next : value ptr * Word.word -> value ptr
    val prev : value ptr * Word.word -> value ptr
  end
