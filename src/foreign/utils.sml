(*  ==== FOREIGN INTERFACE : UTILITIES ====
 *
 *  Copyright (C) 1995 Harlequin Ltd.
 *
 *  Description
 *  -----------
 *  This is a collection of items that it was useful
 *  to group together.
 *
 *  Revision Log
 *  ------------
 *  $Log: utils.sml,v $
 *  Revision 1.3  1996/10/22 12:00:19  io
 *  [Bug #1547]
 *  updating for current naming conventions
 *
 *  Revision 1.2  1996/05/22  13:46:42  brianm
 *  Beta release modifications.
 *
 *  Revision 1.1  1996/05/19  11:46:39  brianm
 *  new unit
 *  Renamed file..
 *
 * Revision 1.4  1996/03/28  13:50:11  matthew
 * Sharing constraints
 *
 * Revision 1.3  1996/02/14  16:51:37  brianm
 * Additions to allow for different data repn's (e.g. endianness).
 *
 *  Revision 1.2  1995/06/21  16:31:54  brianm
 *  Adding remote access, diagnostics and other facilities.
 *
 *  Revision 1.1  1995/04/25  11:43:41  brianm
 *  new unit
 *  New file.
 *
 *
 *)

require "types";

signature FOREIGN_UTILS =
   sig

     type 'a option = 'a option

     structure FITypes : FOREIGN_TYPES

     type bytearray  = FITypes.bytearray
     type word32     = FITypes.word32
     type address    = FITypes.address


     (* Operations for values *)

     type 'a box =  'a option ref

     val someBox    :  'a box -> bool

     val getBox     :  'a box -> 'a
     val setBox     :  'a box -> 'a -> unit

     val extractBox :  'a box -> 'a option
     val updateBox  :  'a box -> 'a option -> unit

     val resetBox   :  'a box -> unit

     val makeBox    :  'a -> 'a box
     val newBox     :  'a box -> 'a box

     val voidBox    :  unit -> 'a box

     (* Display tools *)

     val disp       : ('a -> string) -> 'a -> 'a
     val sep_items  : 'a -> 'a list -> 'a list
     val term_items : 'a -> 'a list -> 'a list

     val is_big_endian : bool

     (* integer mapping functions - correct byte order for host *)
     val int_to_bytearray : {src:int, len:int, arr:bytearray, st:int} -> unit
     val bytearray_to_int : {arr:bytearray, st:int, len:int} -> int

     val string_to_bytearray : {src:string, arr:bytearray, st:int} -> unit
     val bytearray_to_string : {arr:bytearray, st:int, len:int} -> string
     val bytearray_to_hex    : {arr:bytearray, st:int, len:int} -> string

     val word32_to_bytearray : {src:word32, arr:bytearray, st:int} -> unit
     val word32_to_hex       : word32 -> string
     val bytearray_to_word32 : {arr:bytearray, st:int} -> word32

     val peek_memory : { loc   : address,
                         arr   : bytearray,
                         start : int,
                         len   : int } -> unit

         (* This copies memory from any (mapped) non-overlapping address.
            Checks are performed for array size and for overlapping regions.
          *)

   end;
   
