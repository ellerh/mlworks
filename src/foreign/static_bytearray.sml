(*  ==== STATIC BYTEARRAYS ====
 *
 *  Copyright (C) 1995 Harlequin Ltd.
 *
 *  Description
 *  -----------
 *  Static bytearrays are static versions of bytearray values - that
 *  is, once created, they remain in a fixed position within memory
 *  until they are no longer referenced - at which point they can be
 *  GC'd (see rts/src/alloc.[ch] and rts/src/bytearrays.c)
 *
 *  Otherwise, they have many of the properties of standard bytearrays,
 *  plus having a well-defined base address.
 *  
 *
 *  Revision Log
 *  ------------
 *  $Log: static_bytearray.sml,v $
 *  Revision 1.7  1996/09/20 15:41:46  io
 *  current naming conventions
 *
 * Revision 1.6  1996/09/20  14:49:02  io
 * [Bug #1603]
 * convert ByteArray to Internal.ByteArray
 *
 * Revision 1.5  1996/04/18  17:02:14  jont
 * initbasis becomes basis
 *
 * Revision 1.4  1996/03/28  13:32:49  matthew
 * Sharing constraints
 *
 * Revision 1.3  1995/04/20  01:12:47  brianm
 * General updating to reach prototype level for ML FI.
 *
 * Revision 1.2  1995/03/24  15:12:12  brianm
 * Added address_of operator to static_bytearrays and also
 * simplified implementation (mostly casts now).
 *
 * Revision 1.1  1995/03/17  16:07:37  brianm
 * new unit
 * New file.
 *
 *)

require "../basis/__word8";
require "../basis/__word32";



signature STATIC_BYTEARRAY =
  sig

    eqtype bytearray = MLWorks.Internal.ByteArray.bytearray

    eqtype address = Word32.word

    eqtype word8 = Word8.word

    eqtype static_bytearray

    val array           : int * word8 -> static_bytearray
    val length          : static_bytearray -> int
    val update          : static_bytearray * int * word8 -> unit
    val sub             : static_bytearray * int -> word8
    val array_of_list   : word8 list -> static_bytearray
    val tabulate        : int * (int -> word8) -> static_bytearray

    val to_bytearray    : static_bytearray -> bytearray
        (* This is a pure coercion - in particular, _no_ copy is made. *)

    val address_of      : static_bytearray * int -> address

    val from_list       : word8 list -> static_bytearray
    val to_list         : static_bytearray -> word8 list

    val from_string     : string -> static_bytearray
    val to_string       : static_bytearray -> string

    val alloc_array     : int -> static_bytearray
        (* This allocates *uninitialised* static_bytearray values *)
  end;
