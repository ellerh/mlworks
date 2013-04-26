(* strnames.sml the signature *)
(*
$Log: strnames.sml,v $
Revision 1.10  1995/03/28 16:20:38  matthew
Use Stamp instead of Tyname_id etc.

Revision 1.9  1993/02/08  13:18:09  matthew
Removed open Datatypes

Revision 1.8  1992/10/30  15:16:42  jont
Added special maps for tyfun_id, tyname_id, strname_id

Revision 1.7  1992/08/27  20:15:30  davidt
Yet more changes to get structure copying working better.

Revision 1.6  1992/08/27  18:37:53  davidt
Made various changes so that structure copying can be
done more efficiently.

Revision 1.5  1992/07/17  15:53:39  jont
Changed to use btrees for renaming of tynames and strnames

Revision 1.4  1992/07/04  17:16:06  jont
Anel's changes for improved structure copying

Revision 1.3  1991/11/21  16:55:11  jont
Added copyright message

Revision 1.2  91/11/19  12:19:22  jont
*** empty log message ***

Revision 1.1.1.1  91/11/19  11:13:07  jont
Added comments for DRA on functions

Revision 1.1  91/06/07  11:46:06  colin
Initial revision

Copyright (c) 1991 Harlequin Ltd.
*)

(* the type Strname (defined in datatypes.sml) corresponds to the
simple object class StrName, as defined in the Definition (p16). They
are resolved (changed from metastrnames to real strnames) in the
process on unification. This module provides simple functions for
dealing with strnames. *)

require "../typechecker/datatypes";

signature STRNAMES =
  sig
    structure Datatypes : DATATYPES

    val uninstantiated : Datatypes.Strname -> bool
    val strname_eq : Datatypes.Strname * Datatypes.Strname -> bool
    val metastrname_eq : Datatypes.Strname * Datatypes.Strname -> bool
    val strname_ord : Datatypes.Strname * Datatypes.Strname -> bool
    val strip : Datatypes.Strname -> Datatypes.Strname
    val string_strname : Datatypes.Strname -> string

    (* The bool indicates if a flexible strname should be copied as a rigid name *)
    val create_strname_copy :
      bool ->
      Datatypes.Strname Datatypes.StampMap * Datatypes.Strname -> 
      Datatypes.Strname Datatypes.StampMap

    val strname_copy :
      Datatypes.Strname * Datatypes.Strname Datatypes.StampMap-> Datatypes.Strname
  end
