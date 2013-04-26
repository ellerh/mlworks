(*

Result: OK
 
$Log: array_to_list.sml,v $
Revision 1.4  1997/05/28 11:40:14  jont
[Bug #30090]
Remove uses of MLWorks.IO

 * Revision 1.3  1996/05/08  11:28:42  jont
 * Arrays and Vectors have moved to MLWorks.Internal
 *
 * Revision 1.2  1996/05/01  16:56:48  jont
 * Fixing up after changes to toplevel visible string and io stuff
 *
 * Revision 1.1  1993/03/29  11:51:21  jont
 * Initial revision
 *

Copyright (c) 1993 Harlequin Ltd.
*)

val a = MLWorks.Internal.ExtendedArray.from_list[1,2,3,4,5,1,2,3,4,5]

val _ = case MLWorks.Internal.ExtendedArray.to_list a of
  [1,2,3,4,5,1,2,3,4,5] => print"Pass\n"
| _ => print"Fail\n"
