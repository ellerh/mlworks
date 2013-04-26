(*

Result: OK
 
$Log: array_arrayoflist.sml,v $
Revision 1.4  1997/05/28 11:28:14  jont
[Bug #30090]
Remove uses of MLWorks.IO

 * Revision 1.3  1996/05/08  11:06:15  jont
 * Arrays and Vectors have moved to MLWorks.Internal
 *
 * Revision 1.2  1996/05/01  17:27:09  jont
 * Fixing up after changes to toplevel visible string and io stuff
 *
 * Revision 1.1  1993/03/29  11:55:57  jont
 * Initial revision
 *

Copyright (c) 1993 Harlequin Ltd.
*)

val a = MLWorks.Internal.ExtendedArray.arrayoflist[1,2,3,4,5,1,2,3,4,5]

val b = MLWorks.Internal.ExtendedArray.length a

val c = MLWorks.Internal.ExtendedArray.sub(a, 7)

val _ = print(if b = 10 then "Pass1\n" else "Fail1\n")

val _ = print(if c = 3 then "Pass2\n" else "Fail2\n")
