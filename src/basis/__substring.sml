(*  ==== INITIAL BASIS : structure SUBSTRING ===
 *
 * Copyright 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Description
 * -----------
 *  This is part of the extended Initial Basis. 
 *
 * $Log: __substring.sml,v $
 * Revision 1.11  1997/08/11 09:06:20  brucem
 * [Bug #30094]
 * Add span.
 *
 *  Revision 1.10  1996/10/03  15:05:46  io
 *  [Bug #1614]
 *  remove redundant requires
 *
 *  Revision 1.9  1996/10/02  20:22:43  io
 *  [Bug #1630]
 *  fix typo in sub for raising Subscript
 *
 *  Revision 1.8  1996/07/29  23:40:36  io
 *  [Bug #1509]
 *  blotched checkin
 *
 *  Revision 1.7  1996/07/29  20:21:02  io
 *  [Bug #1509]
 *  typo in size check in isPrefix
 *
 *  Revision 1.6  1996/07/29  16:07:01  io
 *  [Bug #1508]
 *  Add structure String
 *
 *  Revision 1.5  1996/06/07  18:07:09  io
 *  fix split[lr] & position
 *
 *  Revision 1.4  1996/06/07  15:26:00  io
 *  fix tokens, fields
 *
 *  Revision 1.3  1996/06/04  17:56:57  io
 *  removing __pre_char
 *
 *  Revision 1.2  1996/05/23  20:06:22  io
 *  ** No reason given. **
 *
 *  Revision 1.1  1996/05/17  13:59:40  io
 *  new unit
 *
 *)

require "substring";
require "__string";
require "__char";
require "__char_vector_slice";
require "__int";

structure Substring :> SUBSTRING
                         where type substring = CharVectorSlice.slice
		(* FIXME: where type string = String.string
                         where type char = Char.char *) =
  struct
    structure String = String
    structure S = CharVectorSlice

    type substring = S.slice
    type char = Char.char
    type string = String.string

    val base = S.base
    val string = String.substring o S.base
    fun concat ssl = (String.concat o (map string)) ssl
    fun substring (s, i, len) = S.slice (s, i, SOME len)
    val extract = S.slice
    (* FIXME: obsolete *)
    val all = S.full
    val isEmpty = S.isEmpty
    val getc = S.getItem

    (* val first = Option.map #1 o S.getItem *)
    fun first ss =
      case S.getItem ss of
	  SOME (c, _) => SOME c
	| NONE => NONE

    fun triml k =
      if k < 0 then raise Subscript
      else fn ss => S.subslice (ss, 0, SOME (Int.max (k, S.length ss)))

    fun trimr k =
      if k < 0 then raise Subscript
      else fn ss => let val len = S.length ss
			val k = Int.max (k, len)
		    in S.subslice (ss, k, SOME (len - k)) end

    val slice = S.subslice
    val sub = S.sub
    fun explode ss = S.foldr (op ::) [] ss
    val foldl = S.foldl
    val foldr = S.foldr
    val app = S.app

    (* slow versions: needs work *)
    fun collate p (ss1, ss2) =
      String.collate p (string ss1, string ss2)
    fun compare (ss1, ss2) =
      String.compare (string ss1, string ss2)

    fun splitAt (ss, i) =
      (S.subslice (ss, 0, SOME i),
       S.subslice (ss, i, NONE))

    fun splitl p ss =
      let
	val (s, ii, nn) = S.base ss
        val sz = ii+nn
        fun scan j =
          if j < sz andalso p (String.sub (s, j)) then
            scan (j+1)
          else
            j
        val res = (scan ii) - ii
      in
        splitAt (ss, res)
      end

    fun splitr p ss =
      let
	val (s, ii, nn) = S.base ss
        val sz = ii+nn
        val exit = ii-1
        fun scan j =
          if j > exit andalso p (String.sub (s, j)) then
            scan (j-1)
          else
            j
        val res = ((scan (sz-1)) - ii)+1
      in
        splitAt (ss, res)
      end

    fun isPrefix p ss =
      let
	val (s, i, slen) = S.base ss
	val plen = String.size p
      in
	if plen > slen then false
	else
	  let fun scan pi si =
		pi = plen orelse (String.sub (p, pi) = String.sub (s, si)
				  andalso scan (pi + 1) (si + 1))
	  in
	    scan 0 i
	  end
      end

    fun fields p ss =
      let
	val (s,ii,nn) = S.base ss
        val sz = ii+nn
        fun substr (i, j, acc) = S.slice (s, i, SOME (j-i)) :: acc
        fun scan (i, j, acc) =
          if j < sz then
            (if p (String.sub (s, j)) then
               scan (j+1, j+1, substr(i, j, acc))
             else
               scan (i, j+1, acc))
          else
            substr (i, j, acc)
      in
        rev (scan (ii, ii, []))
      end

    fun translate f ss =
	String.concat (S.foldr (fn (c, list) => f c :: list) [] ss)

    fun dropl (p:char -> bool) ss : substring = #2(splitl p ss)
    fun takel (p:char -> bool) ss : substring = #1(splitl p ss)
    fun taker (p:char -> bool) ss : substring = #2(splitr p ss)
    fun dropr (p:char -> bool) ss : substring = #1(splitr p ss)

    fun position t ss =
      let
	val (s, ii, nn) = S.base ss
        val size = String.size t
        val sz = ii+nn-size

        fun compare (i, j) =
          if i < size then
            (if String.sub(t, i)=String.sub(s, j) then
               compare (i+1, j+1)
             else
               false)
          else
            true

        fun scan j =
          if j <= sz then
            (if compare (0, j) then
               j
             else
               scan (j+1))
          else
            ii+nn
        val res = (scan ii) - ii
      in
        splitAt (ss, res)
      end

    fun tokens p ss =
      let
	val (s,ii,nn) = S.base ss
        val sz = ii+nn
        fun substr (acc, x, y) =
          if x=y then acc else S.slice (s, x, SOME (y-x))::acc
        fun skipSep (acc,x) =
          if x < sz then
            if p (String.sub (s, x)) then
              skipSep(acc, x+1)
            else
              aux (acc, x, x+1)
          else
            acc
        and aux (acc, x, y) =
          if y < sz then
            if p (String.sub (s, y)) then
              skipSep (substr(acc, x, y), y+1)
            else
              aux (acc, x, y+1)
          else
            substr (acc, x, y)
      in
        rev (aux ([], ii, ii))
      end

    val size = S.length

    exception Span

    fun span (ss1, ss2) =
      let val (s1, i1, n1) = S.base ss1
	  val (s2, i2, n2) = S.base ss2
      in
          if s1 = s2 andalso i1 <= i2 + n2
          then S.slice (s1, i1, SOME (i2 + n2 - i1))
          else raise Span
      end

  end
