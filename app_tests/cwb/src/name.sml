(*
 *
 * $Log: name.sml,v $
 * Revision 1.2  1998/06/02 15:44:34  jont
 * Automatic checkin:
 * changed attribute _comment to ' * '
 *
 *
 *)
(* 	$Id: name.sml,v 1.2 1998/06/02 15:44:34 jont Exp $	 *)
(* name.sml
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories
 *
 * AUTHOR:	John Reppy
 *		AT&T Bell Laboratories
 *		Murray Hill, NJ 07974
 *		jhr@research.att.com
 *)
(* MODIFICATION: *)
(* Perdita added hashOf. This breaks the encapsulation,                *)
(* but is a win since we often want to plug these hash values into     *)
(* hash values for bigger things, and it saves recalculating them. *)

structure Name : NAME =
  struct

  (* local definition of app *)
    fun app f = let
	  fun appF [] = ()
	    | appF (x::r) = (ignore(f x); appF r)
	  in
	    appF
	  end

  (* unique names *)
    datatype name = NAME of {
	hash : int,
	id : string
      }

    fun stringOf (NAME{id, ...}) = id
(* CAUTION: broken encapsulation! *)
    fun hashOf (NAME{hash, ...}) = hash

    fun sameName (NAME{hash=h1, id=id1}, NAME{hash=h2, id=id2}) =
	  (h1 = h2) andalso (id1 = id2)

  (* the unique name hash table *)
    val tableSz = 64
    val table = ref(Array.array(tableSz, [] : name list))
    val numItems = ref 0

    fun mkName s = let
	  val h = HashString.hashString s
	  fun isName (NAME{hash, id}) = (hash = h) andalso (id = s)
	  fun mk () = let
		val tbl = !table
		val sz = Array.length tbl
	      (* grow the table by doubling its size *)
		fun growTable () = let
		      val newSz = sz+sz
		      val newMask = newSz-1
		      val newTbl = Array.array(newSz, [])
		      fun ins (item as NAME{hash, ...}) = let
			    val indx = Bits.andb(hash, newMask)
			    in
			      Array.update (newTbl, indx,
				item :: Array.sub(newTbl, indx))
			    end
		      val appins = app ins
		      fun insert i = (appins (Array.sub(tbl, i)); insert(i+1))
		      in
			(insert 0) handle _ => ();
			table := newTbl
		      end
		in
		  if (!numItems >= sz)
		    then (growTable(); mk())
		    else let
		      val indx = Bits.andb(h, sz-1)
		      fun look [] = let
			    val newName = NAME{hash = h, id = s}
			    in
			      numItems := !numItems + 1;
			      Array.update(tbl, indx, newName :: Array.sub(tbl, indx));
			      newName
			    end
			| look (name::r) = if (isName name) then name else look r
		      in
			look (Array.sub(tbl, indx))
		      end
		end
	  in
	    mk()
	  end


    structure Tbl = HashTable(struct
	type hash_key = name
	fun hashVal (NAME{hash, id}) = hash
	fun sameKey (NAME{hash=h1, id=id1}, NAME{hash=h2, id=id2}) =
	      ((h1 = h2) andalso (id1 = id2))
      end);

    type 'a name_tbl = 'a Tbl.hash_table
    val mkNameTbl = Tbl.mkTable
    open Tbl

  end (* Name *)
