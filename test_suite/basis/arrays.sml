(*  ==== Testing ====
 *
    Result: OK
 *
 *
 *  Revision Log
 *  ------------
 *  $Log: arrays.sml,v $
 *  Revision 1.7  1997/11/21 10:43:08  daveb
 *  [Bug #30323]
 *
 *  Revision 1.6  1997/08/11  09:35:45  brucem
 *  [Bug #30086]
 *  Stop printing structure contents to prevent spurious failure.
 *
 *  Revision 1.5  1997/05/28  11:00:08  jont
 *  [Bug #30090]
 *  Remove uses of MLWorks.IO
 *
 *  Revision 1.4  1996/11/06  13:35:04  andreww
 *  [Bug #1711]
 *  real no longer eqtype.
 *
 *  Revision 1.3  1996/11/06  12:04:41  matthew
 *  [Bug #1728]
 *  __integer becomes __int
 *
 *  Revision 1.2  1996/10/22  13:18:21  jont
 *  Remove references to toplevel
 *
 *  Revision 1.1  1996/05/22  12:27:57  matthew
 *  new unit
 *  New test
 *
*)

(* test/array.sml -- some test cases for Array 
   PS 1994-12-10, 1995-06-14, 1995-11-07 
   modified for MLWorks Fri May 17 10:46:08 1996
*)

Shell.Options.set(Shell.Options.Language.oldDefinition,true);


infix 1 seq
fun e1 seq e2 = e2;
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

local 
    open Array 
    infix 9 sub
    val array0 = fromList []
    val extract = ArraySlice.vector o ArraySlice.slice
in

infix ==
fun a == b =
  Vector.length a = Vector.length b
  andalso
  let
    fun scan i =
      if i = Vector.length a
        then true
      else
        Vector.sub (a,i) = Vector.sub (b,i)
        andalso scan (i+1)
  in
    scan 0
  end

val a = fromList [0.1,1.1,2.1,3.1,4.1,5.1,6.1];
val b = fromList [44.1,55.1,66.1];
val c = fromList [0.1,1.1,2.1,3.1,4.1,5.1,6.1];

val test1 = check'(fn () => a<>c);
val test2 = 
    check'(fn () => 
	   array(0, 1.1) <> array0
	   andalso array(0,()) <> tabulate(0, fn _ => ())
	   andalso tabulate(0, fn _ => ()) <> fromList [] 
	   andalso fromList [] <> fromList [] 
	   andalso array(0, ()) <> array(0, ())
	   andalso tabulate(0, fn _ => ()) <> tabulate(0, fn _ => ()));

val d = tabulate(100, fn i => real (i mod 7) + 0.1);

val test3 = 
    check'(fn () => d sub 27 = 6.1);

val test4a = (tabulate(maxLen+1, fn i => i) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";
val test4b = (tabulate(~1, fn i => i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4c = 
    check'(fn () => length (tabulate(0, fn i => i div 0)) = 0);

val test5a = 
    check'(fn () => length (fromList []) = 0 andalso length a = 7);
val test5b = 
    check'(fn () => length array0 = 0);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn () => c sub 0 = 0.1);

val e = array(203, 0.0);
val _ = (copy{src=d, dst=e, di=0};
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b});

fun a2v a = vector a
val ev = Vector.concat [a2v d, a2v b, a2v d]; (* length e = 203 *)

val test7 = check'(fn () => length e = 203);

val test8a = (update(e, ~1, 9.9) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test8b = (update(e, length e, 9.9) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val f = extract (e, 100, SOME 3);

val test9 = check'(fn () => f = a2v b);

val test9a = 
    check'(fn () => ev = extract(e, 0, SOME (length e))
	   andalso ev = extract(e, 0, NONE));
val test9b = 
    check'(fn () => Vector.fromList [] == extract(e, 100, SOME 0));
val test9c = (extract(e, ~1, SOME (length e))  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9d = (extract(e, length e+1, SOME 0) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9e = (extract(e, 0, SOME (length e+1)) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9f = (extract(e, 20, SOME ~1)        seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9g = (extract(e, ~1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9h = (extract(e, length e+1, NONE) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9i = 
    check'(fn () => a2v (fromList []) == extract(e, length e, SOME 0)
	    andalso a2v (fromList []) == extract(e, length e, NONE));
val test9j =
    check'(fn () => extract(e, 3, SOME(length e - 3)) = extract(e, 3, NONE));

val _ = copy{src=e, dst=e, di=0};
val g = array(203, 999999.9);
val _ = copy{src=e, dst=g, di=0};

val test10a = check'(fn () => a2v e = ev)
val test10b = check'(fn () => a2v g = ev)

val test10i = 
    check'(fn () => (copy{src=array0, dst=array0, di=0}; 
		     array0 <> array(0, 99999.9)));

val test11b = (copy{src=g, dst=g, di= ~1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11d = (copy{src=g, dst=g, di=1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"

local 
    val v = ref 0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + i;
    fun addvi (i, c) = v := c + i + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val inplist = [7,9,13];
in 
    val inp = fromList inplist
    val pni = fromList (rev inplist)
    fun copyinp a = copy {src=inp, dst=a, di=0}

val array0 = fromList [] : int array;

val test12a =
    check'(fn _ =>
	           foldl cons [1,2] array0 = [1,2]
	   andalso foldl cons [1,2] inp = [13,9,7,1,2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = 13));

val test12b =
    check'(fn _ =>
	           foldr cons [1,2] array0 = [1,2]
	   andalso foldr cons [1,2] inp = [7,9,13,1,2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = 7));

(*
val test12c =
    check'(fn _ =>
	           find (fn _ => true) array0 = NONE
	   andalso find (fn _ => false) inp = NONE
	   andalso find (fn x => x=7) inp = SOME 7
	   andalso find (fn x => x=9) inp = SOME 9
	   andalso (setv 0; find (fn x => (addv x; x=9)) inp; !v = 7+9));
*)
val test12d = 
    check'(fn _ =>
           (setv 117; app setv array0; !v = 117)
	   andalso (setv 0; app addv inp; !v = 7+9+13)
	   andalso (app setv inp; !v = 13));

val test12e = 
    let val a = array(length inp, inp sub 0)
    in 
	check'(fn _ =>
           (modify (~ : int -> int) array0; true)
	   andalso (copyinp a; modify ~ a; foldr (op::) [] a = map ~ inplist)
	   andalso (setv 117; modify (fn x => (setv x; 37)) a; !v = ~13))
    end

val test_foldli_1 = check' (fn _ => foldli consi [] array0 = [])
val test_foldli_2 = check' (fn _ => foldli consi [] inp = [(2,13),(1,9),(0,7)])

val test_foldri_1 = check' (fn _ => foldri consi [] array0 = [])
val test_foldri_2 = check' (fn _ => foldri consi [] inp = [(0,7),(1,9),(2,13)])

(*
val test14a =
    check'(fn _ =>
	   findi (fn _ => true) (array0, 0, NONE) = NONE
   andalso findi (fn _ => false) (inp, 0, NONE) = NONE
   andalso findi (fn (i, x) => x=9 orelse 117 div (2-i) = 0) (inp, 0, NONE)
	   = SOME (1,9));

val test14b =
    check'(fn _ =>
	   findi (fn _ => true) (array0, 0, SOME 0) = NONE
   andalso findi (fn _ => false) (inp, 0, NONE) = NONE
   andalso findi (fn (i, x) => x=9 orelse 117 div (2-i) = 0) (inp, 0, NONE)
	   = SOME (1,9));

val test14c = (findi (fn _ => true) (inp, ~1, NONE) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
val test14d = (findi (fn _ => true) (inp, 4, NONE) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
val test14e = (findi (fn _ => true) (inp, ~1, SOME 2) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
val test14f = (findi (fn _ => true) (inp, 4, SOME 0) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
val test14g = (findi (fn _ => true) (inp, 0, SOME 4) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
val test14h = (findi (fn _ => true) (inp, 2, SOME ~1) seq "WRONG")
           handle Subscript => "OK" | _ => "WRONG";
*)
val test15a =
    check'(fn _ =>
           (setvi (0,117); appi setvi array0; !v = 117)
	   andalso (setvi (0,0); appi addvi inp; !v = 0+7+1+9+2+13)
	   andalso (appi setvi inp; !v = 2+13));


local
    val a = array(length inp, inp sub 0);
in
val test_modifyi_1 = check'(fn _ => (modifyi (op +) array0;
				     foldr (op::) [] array0 = []))

val test_modifyi_2 = check'(fn _ => (copyinp a;
				     modifyi (op -) a;
				     foldr (op::) [] a = [~7,~8,~11]));
val test_modifyi_3 = check' (fn _ =>
				(copyinp a;
				 setv 117;
				 modifyi (fn x => (setvi x; 37)) a;
				 !v = 2+13));

val test_modifyi_4 = check' (fn _ =>
				(copyinp a;
				 setv 100;
				 modifyi (fn x => !v before setvi (!v, 1)) a;
				 foldr (op::) [] a = [100, 101, 102]));

end

end
end
