(*  ==== Testing RealArrays ====
 *
    Result: OK
 *
 *
 *  Revision Log
 *  ------------
 *  $Log: real_arrays.sml,v $
 *  Revision 1.3  1997/11/21 10:48:04  daveb
 *  [Bug #30323]
 *
 *  Revision 1.2  1997/05/28  15:55:15  matthew
 *  Updating
 *
 *  Revision 1.1  1997/01/30  15:10:55  andreww
 *  new unit
 *  Adapting Sestoft's array tests for real arrays.
 *
 *
*)

(* test/array.sml -- some test cases for Array 
   PS 1994-12-10, 1995-06-14, 1995-11-07 
   modified for MLWorks Fri May 17 10:46:08 1996
*)


infix 1 seq
fun e1 seq e2 = e2;
fun say s = print s
fun check b = if b then "OK" else "WRONG";
fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN";

fun range (from, to) p = 
    let open Int
    in
	(from > to) orelse (p from) andalso (range (from+1, to) p)
    end;

fun checkrange bounds = check o range bounds;

local 
    infix eq
    val op eq = Real.==
    open RealArray 
    infix 9 sub
    val array0 = fromList []
    fun extract (a, start, len) =
      let val l = length a
	  val stop = case len of
			 NONE => l
		      |  SOME l => start + l
      in
	  if 0 <= start andalso start <= stop andalso stop <= l then
	      RealVector.tabulate (stop - start, fn i => a sub (start + i))
	  else raise Subscript
      end
in

infix ==
fun a == b =
  RealVector.length a = RealVector.length b
  andalso
  let
    fun scan i =
      if i = RealVector.length a
        then true
      else
        RealVector.sub (a,i) eq (RealVector.sub (b,i))
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
	   andalso array(0,0.0) <> tabulate(0, fn _ => 0.0)
	   andalso tabulate(0, fn _ => 0.0) <> fromList [] 
	   andalso fromList [] <> fromList [] 
	   andalso array(0, 0.0) <> array(0, 0.0)
	   andalso tabulate(0, fn _ => 0.0) <> tabulate(0, fn _ => 0.0));

val d = tabulate(100, fn i => real (i mod 7) + 0.1);

val test3 = 
    check'(fn () => d sub 27 eq 6.1);

val test4a = (tabulate(maxLen+1, fn i => real i) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";
val test4b = (tabulate(~1, fn i => real i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4c = 
    check'(fn () => length (tabulate(0, fn i => real (i div 0))) = 0);

val test5a = 
    check'(fn () => length (fromList []) = 0 andalso length a = 7);
val test5b = 
    check'(fn () => length array0 = 0);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn () => c sub 0 eq 0.1);

val e = array(203, 0.0);

val _ = (copy{src=d, dst=e, di=0};
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b});

fun a2v a = vector a
val ev = RealVector.concat [a2v d, a2v b, a2v d]; (* length e = 203 *)

val test7 = check'(fn () => length e = 203);
val test7a = check'(fn () => ev == a2v e);

val test8a = (update(e, ~1, 9.9) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test8b = (update(e, length e, 9.9) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val f = extract (e, 100, SOME 3);

val test9 = check'(fn () => f == a2v b);

val test9a = 
    check'(fn () => ev == extract(e, 0, SOME (length e))
	   andalso ev == extract(e, 0, NONE));
val test9b = 
    check'(fn () => RealVector.fromList [] == extract(e, 100, SOME 0));
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
    check'(fn () => extract(e, 3, SOME(length e - 3)) == extract(e, 3, NONE));

val _ = copy{src=e, dst=e, di=0};
val g = array(203, 999999.9);
val _ = copy{src=e, dst=g, di=0};

val test10a = check'(fn () => ev == extract(e, 0, SOME (length e)) 
 		      andalso ev == extract(e, 0, NONE));
val test10b = check'(fn () => ev == extract(g, 0, SOME (length g))
		      andalso ev == extract(g, 0, NONE));

val test10i =
    check'(fn () => (copy{src=array0, dst=array0, di=0};
		     array0 <> array(0, 99999.9)));

val test11b = (copy{src=g, dst=g, di= ~1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11d = (copy{src=g, dst=g, di=1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"

local 
    val v = ref 0.0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + (real i);
    fun addvi (i, c) = v := c + (real i) + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    fun listEq ([],[]) = true
      | listEq (h::t,h'::t') = h eq h' andalso listEq(t,t')
      | listEq _ = false
    fun listEq' ([],[]) = true
      | listEq' ((a,b)::t,(a',b')::t') = 
      a=a' andalso b eq b' andalso listEq'(t,t')
      | listEq' _ = false

    val inplist = [7.0,9.0,13.0]
in 
    val inp = fromList inplist
    val pni = fromList (rev inplist)
    fun copyinp a =
	copy {src=inp, dst=a, di=0}

val array0 = fromList [] : array;

val test12a =
    check'(fn _ =>
	           listEq(foldl cons [1.1,2.2] array0,[1.1,2.2])
	   andalso listEq(foldl cons [1.0,2.0] inp,[13.0,9.0,7.0,1.0,2.0])
	   andalso (foldl (fn (x, _) => setv x) () inp; !v eq 13.0));

val test12b =
    check'(fn _ =>
	           listEq(foldr cons [1.0,2.0] array0,[1.0,2.0])
	   andalso listEq(foldr cons [1.0,2.0] inp, [7.0,9.0,13.0,1.0,2.0])
	   andalso (foldr (fn (x, _) => setv x) () inp; !v eq 7.0));

(*
val test12c =
    check'(fn _ =>
	           find (fn _ => true) array0 = NONE
	   andalso find (fn _ => false) inp = NONE
	   andalso find (fn x => x=7) inp = SOME 7
	   andalso find (fn x => x=9) inp = SOME 9
	   andalso (setv 0.0; find (fn x => (addv x; x=9)) inp; !v eq 7+9));
*)
val test12d = 
    check'(fn _ =>
           (setv 117.0; app setv array0; !v eq 117.0)
	   andalso (setv 0.0; app addv inp; !v eq (7.0+9.0+13.0))
	   andalso (app setv inp; !v eq 13.0));

val test12e = 
    let val a = array(length inp, inp sub 0)
    in 
	check'(fn _ =>
           (modify (~ : real -> real) array0; true)
	   andalso (copyinp a; modify ~ a; 
                    listEq(foldr (op::) [] a,map ~ inplist))
	   andalso (setv 117.0; modify (fn x => (setv x; 37.0)) a;
                    !v eq ~13.0))
    end

val test13a =
    check'(fn _ => listEq'(foldli consi [] array0,[])
	   andalso listEq'(foldri consi [] array0,[])
	   andalso listEq'(foldli consi [] inp,
                          [(2,13.0),(1,9.0),(0,7.0)])
	   andalso listEq'(foldri consi [] inp,
                          [(0,7.0),(1,9.0),(2,13.0)]))
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
           (setvi (0,117.0); appi setvi array0; !v eq 117.0)
	   andalso (setvi (0,0.0); appi addvi inp;
                    !v eq (0.0+7.0+1.0+9.0+2.0+13.0)
	   andalso (appi setvi inp; !v eq (2.0+13.0))));


fun minus(a,b) = real a - b
fun plus (a,b) = real a + b

local
    val a = array(length inp, inp sub 0)
in
val test_update_1 = check' (fn _ => (copyinp a;
				     update (a, 0, 123.34);
				     a sub 0 eq 123.34))
val test_update_2 = check' (fn _ => (copyinp a;
				     update (a, 1, 123.34);
				     a sub 1 eq 123.34))
val test_update_3 = check' (fn _ => (copyinp a;
				     update (a, 2, 123.34);
				     a sub 2 eq 123.34))
val test_update_4 = check' (fn _ => (copyinp a;
				     update (a, 2, real 1 + 7.0);
				     a sub 2 eq 8.0))

val test_modify_1 = check' (fn _ => (copyinp a;
				     modify (fn (x) => x) a;
				     a2v a == a2v inp))

val test_modify_2 = check' (fn _ => let val l = ref []
				    in
					copyinp a;
					modify (fn x => (l := x :: (! l); x))
					       a;
					a2v (fromList (rev (! l))) == a2v inp
				    end)

val test_modifyi_1 = check' (fn _ => (modifyi plus array0; true))
val test_modifyi_2 = check' (fn _ => (copyinp a;
				      modifyi (fn (_, x) => x) a;
				      a2v a == a2v inp))

val test_modifyi_3 = check' (fn _ =>
				(copyinp a; modifyi plus a;
				 listEq(foldr (op::) [] a,[7.0,10.0,15.0])))
val test_modifyi_4 = check' (fn _ =>
				(copyinp a; modifyi minus a;
				 listEq(foldr (op::) [] a,[~7.0,~8.0,~11.0])))
val test_modifyi_5 = check' (fn _ =>
				(copyinp a; modifyi (fn _ => 1.2) a;
				 listEq(foldr (op::) [] a, [1.2, 1.2, 1.2])))

val test_modifyi_6 = check' (fn _ =>
				(copyinp a; setv 117.0;
				 modifyi (fn x => (setvi x; 37.0)) a;
				 !v eq (2.0+13.0)))

val test_modifyi_7 = check' (fn _ => let val l = ref []
				     in
					 copyinp a;
					 modifyi (fn (_, x) =>
						     (l := x :: (! l);
						      x))
						 a;
					 a2v (fromList (rev (! l))) == a2v inp
				    end)

end

end
end
