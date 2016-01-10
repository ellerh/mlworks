(* vector_slice.sml --- Tests for VectorSlice
 *
 * This code has been placed in the public domain.
 *

Result: OK

 *)
local
    fun pr name result = print (name ^ ":" ^ result ^ "\n")

    fun check name f =
      pr name (if f () then "OK" else "WRONG"
	       handle exn => "FAIL:" ^ General.exnName exn)

    fun checkexn name exn f =
      let val result = (ignore(f ()); "FAIL")
		       handle ex =>
			      if General.exnName ex = General.exnName exn then 
				  "OKEXN"
			      else
				  "BADEXN:" ^ (General.exnName ex)
      in
	  pr name result
      end

    structure V = Vector
    structure VS = VectorSlice

    fun l2v l = V.fromList l
    fun v2l v = V.foldr (op ::) [] v
    fun l2s l = VS.full (l2v l)
    fun s2l s = VS.foldr (op ::) [] s

    val a = VS.slice (l2v [~1, 0, 1, 2, 3, 4, 5, 6], 1, SOME 6)
    val b = VS.slice (l2v [~1, 44, 55, 66, 77], 1, SOME 3)
    val c = VS.slice (l2v [~1, 0, 1, 2, 3, 4, 5, 6], 1, SOME 6)
    val d = VS.slice (V.tabulate (102, fn i => i mod 7), 1, SOME 100)
    val e = VS.concat [d, b, d]
    val z = l2s []

in


val _ = check "test_length_1" (fn _ => VS.length a = 6);
val _ = check "test_length_2" (fn _ => VS.length b = 3);
val _ = check "test_length_3" (fn _ => VS.length c = 6);
val _ = check "test_length_4" (fn _ => VS.length d = 100);
val _ = check "test_length_5" (fn _ => V.length e = 203);
val _ = check "test_length_6" (fn _ => VS.length z = 0);

val _ = check "test_sub_1" (fn _ => VS.sub (a, 0) = 0);
val _ = check "test_sub_2" (fn _ => VS.sub (a, 1) = 1);
val _ = check "test_sub_3" (fn _ => VS.sub (a, 2) = 2);
val _ = check "test_sub_4" (fn _ => VS.sub (a, 5) = 5);

val _ = checkexn "test_sub_2_5" (Subscript) (fn _ => VS.sub (a, ~1));
val _ = checkexn "test_sub_2_6" (Subscript) (fn _ => VS.sub (a, ~2));
val _ = checkexn "test_sub_2_7" (Subscript) (fn _ => VS.sub (a, 6));
val _ = checkexn "test_sub_2_8" (Subscript) (fn _ => VS.sub (a, 7));

val _ = (check "test_slice_1") (fn _ => s2l (VS.slice (l2v [], 0, NONE)) = []);
val _ = (check "test_slice_2")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 0, NONE)) = [1, 2, 3]);
val _ = (check "test_slice_3")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 1, NONE)) = [2, 3]);
val _ = (check "test_slice_4")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 2, NONE)) = [3]);
val _ = (check "test_slice_5")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 3, NONE)) = []);
val _ = (check "test_slice_6")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 0, SOME 2)) = [1, 2]);
val _ = (check "test_slice_7")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 1, SOME 2)) = [2, 3]);
val _ = (check "test_slice_8")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 1, SOME 1)) = [2]);
val _ = (check "test_slice_9")
	    (fn _ => s2l (VS.slice (l2v [1, 2, 3], 1, SOME 0)) = []);
val _ = (check "test_slice_10")
	    (fn _ => s2l (VS.slice (l2v [], 0, NONE)) = []);

val _ = (checkexn "test_slice_2_1" Subscript)
	    (fn _ => VS.slice (l2v [], ~1, NONE))
val _ = (checkexn "test_slice_2_2" Subscript)
	    (fn _ => VS.slice (l2v [], ~2, NONE))
val _ = (checkexn "test_slice_2_3" Subscript)
	    (fn _ => VS.slice (l2v [], 1, NONE))
val _ = (checkexn "test_slice_2_4" Subscript)
	    (fn _ => VS.slice (l2v [], 0, SOME 1))
val _ = (checkexn "test_slice_2_5" Subscript)
	    (fn _ => VS.slice (l2v [], 0, SOME 2))
val _ = (checkexn "test_slice_2_6" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], ~1, NONE))
val _ = (checkexn "test_slice_2_7" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 4, NONE))
val _ = (checkexn "test_slice_2_8" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 0, SOME 4))
val _ = (checkexn "test_slice_2_9" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 1, SOME 3))
val _ = (checkexn "test_slice_2_10" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 2, SOME 2))
val _ = (checkexn "test_slice_2_11" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 3, SOME 1))
val _ = (checkexn "test_slice_2_12" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 2, SOME ~1))
val _ = (checkexn "test_slice_2_13" Subscript)
	    (fn _ => VS.slice (l2v [1, 2, 3], 3, SOME ~1))


val _ = (check "test_subslice_1")
	    (fn _ => s2l (VS.subslice (l2s [], 0, NONE)) = []);
val _ = (check "test_subslice_2")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 0, NONE)) = [1, 2, 3]);
val _ = (check "test_subslice_3")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 1, NONE)) = [2, 3]);
val _ = (check "test_subslice_4")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 2, NONE)) = [3]);
val _ = (check "test_subslice_5")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 3, NONE)) = []);
val _ = (check "test_subslice_6")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 0, SOME 2)) = [1, 2]);
val _ = (check "test_subslice_7")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 1, SOME 2)) = [2, 3]);
val _ = (check "test_subslice_8")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 1, SOME 1)) = [2]);
val _ = (check "test_subslice_9")
	    (fn _ => s2l (VS.subslice (l2s [1, 2, 3], 1, SOME 0)) = []);
val _ = (check "test_subslice_10")
	    (fn _ => s2l (VS.subslice (l2s [], 0, NONE)) = []);

val _ = (checkexn "test_subslice_2_1" Subscript)
	    (fn _ => VS.subslice (l2s [], ~1, NONE))
val _ = (checkexn "test_subslice_2_2" Subscript)
	    (fn _ => VS.subslice (l2s [], ~2, NONE))
val _ = (checkexn "test_subslice_2_3" Subscript)
	    (fn _ => VS.subslice (l2s [], 1, NONE))
val _ = (checkexn "test_subslice_2_4" Subscript)
	    (fn _ => VS.subslice (l2s [], 0, SOME 1))
val _ = (checkexn "test_subslice_2_5" Subscript)
	    (fn _ => VS.subslice (l2s [], 0, SOME 2))
val _ = (checkexn "test_subslice_2_6" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], ~1, NONE))
val _ = (checkexn "test_subslice_2_7" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 4, NONE))
val _ = (checkexn "test_subslice_2_8" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 0, SOME 4))
val _ = (checkexn "test_subslice_2_9" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 1, SOME 3))
val _ = (checkexn "test_subslice_2_10" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 2, SOME 2))
val _ = (checkexn "test_subslice_2_11" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 3, SOME 1))
val _ = (checkexn "test_subslice_2_12" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 2, SOME ~1))
val _ = (checkexn "test_subslice_2_13" Subscript)
	    (fn _ => VS.subslice (l2s [1, 2, 3], 3, SOME ~1))

val _ = (check "test_base_1")
	    (fn _ => VS.base (VS.slice (l2v [], 0, NONE)) = (l2v [], 0, 0));

val _ = (check "test_base_2")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 0, NONE))
		     = (l2v [1, 2, 3], 0, 3));

val _ = (check "test_base_3")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 1, NONE))
		     = (l2v [1, 2, 3], 1, 2));

val _ = (check "test_base_4")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 2, NONE))
		     = (l2v [1, 2, 3], 2, 1));

val _ = (check "test_base_5")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 3, NONE))
		     = (l2v [1, 2, 3], 3, 0));

val _ = (check "test_base_6")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 0, SOME 1))
		     = (l2v [1, 2, 3], 0, 1));

val _ = (check "test_base_7")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 1, SOME 1))
		     = (l2v [1, 2, 3], 1, 1));

val _ = (check "test_base_8")
	    (fn _ => VS.base (VS.slice (l2v [1, 2, 3], 1, SOME 2))
		     = (l2v [1, 2, 3], 1, 2));

val _ = (check "test_vector_1")
	    (fn _ => v2l (VS.vector a) = [0, 1, 2, 3, 4, 5])
val _ = (check "test_vector_2")
	    (fn _ => v2l (VS.vector b) = [44, 55, 66])
val _ = (check "test_vector_3")
	    (fn _ => v2l (VS.vector (l2s [])) = [])

val _ = (check "test_concat_1")
	    (fn _ => v2l (VS.concat []) = []);

val _ = (check "test_concat_2")
	    (fn _ => v2l (VS.concat [b]) = [44, 55, 66]);

val _ = (check "test_concat_3")
	    (fn _ => v2l (VS.concat [a, b]) = [0, 1, 2, 3, 4, 5, 44, 55, 66]);

val _ = (check "test_concat_4")
	    (fn _ => v2l (VS.concat [b, a, b])
		     = [44, 55, 66, 0, 1, 2, 3, 4, 5, 44, 55, 66]);

val _ = (check "test_concat_5")
	    (fn _ => v2l (VS.concat [b, z, b])
		     = [44, 55, 66, 44, 55, 66]);

val _ = (check "test_concat_6")
	    (fn _ => v2l (VS.concat [b, z, z, z, b])
		     = [44, 55, 66, 44, 55, 66]);

val _ = (check "test_isEmtpy_1") (fn _ => VS.isEmpty z)
val _ = (check "test_isEmtpy_1") (fn _ => not (VS.isEmpty (l2s [1])))
val _ = (check "test_isEmtpy_2") (fn _ => not (VS.isEmpty a))

val _ = (check "test_getItem_1") (fn _ => case VS.getItem (z) of
					      NONE => true
					    | _ => false)
val _ = (check "test_getItem_2")
	    (fn _ => case VS.getItem (b) of
			 SOME (44, a) =>
			 (case VS.getItem a of
			      SOME (55, b) =>
			      (case VS.getItem b of
				   SOME (66, c) =>
				   (case VS.getItem c of
					NONE => true
				      | _ => false)
				 | _ => false)
			    | _ => false)
		       | _ => false)

val _ = (check "test_appi_1")
	    (fn _ =>
		let val l = ref []
		in VS.appi (fn x => l := x :: !l) (l2s []);
		   !l = []
		end)
val _ = (check "test_appi_2")
	    (fn _ =>
		let val l = ref []
		in VS.appi (fn x => l := x :: !l) b;
		   !l = [(2, 66), (1, 55), (0, 44)]
		end)

val _ = (check "test_app_1")
	    (fn _ =>
		let val l = ref []
		in VS.app (fn x => l := x :: !l) (l2s []);
		   !l = []
		end)
val _ = (check "test_app_2")
	    (fn _ =>
		let val l = ref []
		in VS.app (fn x => l := x :: !l) b;
		   !l = [66, 55, 44]
		end)

val _ = (check "test_mapi_1")
	    (fn _ =>
		v2l (VS.mapi (fn x => x) z) = [])
val _ = (check "test_mapi_2")
	    (fn _ =>
		v2l (VS.mapi (fn x => x) (l2s [1, 2, 3]))
		= [(0,1), (1,2), (2,3)])
val _ = (check "test_mapi_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.mapi (fn x => l := x :: !l) b);
		   !l = [(2, 66), (1, 55), (0, 44)]
		end)

val _ = (check "test_map_1")
	    (fn _ =>
		v2l (VS.map (fn x => x) (l2s [])) = [])
val _ = (check "test_map_2")
	    (fn _ =>
		v2l (VS.map (fn x => x) (l2s [1, 2, 3]))
		= [1, 2, 3])
val _ = (check "test_map_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.map (fn x => l := x :: !l) b);
		   !l = [66, 55, 44]
		end)

val _ = (check "test_foldli_1")
	    (fn _ => VS.foldli (fn (_, e, l) => e :: l) [] z = [])
val _ = (check "test_foldli_2")
	    (fn _ => VS.foldli (fn (i, e, l) => ((i, e) :: l)) [] b
		     = [(2, 66), (1, 55), (0, 44)])
val _ = (check "test_foldli_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.foldli (fn (x as (i, _, s)) => (l := x :: !l; i))
				     123 b);
		   !l = [(2, 66, 1), (1, 55, 0), (0, 44, 123)]
		end)

val _ = (check "test_foldri_1")
	    (fn _ => VS.foldri (fn (_, e, l) => e :: l) [] z = [])
val _ = (check "test_foldri_2")
	    (fn _ => VS.foldri (fn (i, e, l) => ((i, e) :: l)) [] b
		     = [(0, 44), (1, 55), (2, 66)])
val _ = (check "test_foldri_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.foldri (fn (x as (i, _, s)) => (l := x :: !l; i))
				     123 b);
		   !l = [(0, 44, 1), (1, 55, 2), (2, 66, 123)]
		end)

val _ = (check "test_foldl_1")
	    (fn _ => VS.foldl (fn (e, l) => e :: l) [] z = [])
val _ = (check "test_foldl_2")
	    (fn _ => VS.foldl (fn (e, l) => e :: l) [] b = [66, 55, 44])
val _ = (check "test_foldl_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.foldl (fn (x, s) => (l := x :: !l; s)) [] b);
		   !l = [66, 55, 44]
		end)

val _ = (check "test_foldr_1")
	    (fn _ => VS.foldr (fn (e, l) => e :: l) [] z = [])
val _ = (check "test_foldr_2")
	    (fn _ => VS.foldr (fn (e, l) => e :: l) [] b = [44, 55, 66])
val _ = (check "test_foldr_3")
	    (fn _ =>
		let val l = ref []
		in ignore (VS.foldr (fn (x, s) => (l := x :: !l; s)) [] b);
		   !l = [44, 55, 66]
		end)


val _ = (check "test_findi_1")
	    (fn _ => VS.findi (fn _ => false) z = NONE)
val _ = (check "test_findi_2")
	    (fn _ => VS.findi (fn _ => true) z = NONE)
val _ = (check "test_findi_3")
	    (fn _ =>
		let val l = ref []
		in (VS.findi (fn (i, x) => (l := (i, x) :: !l;
					    i = 1)) b
		    = SOME (1, 55))
		   andalso !l = [(1, 55), (0, 44)]
		end)
val _ = (check "test_findi_4")
	    (fn _ =>
		let val l = ref []
		in (VS.findi (fn (i, x) => (l := (i, x) :: !l;
					    false)) b
		    = NONE)
		   andalso !l = [(2, 66), (1, 55), (0, 44)]
		end)

val _ = (check "test_find_1") (fn _ => VS.find (fn _ => false) z = NONE)
val _ = (check "test_find_2") (fn _ => VS.find (fn _ => true) z = NONE)
val _ = (check "test_find_3") (fn _ => VS.find (fn x => x = 55) b = SOME 55)
val _ = (check "test_find_4") (fn _ => VS.find (fn x => false) b = NONE)
val _ = (check "test_find_5")
	    (fn _ =>
		let val l = ref []
		in (VS.find (fn x => (l := x :: !l; x = 55)) b = SOME 55)
		   andalso !l = [55, 44]
		end)
val _ = (check "test_find_6")
	    (fn _ =>
		let val l = ref []
		in (VS.find (fn x => (l := x :: !l; false)) b = NONE)
		   andalso !l = [66, 55, 44]
		end)

val _ = (check "test_exists_1") (fn _ => not (VS.exists (fn _ => false) z))
val _ = (check "test_exists_2") (fn _ => not (VS.exists (fn _ => true) z))
val _ = (check "test_exists_3") (fn _ => VS.exists (fn x => x = 55) b)
val _ = (check "test_exists_4") (fn _ => not (VS.exists (fn x => false) b))
val _ = (check "test_exists_5")
	    (fn _ =>
		let val l = ref []
		in (VS.exists (fn x => (l := x :: !l; x = 55)) b)
		   andalso !l = [55, 44]
		end)
val _ = (check "test_exists_6")
	    (fn _ =>
		let val l = ref []
		in (not (VS.exists (fn x => (l := x :: !l; false)) b))
		   andalso !l = [66, 55, 44]
		end)

val _ = (check "test_all_1") (fn _ => VS.all (fn _ => false) z)
val _ = (check "test_all_2") (fn _ => VS.all (fn _ => true) z)
val _ = (check "test_all_3") (fn _ => VS.all (fn x => x < 100) b)
val _ = (check "test_all_4") (fn _ => not (VS.all (fn x => x < 55) b))
val _ = (check "test_all_5")
	    (fn _ =>
		let val l = ref []
		in (not (VS.all (fn x => (l := x :: !l; x < 55)) b)
		    andalso !l = [55, 44])
		end)
val _ = (check "test_all_6")
	    (fn _ =>
		let val l = ref []
		in (VS.all (fn x => (l := x :: !l; x < 100)) b)
		   andalso !l = [66, 55, 44]
		end)

val _ = (check "test_collate_1")
	    (fn _ => VS.collate Int.compare (z, z) = EQUAL)
val _ = (check "test_collate_2")
	    (fn _ => VS.collate Int.compare (z, b) = LESS)
val _ = (check "test_collate_3")
	    (fn _ => VS.collate Int.compare (b, z) = GREATER)
val _ = (check "test_collate_4")
	    (fn _ => VS.collate Int.compare (b, b) = EQUAL)
val _ = (check "test_collate_5")
	    (fn _ => VS.collate Int.compare (l2s [1, 2], l2s [1, 3]) = LESS)
val _ = (check "test_collate_6")
	    (fn _ => VS.collate Int.compare (l2s [1, 3], l2s [1, 2]) = GREATER)

end
