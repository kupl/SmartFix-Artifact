open Patch
open Lang
open GenPatchUtil

let generate' : line -> string -> (loc * exp) -> patch_comp list
= fun l ae (sloc,exp) -> (* sloc : location of surrounding statement *)
  match exp with
  | BinOp (Add,e1,e2,einfo)
    when einfo.eloc.line = l && ae = to_string_exp ~report:true exp ->
    let line = if sloc.line > 0 then sloc.line else l in
    let s = Assume (mk_ge (mk_add e1 e2) e1, dummy_loc) in
    [Atom (InsertLine (line,s,true))]

  | BinOp (Sub,e1,e2,einfo)
    when einfo.eloc.line = l && ae = to_string_exp ~report:true exp ->
    let line = if sloc.line > 0 then sloc.line else l in
    let s = Assume (mk_ge e1 e2, dummy_loc) in
    [Atom (InsertLine (line, s, true))]

  | BinOp (Mul,e1,e2,einfo)
    when einfo.eloc.line = l && ae = to_string_exp ~report:true exp ->
    let zero = mk_eq e1 (Int BatBig_int.zero) in
    let mul_div = mk_eq (mk_div (mk_mul e1 e2) e1) e2 in
    let s = Assume (mk_or zero mul_div, dummy_loc) in
    [Atom (InsertLine ((if sloc.line > 0 then sloc.line else l), s, true))]

    (* comparison with integer constants are not considered from the search space *)
  | BinOp (bop,e1,Int _,einfo) when bop = GEq || bop = Gt -> []
  | BinOp (bop,Int _,_,einfo) when bop = GEq || bop = Gt -> []

    (* usual programming patterns *)
    (* a+b>=a, a+b>a ~> a+b<a *)
  | BinOp (bop, (BinOp (Add,e1',e1'',_) as e1), e2, einfo) when bop=GEq || bop=Gt ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (Lt, e1, e2, {einfo with eid = -1})))]

    (* a+b<=a, a+b<a ~> a+b>=a *)
  | BinOp (bop, (BinOp (Add,e1',e1'',_) as e1), e2, einfo) when bop = LEq || bop = Lt ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (GEq, e1, e2, {einfo with eid = -1})))]

    (* a>=b, a>b ~> a<b *)
  | BinOp (bop, e1, e2, einfo) when (bop = GEq || bop = Gt) && BatString.ends_with ae ("- " ^ to_string_exp ~report:true e2 ^")") ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (Lt, e1, e2, {einfo with eid = -1})))]

    (* a<=b, a<b ~> a>=b *)
  | BinOp (bop, e1, e2, einfo) when (bop = LEq || bop = Lt) && BatString.ends_with ae ("- " ^ to_string_exp ~report:true e2 ^")") ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (GEq, e1, e2, {einfo with eid = -1})))]


    (* unusual programming patterns *)
    (* a>=a+b, a>a+b ~> a<=a+b *)
  | BinOp (bop, e1, (BinOp (Add,e2',e2'',_) as e2), einfo) when bop=GEq || bop=Gt ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (LEq, e1, e2, {einfo with eid = -1})))]

    (* a<=a+b, a<a+b ~> a>a+b *)
  | BinOp (bop, e1, (BinOp (Add,e2',e2'',_) as e2), einfo) when bop=LEq || bop=Lt ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (Gt, e1, e2, {einfo with eid = -1})))]

    (* b>=a, b>a ~> b<=a *)
  | BinOp (bop, e1, e2, einfo) when (bop = GEq || bop = Gt) && BatString.ends_with ae ("- " ^ to_string_exp ~report:true e1 ^")") ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (LEq, e1, e2, {einfo with eid = -1})))]

    (* b<=a, b<a ~> b>a *)
  | BinOp (bop, e1, e2, einfo) when (bop = LEq || bop = Lt) && BatString.ends_with ae ("- " ^ to_string_exp ~report:true e1 ^")") ->
    [Atom (Replace (einfo.eloc.line, exp, BinOp (Gt, e1, e2, {einfo with eid = -1})))]

  | _ -> []

let generate : line -> string -> (loc * exp) list -> patch_comp list
= fun line ae lst ->
  List.fold_left (fun acc (sloc,exp) ->
    acc @ (generate' line ae (sloc,exp))
  ) [] lst
