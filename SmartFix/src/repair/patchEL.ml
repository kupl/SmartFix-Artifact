open Patch
open Patch.PatchComp
open Lang
open FixTarget
open GenPatchUtil

(***********************************)
(** Ether-Leaking Repair Template **)
(***********************************)

let rec collect_eth_transfer : Global.t -> line -> stmt -> (lv * stmt) list
= fun global l stmt ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (collect_eth_transfer global l s1) @ (collect_eth_transfer global l s2)
  | Call (lvop,Lv (MemberAccess (e,fname,_,_)),args,_,_,loc)
    when is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] && l = loc.line ->
    let eth = (assert (List.length args = 1); List.hd args) in
    (match eth with
     | Lv (Var _) -> []
     | Lv lv -> if BatSet.disjoint (var_lv lv) (BatSet.of_list global.gvars) then [] else [(lv, stmt)]
     | _ -> [])
  | Call (lvop, Lv (MemberAccess (e,"call",_,_)), args, Some eth, gasop, loc)
    when is_address_kind (get_type_exp e) && l = loc.line ->
    (match eth with
     | Lv (Var _) -> []
     | Lv lv -> if BatSet.disjoint (var_exp eth) (BatSet.of_list global.gvars) then [] else [(lv, stmt)]
     | _ -> [])
  | Call _ -> []
  | Skip -> []
  | If (e,s1,s2,i) -> (collect_eth_transfer global l s1) @ (collect_eth_transfer global l s2)
  | While (e,s) -> collect_eth_transfer global l s
  | Break | Continue | Return _
  | Throw | Assume _ | Assert _
  | Assembly _ | PlaceHolder -> []
  | Unchecked (slst,loc) -> List.fold_left (fun acc s -> acc @ (collect_eth_transfer global l s)) [] slst

let leak_fix : Global.t -> func -> line -> patch_comp list
= fun global f line ->
  let assigns = get_assigns line (get_body f) in
  let eth_transfer = collect_eth_transfer global line (get_body f) in
  if List.length eth_transfer = 0 then []
  else
    let (eth_lv,_) = (assert (List.length eth_transfer = 1); List.hd eth_transfer) in
    let assigns = List.filter (fun (lv',_,_) -> to_string_lv lv' = to_string_lv eth_lv) assigns in
    if List.length assigns > 0 then []
    else (* x.transfer(b[m]) => uint tmp = b[m]; b[m] := 0 ; x.transfer(tmp); *)
      let tmp = fresh_tmp () in
      let tmp = Var (tmp, mk_vinfo ~typ:(get_type_lv eth_lv) ()) in
      let a1 = Replace (line, Lv eth_lv, Lv tmp) in
      let a2 = InsertLine (line, Assign (eth_lv, Int BatBig_int.zero, dummy_loc), false) in
      let a3 = InsertLine (line, Assign (tmp, Lv eth_lv, dummy_loc), false) in
      [AtomLst [a1;a2;a3]]

let generate : Global.t -> pgm -> func -> line -> patch_comp list
= fun global pgm f line ->
  (leak_fix global f line)
  @ (PatchACC.report_aware_template global pgm f)
