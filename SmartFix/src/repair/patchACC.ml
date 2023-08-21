open Global
open Patch
open Patch.PatchComp
open Lang
open GenPatchUtil

let likely_by_all' : Global.t -> func -> bool
= fun global func ->
  let g = get_cfg func in
  let nodes = MakeCfg.nodesof g in
  let gnames = List.map fst global.gvars in
  List.exists (fun n ->
    let stmt = find_stmt n g in
    match stmt with
    | Assign ((IndexAccess _) as lv, _, _) when is_uintkind (get_type_lv lv) ->
      let defs = FuncDefUse.get_def_set_lv lv in
      BatSet.exists (fun d -> List.mem d gnames) defs
    | Assign (MemberAccess (e,"balance",_,_),_,_) -> false
    | Assign ((MemberAccess _) as lv, _, _) -> is_uintkind (get_type_lv lv) || is_enum (get_type_lv lv)
    | _ -> false
  ) nodes

let exist_def_addr global func =
  let g = get_cfg func in
  let nodes = MakeCfg.nodesof g in
  let gnames = List.map fst global.gvars in
  List.exists (fun n ->
    let stmt = find_stmt n g in
    match stmt with
    | Assign (lv, _, _) when is_address_kind (get_type_lv lv) ->
      let defs = FuncDefUse.get_def_set_lv lv in
      BatSet.exists (fun d -> List.mem d gnames) defs
    | _ -> false
  ) nodes

let rec find_eth_send global stmt =
  match stmt with
  | Call (lvop,Lv (MemberAccess (e,fname,_,_)),args,_,_,loc)
    when is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] -> true
  | Seq (s1,s2) -> find_eth_send global s1 || find_eth_send global s2
  | If (_,s1,s2,_) -> find_eth_send global s1
  | _ -> false

let rec eth_send_in_loop' global stmt =
  match stmt with
  | Seq (s1,s2) -> eth_send_in_loop' global s1 || eth_send_in_loop' global s2
  | While (_,s) -> find_eth_send global s
  | If _ -> false
  | _ -> false

let rec eth_send_in_loop global func = eth_send_in_loop' global (get_body func)

let likely_privileged' : Global.t -> func -> bool
= fun global func ->
  (* let b = eth_send_in_loop global func in *)
  (* let _ = print_endline (get_fname func ^ " : " ^ string_of_bool b) in *)
  exist_def_addr global func || eth_send_in_loop global func

(* The heuristic is based on the above contracts *)
let likely_accessible_by_all : Global.t -> func -> bool
= fun global f ->
  let reachable = CallGraph.transitive_callees (BatSet.singleton (Lang.get_fkey f)) global.call_edges in
  let reachable = BatSet.map (fun k -> FuncMap.find k global.fmap) reachable in
  let b1 = BatSet.exists (fun f' -> likely_by_all' global f') reachable in
  (* let b2 = BatSet.for_all (fun f' -> not (likely_privileged' global f')) reachable in
  let _ = print_endline (get_fname f ^ " : " ^ string_of_bool b1 ^ ", " ^ string_of_bool b2 ^ ", " ^ string_of_bool (contain_extern global f)) in *)
  b1 (* && b2 *)

(************************************)
(** Access-control Repair Template **)
(************************************)

(* utils for common repair templates *)
let collect_cnstr_cands ~def_var_num : Global.t -> func list -> patch_comp list
= fun global funcs ->
  let call_edges = global.call_edges in
  (* constructors should be either 'public' or 'internal', *)
  (* but consider 'public' only because 'internal' cannot be directly invoked by users *)
  let funcs = List.filter (fun f -> not (is_constructor f) && (is_public_func f)) funcs in
  List.fold_left (fun acc f ->
    let defs = FuncDefUse.find_def_set (Lang.get_fkey f) global.f_defuse in
    let defs_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.gvars)) defs in
    let addr_gvars = List.filter (fun v -> is_address (snd v)) global.gvars |> BatSet.of_list |> BatSet.map fst in
    if not (BatSet.disjoint defs_gvars addr_gvars)
       && BatSet.cardinal defs >= def_var_num
       && not (BatSet.mem (Lang.get_fkey f) (BatSet.map snd call_edges)) (* callee functions must not be candidates of constructors *)
       && (get_finfo f).org_scope_s = !Options.main_contract (* e.g., do not change setOwner funcitons in parents *)
      then (Atom (ChangeToCnstr (Lang.get_fkey f))) :: acc
    else acc
  ) [] funcs

let is_range_address_of_mapping : typ -> bool
= fun typ ->
  match typ with
  | Mapping (_, EType Address) -> true
  | Mapping _ -> false
  | _ -> false

let collect_funcs_that_define_addr_lvs : Global.t -> func list -> func list
= fun global funcs ->
  List.filter (fun f ->
    let defs = FuncDefUse.find_def_set (Lang.get_fkey f) global.f_defuse in
    let defs_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.gvars)) defs in
    let addr_gvars = List.filter (fun v -> is_address (snd v) || is_range_address_of_mapping (snd v)) global.gvars |> BatSet.of_list |> BatSet.map fst in
    not (BatSet.disjoint defs_gvars addr_gvars)
  ) funcs

let collect_mods_that_use_addr_exps : Global.t -> func list -> func list
= fun global funcs ->
  let mods = List.filter is_modifier funcs in
  let addr_gvars =
    let is_extended_address_kind (x,t) = (is_address_kind t || is_range_address_of_mapping t) in
    global.gvars |> List.filter is_extended_address_kind |> BatSet.of_list |> BatSet.map fst
  in
  List.filter (fun f ->
    (* use 'use_set' rather than 'use_set_assume'; addr vars  may not be directly used in assume. *)
    let uses = FuncDefUse.find_use_set (Lang.get_fkey f) global.f_defuse in
    let uses_gvars = BatSet.filter (fun d -> List.mem d (List.map fst global.gvars)) uses in
    not (BatSet.disjoint uses_gvars addr_gvars)
  ) mods

(***************************)
(** Report-Aware Template **)
(***************************)

let rec collect_loc : stmt -> line BatSet.t
= fun stmt ->
  match stmt with
  | Assign (_,_,loc) -> BatSet.singleton loc.line
  | Decl _ -> BatSet.empty
  | Seq (s1,s2) -> BatSet.union (collect_loc s1) (collect_loc s2)
  | Call (_,_,_,_,_,loc) -> BatSet.singleton loc.line
  | Skip -> BatSet.empty
  | If (e,s1,s2,i) ->
    BatSet.add i.if_loc.line
      (BatSet.union (collect_loc_e e) (BatSet.union (collect_loc s1) (collect_loc s2)))
  | While (e,s) -> BatSet.union (collect_loc_e e) (collect_loc s)
  | Break | Continue -> BatSet.empty
  | Return (_,loc) -> BatSet.singleton loc.line
  | Throw -> BatSet.empty
  | Assume (_,loc) | Assert (_,_,loc) -> BatSet.singleton loc.line
  | Assembly _ -> BatSet.empty
  | PlaceHolder -> BatSet.empty
  | Unchecked (slst,loc) ->
    List.fold_left (fun acc s -> BatSet.union (collect_loc s) acc) (BatSet.singleton loc.line) slst

and collect_loc_e : exp -> line BatSet.t
= fun exp ->
  match exp with
  | Int _ | Real _ | Str _ -> BatSet.empty
  | Lv lv -> collect_loc_lv lv
  | Cast (_,e) -> collect_loc_e e
  | BinOp (_,_,_,einfo) -> BatSet.singleton einfo.eloc.line
  | UnOp (_,e,_) -> collect_loc_e e
  | True | False | ETypeName _ -> BatSet.empty
  | _ -> assert false (* temp expressions *)

and collect_loc_lv : lv -> line BatSet.t
= fun lv ->
  match lv with
  | Var (_,vinfo)
  | MemberAccess (_,_,vinfo,_) -> BatSet.singleton vinfo.vloc.line
  | IndexAccess (e1,Some e2,_) -> BatSet.union (collect_loc_e e1) (collect_loc_e e2)
  | IndexAccess (e,None,_) -> assert false
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | None -> acc | Some e -> BatSet.union (collect_loc_e e) acc
    ) BatSet.empty eops

let get_toploc : func -> line BatSet.t -> line option
= fun f mod_lines ->
  let lines = collect_loc (get_body f) in
  let lines = BatSet.filter (fun l -> l > 0) lines in (* exclude modelled stmts *)
  let lines = BatSet.diff lines mod_lines in (* exclude lines from inliend modifiers *)
  if BatSet.is_empty lines then None
  else Some (BatSet.min_elt lines)

let add_authority_check_mod : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  let mods = collect_mods_that_use_addr_exps global (get_funcs (get_main_contract pgm)) in
  let org_contract_of_f = find_contract_id pgm (get_finfo f).org_scope_s in
  let bases_of_f = List.map (find_contract_nid pgm) (get_cinfo org_contract_of_f).inherit_order |> List.map get_cname in
  List.fold_left (fun acc m ->
    (* Do not apply modifiers defined in child contracts *)
    if not (List.mem (get_finfo m).org_scope_s bases_of_f) then acc
    else if List.mem (get_fname m) (List.map (fun (a,_,_) -> a) (get_finfo f).mod_list) then acc

    else
      (Atom (AddModifier (Lang.get_fkey f, get_fname m, (get_finfo f).param_loc.finish_line)))::acc
  ) [] mods

let add_authority_check_var : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  let funcs = List.map snd (BatMap.bindings global.fmap) in
  let mods = List.filter is_modifier funcs in
  let mod_body_lines = List.fold_left (fun acc m -> BatSet.union (collect_loc (get_body m)) acc) BatSet.empty mods in
  let mod_invoke_lines = List.map (fun (_,_,c) -> c.line) (get_finfo f).mod_list |> BatSet.of_list in
  let mod_lines = BatSet.union mod_body_lines mod_invoke_lines in
  match get_toploc f mod_lines with
  | None -> []
  | Some loc ->
    let decls = List.filter (fun (id,_,vinfo) -> is_address vinfo.vtyp) (get_decls (get_main_contract pgm)) in
    List.fold_left (fun acc (v,_,vinfo) ->
      let func_defined_cname = (get_finfo f).org_scope_s in
      let var_defined_cname = get_cname (List.find (fun c -> vinfo.vscope = get_numid c) pgm) in
      (* avoid uncomilable patches *)
      if not (List.mem (v,vinfo.vtyp) (get_gvars_c (find_contract_id pgm func_defined_cname))) then acc
      else if not (!Options.main_contract = var_defined_cname) && vinfo.vvis = Private then acc
      else
        let e1 = Lv (Var (v, vinfo)) in
        let e2 = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
        let s = Assume (mk_eq e1 e2, dummy_loc) in
        let comp = Atom (InsertLine (loc, s, false)) in
        comp :: acc
    ) [] decls

(* protect with fresh ownership variable when no templates are applied. *)
let add_authority_check_var2 : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  let funcs = List.map snd (BatMap.bindings global.fmap) in
  let mods = List.filter is_modifier funcs in
  let mod_body_lines = List.fold_left (fun acc m -> BatSet.union (collect_loc (get_body m)) acc) BatSet.empty mods in
  let mod_invoke_lines = List.map (fun (_,_,c) -> c.line) (get_finfo f).mod_list |> BatSet.of_list in
  let mod_lines = BatSet.union mod_body_lines mod_invoke_lines in
  match get_toploc f mod_lines with
  | None -> assert false
  | Some loc ->
    let e1 = Lv (Var ("smartfix_owner",  mk_vinfo ~typ:(EType Address) ())) in
    let e2 = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
    let s = Assume (mk_eq e1 e2, dummy_loc) in
    let comp = Atom (InsertLine (loc, s, false)) in
    [comp]

(* template 2 *)
let negate_bools : exp list -> patch_comp list
= fun exps ->
  List.fold_left (fun acc exp ->
    match exp with
    (* comparison with zero-address is not considered. *)
    | BinOp (bop,_,Cast (EType Address,Int n),_) when (bop=Eq || bop=NEq) && BatBig_int.equal n BatBig_int.zero -> []
    | BinOp (bop,Cast (EType Address,Int n),_,_) when (bop=Eq || bop=NEq) && BatBig_int.equal n BatBig_int.zero -> []

    | BinOp (Eq,e1,e2,einfo) when is_address (get_type_exp e1) && is_address (get_type_exp e2) ->
      acc @ [Atom (Replace (einfo.eloc.line, exp, BinOp (NEq, e1, e2, {einfo with eid = -1})))]
    | BinOp (NEq,e1,e2,einfo) when is_address (get_type_exp e1) && is_address (get_type_exp e2) ->
      acc @ [Atom (Replace (einfo.eloc.line, exp, BinOp (Eq, e1, e2, {einfo with eid = -1})))]
    | UnOp (LNot,Lv (Var (v,vinfo)),typ) ->
      acc @ [Atom (Replace (vinfo.vloc.line, exp, Lv (Var (v,vinfo))))]
    (* | UnOp (uop,e,typ) -> cand_e kind stmt_loc loc e *)
    | _ -> acc
  ) [] exps

(*****************************)
(** Report-Unaware Template **)
(*****************************)

(* common template 1. change to cnstr *)
let change_to_cnstr : Global.t -> pgm -> func list -> patch_comp list
= fun global pgm funcs ->
  let cnstr = List.filter is_constructor funcs |> (fun funcs' -> assert (List.length funcs' = 1); List.hd funcs') in
  let no_cnstr_in_org = (get_finfo cnstr).fid = -1 in
  if no_cnstr_in_org then
    let cands = collect_cnstr_cands ~def_var_num:2 global funcs in
    if List.length cands > 0 then cands
    else collect_cnstr_cands ~def_var_num:1 global funcs
  else []

(* common template 2. negate_cond_in_mod *)
let negate_cond_in_mod : Global.t -> pgm -> func list -> patch_comp list
= fun global pgm funcs ->
  let mods = collect_mods_that_use_addr_exps global funcs in
  List.fold_left (fun acc m ->
    acc @ (negate_bools (List.map snd (collect_exp (get_body m))))
  ) [] mods
  |> List.sort_uniq PatchComp.compare

let add_authority_check_to_func ~fresh_owner : Global.t -> pgm -> func -> patch_comp list -> patch_comp list
= fun global pgm f acc ->
  let mods_that_use_addrs = collect_mods_that_use_addr_exps global (get_funcs (get_main_contract pgm)) in
  let validation_with_vars = add_authority_check_var global pgm f in
  let no_global_addrs =
    let fields = Global.get_all_fields global in
    let is_extended_address_kind t = (is_address_kind t || is_range_address_of_mapping t || is_contract t) in
    List.for_all (fun (x,t) -> not (is_extended_address_kind t)) (global.gvars @ fields)
  in
  if is_constructor f                     (* no input valiadation patches for constructors or modifers *)
     || likely_accessible_by_all global f (* heuristic : do not add ownership checking when likely to be normal functions *)
     || is_private_func f || is_internal_func f
    then acc
  else if List.length mods_that_use_addrs > 0 then acc @ (add_authority_check_mod global pgm f)
  else if List.length validation_with_vars > 0 then acc @ validation_with_vars
  else if no_global_addrs && fresh_owner then acc @ (add_authority_check_var2 global pgm f)
  else acc

(* common template 3. add_authority_check_to_addr *)
let add_authority_check_to_addr_def_funcs : Global.t -> pgm -> func list -> patch_comp list
= fun global pgm funcs ->
  let funcs = collect_funcs_that_define_addr_lvs global funcs in
  let funcs = List.filter (fun f-> is_public_func f || is_external_func f) funcs in
  Vocab.list_fold (add_authority_check_to_func ~fresh_owner:false global pgm) funcs []
  |> List.sort_uniq PatchComp.compare

let report_unaware_template : Global.t -> pgm -> patch_comp list
= fun global pgm ->
  let funcs = get_funcs (get_main_contract pgm) in
  let lst1 = change_to_cnstr global pgm funcs in
  let lst2 = negate_cond_in_mod global pgm funcs in
  let lst3 = add_authority_check_to_addr_def_funcs global pgm funcs in
  (lst1 @ lst2 @ lst3)
  |> List.sort_uniq PatchComp.compare

let report_aware_template : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  let negated = if likely_accessible_by_all global f then [] else (negate_bools (List.map snd (collect_exp (get_body f)))) in
  negated
  @ (add_authority_check_to_func ~fresh_owner:true global pgm f [])
  |> List.sort_uniq PatchComp.compare
