open Global
open Patch
open Patch.PatchComp
open Lang
open FuncMap
open GenPatchUtil
open CallGraph

(********************************)
(** Reentrancy Repair Template **)
(********************************)

let rec collect_extcall : Global.t -> func -> stmt -> line * line ->
                          (line * line * line * exp option * stmt) list
= fun global curf stmt (l1,l2) ->
  match stmt with
  | Assign _ | Decl _ -> []
  | Seq (s1,s2) -> (collect_extcall global curf s1 (l1,l2)) @ (collect_extcall global curf s2 (l1,l2))
  | Call (lvop, Lv (MemberAccess (e,"call",_,_)), args, Some eth, gasop, loc)
    when is_address_kind (get_type_exp e) ->
    let vars = var_exp eth in
    if BatSet.disjoint vars (BatSet.of_list global.gvars) then [(l1,l2, loc.line, Some eth, stmt)]
    else []

    (* to avoid treating undefined function calls as static call *)
  | Call (lvop,e,args,ethop,gasop,loc) when is_undef_call global.fmap stmt -> []

  | Call (lvop,e,args,ethop,gasop,loc) when is_internal_call global.fmap global.cnames stmt ->
    let callee = get_callee_of_intcall global curf stmt in
    if contain_extern global callee then [(l1, l2, loc.line, None, stmt)]
    else []

  | Call _ -> []
  | Skip -> []
  (* | If _ | While _ -> [] *)
  | If (e,s1,s2,ifinfo) ->
    (match ifinfo.if_floc with
     | None -> (collect_extcall global curf s1 (ifinfo.if_tloc.line, ifinfo.if_tloc.finish_line))
     | Some floc ->
       let t_l1 = if ifinfo.if_tloc.line = -1 then l1 else ifinfo.if_tloc.line in
       let t_l2 = if ifinfo.if_tloc.finish_line = -1 then l2 else ifinfo.if_tloc.finish_line in
       let f_l1 = if floc.line = -1 then l1 else floc.line in
       let f_l2 = if floc.finish_line = -1 then l2 else floc.finish_line in
       (collect_extcall global curf s1 (t_l1, t_l2))
        @ (collect_extcall global curf s2 (f_l1, f_l2)))
  | While (e,s) -> collect_extcall global curf s (l1,l2)
  | Break | Continue | Return _
  | Throw | Assume _ | Assert _
  | Assembly _ | PlaceHolder -> []
  | Unchecked (slst,_) -> List.fold_left (fun acc s -> acc @ (collect_extcall global curf s (l1,l2))) [] slst

let rec collect_extcall2 : Global.t -> stmt -> (line * exp option * stmt) list
= fun global stmt ->
  match stmt with
  | Seq (s1,s2) -> (collect_extcall2 global s1) @ (collect_extcall2 global s2)
  | Call (lvop, Lv (MemberAccess (e,"call",_,_)), args, Some eth, gasop, loc)
    when is_address_kind (get_type_exp e) ->
    let vars = var_exp eth in
    if BatSet.disjoint vars (BatSet.of_list global.gvars) then []
    else [(loc.line, Some eth, stmt)]
  | If (e,s1,s2,_) -> (collect_extcall2 global s1) @ (collect_extcall2 global s2)
  | While (e,s) -> collect_extcall2 global s
  | _ -> []

let find_toploc_extcall ~is_template_1 : Global.t -> func -> (line * exp option * stmt) option
= fun global func ->
  (* let lst = if is_template_1 then collect_extcall global func (get_body func)
            else collect_extcall2 global (get_body func) in *)
  let lst = collect_extcall2 global (get_body func) in
  if List.length lst = 0 then None
  else
    let lst = BatList.sort (fun (a,_,_) (a',_,_) -> Stdlib.compare a a') lst in
    Some (List.hd lst)

let add_callret_check line extcall body =
  let rets = callret extcall in
  if not (List.length rets = 1) then []
  else
    let ret = List.hd rets in
    if callret_checked ret body then []
    else [InsertLine (line+1, Assume (Lv ret, dummy_loc), false)]

let template_1 : Global.t -> func list -> patch_comp list
= fun global funcs ->
  List.fold_left (fun acc func ->
    let finfo = get_finfo func in
    (* let _ = print_endline (get_fname func ^ " : " ^  string_of_int finfo.floc.line ^ " , " ^ string_of_int finfo.floc.finish_line) in *)
    let extcalls = collect_extcall global func (get_body func) (finfo.floc.line, finfo.floc.finish_line) in
    List.fold_left (fun acc2 (l1,l2,line,_,extcall) ->
      let triples = get_assigns line (get_body func) in
      let triples = List.filter (fun (a,b,c) ->
         (* let _ = print_endline (to_string_stmt (Assign (a,b,dummy_loc))) in
            let _ = print_endline (string_of_int c ^ "," ^ string_of_int l1 ^ "," ^ string_of_int l2) in
            let _ = print_endline "" in *)
         l1 <= c && c <= l2) triples in
      let assigns = List.map (fun (a,b,c) -> (c, Assign (a,b,dummy_loc))) triples in
      if List.length assigns = 0 then acc2
      else
        acc2 @ [Move (assigns, line, extcall)] @ (add_callret_check line extcall (get_body func))
    ) [] extcalls
    |> (fun res ->
        if List.length res = 0 then acc
        else
          let ifs = get_if (get_body func) in
          let ifs = List.map (fun (a,b,c,d) -> ElseRevert (a,b,c,d)) ifs in
          acc @ res @ ifs)
          (* acc @ [AtomLst (res@ifs)]) *)
  ) [] funcs
  |> (fun res ->
      if List.length res = 0 then []
      else [AtomLst res])

let template_2 : Global.t -> func list -> patch_comp list
= fun global funcs ->
  List.fold_left (fun acc func ->
    match find_toploc_extcall ~is_template_1:false global func with
    | None -> acc
    | Some (line, None, _) -> assert false
    | Some (line, Some eth, extcall) ->
      let assign = get_assigns line (get_body func) in
      if List.length assign > 1 then acc
      else if List.length assign = 0 then acc
      else
        let _ = assert (List.length assign = 1) in
        let (lv',e',line') = List.hd assign in
        if not (to_string_exp eth = to_string_exp (Lv lv')) then acc
        else
          (* x.call.value(b[m]); b[m] = 0 *)
          (* ~> tmp = b[m]; b[m] = 0; x.call.value(tmp); *)
          let tmp = fresh_tmp () in
          let tmp = Var (tmp, mk_vinfo ~typ:(get_type_exp eth) ()) in
          let a1 = Replace (line, eth, Lv tmp) in
          let a2 =
            let assign = (line', Assign (lv',e', dummy_loc)) in (* assign behind the extcall *)
            Move ([assign], line, extcall) in
          let a3 = InsertLine (line, Assign (tmp, Lv lv', dummy_loc), false) in
          acc @ [AtomLst [a1;a2;a3]]
  ) [] funcs

let is_global_based_addr_typed_exp : Global.t -> exp -> bool
= fun global exp ->
  match exp with
  | Lv (Var (v,vinfo)) ->
    (* is_address vinfo.vtyp && List.mem (v,vinfo.vtyp) global.gvars *)
    is_address_kind vinfo.vtyp && BatSet.cardinal (ItvDom.Val.gtaint_of (ItvDom.Mem.find (v,vinfo.vtyp) global.mem)) > 0
  | Lv (IndexAccess (Lv (Var (v,vinfo)),_,typ)) ->
    (* is_address typ && List.mem (v,vinfo.vtyp) global.gvars *)
    is_address_kind typ && BatSet.cardinal (ItvDom.Val.gtaint_of (ItvDom.Mem.find (v,vinfo.vtyp) global.mem)) > 0
  | Lv (MemberAccess (_,_,vinfo,_)) -> false (* field access is likely to be unprivileged access *)
  | _ -> false

let has_addr_comparison_in_assume : Global.t -> func -> bool
= fun global f ->
  let g = get_cfg f in
  let nodes = MakeCfg.nodesof g in
  List.exists (fun n ->
    let stmt = find_stmt n g in
    match stmt with
    | Assume (BinOp (_,e1,e2,_), _) ->
      is_global_based_addr_typed_exp global e1
      || is_global_based_addr_typed_exp global e2
    | _ -> false
  ) nodes

let debug_template ext_and_states ext_and_states_wo_call ext_and_states_wo_mod ext_and_states_wo_mod2 =
  let _ = print_endline ("group 1 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states) in
  let _ = print_endline ("group 2 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_call) in
  let _ = print_endline ("group 3 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_mod) in
  let _ = print_endline ("group 4 : " ^ Vocab.string_of_list ~sep:", " get_fname ext_and_states_wo_mod2) in
  assert false

(* Add 'nonReentrant' Modifier *)
let template_3 : Global.t -> func list -> patch_comp list
= fun global funcs ->
  let funcs = List.filter (fun f -> not (is_constructor f) && not (is_modifier f) && not (is_view_pure_f f)) funcs in

  (* for search efficiency and patch readability *)
  let funcs = List.filter (fun f -> is_public_func f || is_external_func f) funcs in

  let ext = List.filter (fun f -> contain_extern global f) funcs in
  (* let _ = print_endline (Vocab.string_of_list get_fname ext) in *)
  let states = List.filter (PatternAnalysis.may_update_state_f global) funcs in

  let states_wo_mod = List.filter (fun f -> not (has_addr_comparison_in_assume global f)) states in

  let ext_and_states = BatList.unique ~eq:(fun f1 f2 -> get_fkey f1 = get_fkey f2) (ext@states) in                  (* X + S *)
  let ext_and_states_wo_call = List.filter (fun f -> not (contain_dead_intcall global f)) ext_and_states in         (* (X + S) \ call *)
  let ext_and_states_wo_mod = BatList.unique ~eq:(fun f1 f2 -> get_fkey f1 = get_fkey f2) (ext@states_wo_mod) in    (* X + (S/M) *)
  let ext_and_states_wo_mod2 =  List.filter (fun f -> not (contain_dead_intcall global f)) ext_and_states_wo_mod in (* (X + (S/M)) \ call *)

  let _ = if !Options.debug = "fix_re" then debug_template ext_and_states ext_and_states_wo_call ext_and_states_wo_mod ext_and_states_wo_mod2 in

  let comps1 = List.map (fun f -> AddModifier (Lang.get_fkey f, "@nonReentrant", (get_finfo f).param_loc.finish_line)) ext_and_states in
  let comps2 = List.map (fun f -> AddModifier (Lang.get_fkey f, "@nonReentrant", (get_finfo f).param_loc.finish_line)) ext_and_states_wo_call in
  let comps3 = List.map (fun f -> AddModifier (Lang.get_fkey f, "@nonReentrant", (get_finfo f).param_loc.finish_line)) ext_and_states_wo_mod in
  let comps4 = List.map (fun f -> AddModifier (Lang.get_fkey f, "@nonReentrant", (get_finfo f).param_loc.finish_line)) ext_and_states_wo_mod2 in

  let r1 = if List.length comps1 > 0 then [AtomLst comps1] else [] in
  let r2 = if List.length comps2 > 0 then [AtomLst comps2] else [] in
  let r3 = if List.length comps3 > 0 then [AtomLst comps3] else [] in
  let r4 = if List.length comps4 > 0 then [AtomLst comps4] else [] in
  r1 @ r2 @ r3 @r4

let generate : Global.t -> func list -> patch_comp list
= fun global funcs ->
  (template_1 global funcs)
  @ (template_2 global funcs)
  @ (template_3 global funcs)
