open Vocab
open Lang
open FuncMap
open MakeCfg
open Path
open Global

let may_violate_cei = ref false
let may_call_itself = ref false

let may_update_state_f : Global.t -> func -> bool
= fun global f ->
  let defs = FuncDefUse.find_def_set (Lang.get_fkey f) global.f_defuse in
  let field_names = Global.get_all_fields global |> List.map fst in
  is_payable f
  || BatSet.exists (fun d -> List.mem d (List.map fst global.gvars) || d = "@Invest_sum" || List.mem d field_names) defs

let is_state_manipulating_assign : Global.t -> stmt -> bool
= fun global stmt ->
  let fields = Global.get_all_fields global in
  let field_names = List.map fst fields in
  match stmt with
  | Assign (lv,_,_) ->
    let defs = FuncDefUse.get_def_set_lv lv in
    BatSet.exists (fun d -> List.mem d (List.map fst global.gvars) || d = "@Invest_sum" || List.mem d field_names) defs
  | _ -> false

let may_update_state : Global.t -> func -> Node.t -> bool
= fun global curf n ->
  let g = get_cfg curf in
  let stmt = find_stmt n g in
  match stmt with
  | _ when is_state_manipulating_assign global stmt -> true
  | Call (lvop,e,args,ehtop,gasop,loc) when is_undef_call global.fmap stmt ->
    (match lvop with
     | None -> false
     | Some lv ->
       let fields = Global.get_all_fields global in
       let field_names = List.map fst fields in
       let defs = FuncDefUse.get_def_set_lv lv in
       BatSet.exists (fun d -> List.mem d (List.map fst global.gvars) || d = "@Invest_sum" || List.mem d field_names) defs)
  | Call (lvop,e,args,ehtop,gasop,loc) when is_internal_call global.fmap global.cnames stmt ->
    let _ = assert (no_eth_gas_modifiers stmt) in
    let callee = get_callee_of_intcall global curf stmt in
    may_update_state_f global callee
  | _ -> false

let callee_contain_extcall : Global.t -> func -> Node.t -> cfg -> bool
= fun global f n g ->
  let callee = get_callee_of_intcall global f (find_stmt n g) in
  contain_extern global callee

let is_ext_node : Global.t -> func -> Node.t -> bool
= fun global f n ->
  let g = get_cfg f in
  if is_external_call_node n g then true
  else if is_internal_call_node global.fmap global.cnames n g then
    if callee_contain_extcall global f n g then true
    else false
  else false

let get_ext_nodes : Global.t -> func -> Node.t list
= fun global f ->
  let nodes = nodesof (get_cfg f) in
  List.filter (is_ext_node global f) nodes

let get_all_succs g node =
  let onestep g nodes = BatSet.fold (fun n acc -> BatSet.union (BatSet.of_list (succ n g)) acc) nodes nodes in
  BatSet.diff (Vocab.fix (onestep g) (BatSet.singleton node)) (BatSet.singleton node)

let cei_hold' : Global.t -> func -> bool
= fun global func ->
  let cfg = get_cfg func in
  let ext_nodes = get_ext_nodes global func in
  List.for_all (fun e ->
    let all_succs = BatSet.to_list (get_all_succs cfg e) in
    List.for_all (fun succ ->
      not (may_update_state global func succ)
      (* let b = not (may_update_state global func succ) in
      let _ = if not b then print_endline (get_fname func ^ "  : " ^ Node.to_string e ^ " -> " ^ Node.to_string succ) in
      b *)
    ) all_succs
  ) ext_nodes

(* check if cei pattern holds *)
let cei_hold : Global.t -> func list -> bool
= fun global funcs -> List.for_all (cei_hold' global) funcs

let check_recursive'' : Global.t -> cfg -> Node.t -> bool
= fun global g node ->
  let stmt = find_stmt node g in
  match stmt with
    (* built-in functions *)
  | Call (lvop,e,args,ethop,gasop,loc) when FuncMap.is_undef_call global.fmap stmt -> false
  | Call (lvop,e,args,ethop,gasop,loc) when is_internal_call global.fmap global.cnames stmt -> false
  | Call _ when is_external_call stmt -> false
    (* object call *)
  | Call (lvop,e,args,ethop,gasop,loc) ->
    let rcv = match e with Lv (MemberAccess (rcv,_,_,_)) -> rcv | _ -> assert false in
    (match get_type_exp rcv with
     | EType (Contract c) -> List.mem c global.base_names
     | _ -> assert false)
  | _ -> false

let check_recursive' : Global.t -> Path.t -> bool
= fun global (fk,nodes) ->
  let f = FuncMap.find fk global.fmap in
  let g = get_cfg f in
  List.exists (check_recursive'' global g) nodes

let check_recursive : Global.t -> PathSet.t -> bool
= fun global paths -> PathSet.exists (check_recursive' global) paths

let run : Global.t -> pgm -> PathSet.t -> unit
= fun global pgm paths ->
  let _ = may_call_itself := check_recursive global paths in
  let _ = may_violate_cei := not (cei_hold global (pgm |> get_main_contract |> get_funcs)) in
  let _ = print_endline ("[INFO] Violate CEI: " ^ string_of_bool !may_violate_cei) in
  let _ = print_endline ("[INFO] msg.sender = this possible: " ^ string_of_bool !may_call_itself) in
  ()
