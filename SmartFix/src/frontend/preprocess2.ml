open Lang
open FuncMap
open Vlang
open Vocab
open CallGraph
open Options

let clone_cnt = ref 0
let update_clone_cnt () = clone_cnt:=!clone_cnt+1
let clone_label () = "__clone" (* ^ (string_of_int !clone_cnt) *)

let extern_called = Var ("@extern_called", mk_vinfo ~typ:(EType Bool) ())

let clone_func : func -> func
= fun f ->
  (* let stmt' = rename_stmt (clone_label ()) gvars cnames (get_body f) in *)
  let fname' = (get_fname f) ^ clone_label () in
  let finfo' =
    {is_constructor = (get_finfo f).is_constructor;
     is_payable = (get_finfo f).is_payable;
     is_modifier = (get_finfo f).is_modifier;
     mod_list = [];
     mod_list2 = [];
     param_loc = dummy_loc;
     ret_param_loc = dummy_loc;
     fvis = Internal;
     mutability = get_mutability f;
     fid = (get_finfo f).fid;
     floc = dummy_loc;
     scope = (get_finfo f).scope;
     scope_s = (get_finfo f).scope_s;
     org_scope_s = (get_finfo f).org_scope_s;
     cfg = (get_finfo f).cfg
    }
  in
  f (* |> update_body stmt' *) |> update_fname fname' |> update_finfo finfo'

let rec perm lst r acc =
  if r = 0 then acc
  else
    List.fold_left (fun acc' l ->
      acc' @ (List.map (fun e -> [e] @ l) lst)
    ) [] acc
    |> perm lst (r-1)

let rec repeat_perms lst r acc =
  if r = 0 then acc
  else repeat_perms lst (r-1) ((perm lst r [[]]) @ acc)

let make_all_perms lst ~r:r =
  let _ = assert (List.length lst >= r) in
  repeat_perms lst r []

let cnt = ref 0
let fresh_label () =
  (cnt := !cnt + 1; "__" ^ string_of_int !cnt)

let call_pubs pubs =
  List.fold_left (fun acc f ->
    let args = List.map (fun (v,vinfo) -> Lv (Var (v^fresh_label(), vinfo))) (get_params f) in
    let log = Call (None, Lv (Var ("@extern_log", dummy_vinfo)), Lv (Var (get_fname f, dummy_vinfo))::args, None, None, dummy_loc) in
    let call = Call (None, Lv (Var (get_fname f, dummy_vinfo)), args, None, None, dummy_loc) in
    let stmt = Seq (log,call) in
    if is_skip acc then stmt
    else Seq (acc, stmt)
  ) Skip pubs

let rec call_cloned_pubs i rcv perms =
  match perms with
  | [] -> Skip
  | pubs::tl ->
    let calls = call_pubs pubs in
    let assign = Assign (extern_called, True, dummy_loc) in
    let assume = Assume (mk_eq (Lv rcv) (Lv (Var ca)), dummy_loc) in
    let btmp = gen_tmpvar (EType Bool) in
    If (Lv btmp,
        Seq (Seq(assign, assume), calls),
        call_cloned_pubs (i+1) rcv tl, dummy_ifinfo)

let make_call_to_cloned_pubs : lv -> func list -> stmt
= fun rcv pubs ->
  call_cloned_pubs 0 rcv (make_all_perms pubs ~r:(min (List.length pubs) !Options.extern_depth))
  |> (fun stmt -> If (mk_not (Lv extern_called), stmt, Skip, dummy_ifinfo))

let make_extern c pubs : func =
  let fname = "@extern" in
  let param = match gen_tmpvar (EType Address) with Var v -> v | _ -> assert false in
  let stmt = make_call_to_cloned_pubs (Var param) pubs in
  let finfo =
    {is_constructor = false; is_payable = false; is_modifier = false;
     mod_list = []; mod_list2 = [];
     param_loc=dummy_loc; ret_param_loc=dummy_loc; fvis=Internal;
     mutability=NonPayable;
     fid = (-1); floc = dummy_loc; scope = (-1);
     scope_s = get_cname c; org_scope_s = get_cname c;
     cfg = empty_cfg} in
  (fname, [param], [], stmt, finfo)

let make_clones pubs = List.map clone_func pubs

let make_extern_clones c pubs = [make_extern c pubs] (* ::(make_clones pubs) *)

let call_extern rcv loc =
  Call (None, Lv (Var ("@extern", dummy_vinfo)), [rcv], None, None, loc)

let extern_generated = ref false

(***********************************************************)
(*** Insert assertions for detecting invalid assignments ***)
(***********************************************************)

let pid = ref 0
let fresh_pid () = (pid:=!pid +1; !pid)

let is_pseudo_stmt_node : node -> cfg -> bool
= fun n g ->
  let stmt = find_stmt n g in
  match stmt with
  | Assign (lv,e,_) ->
    BatSet.exists (fun x -> BatString.starts_with (fst x) "@") (BatSet.union (var_lv lv) (var_exp e))
  | Assume (e,_) ->
    BatSet.exists (fun x -> BatString.starts_with (fst x) "@") (var_exp e)
  | Assert (_,"assign_const",_) -> true
  | Assert (_,"no_effect",_) -> true
  | Assert (_,"deadcode",_) -> true
  | _ -> false

let rec get_addrs : stmt -> exp list
= fun stmt ->
  match stmt with
  | Assign (IndexAccess (Lv (Var (x,xinfo)), Some idx, _), _, _)
    when is_usual_mapping xinfo.vtyp -> [idx]
  | Seq (s1,s2) -> (get_addrs s1) @ (get_addrs s2)
  | If (e,s1,s2,_) -> (get_addrs s1) @ (get_addrs s2)
  | While (e,s) -> get_addrs s
  | _ -> []

let all_addrs_diff : exp list -> exp
= fun addrs ->
  let is_true e = match e with True -> true | _ -> false in
  let pairs = BatList.cartesian_product addrs addrs in
  (* avoid vacuously true (e.g., a!=a) case, and redundant (e.g., a!=b, b!=a) condition *)
  let pairs = List.filter (fun (a1,a2) -> to_string_exp a1 < to_string_exp a2) pairs in
  List.fold_left (fun acc (a1,a2) ->
    let res = mk_neq a1 a2 in
    if is_true acc then res
    else mk_and acc res
  ) True pairs

let gen_assign_const all lv loc =
  let base = mk_eq (Lv lv) (Int BatBig_int.zero) in
  let addrs = get_addrs all |> List.sort_uniq (fun e1 e2 -> Stdlib.compare (to_string_exp e1) (to_string_exp e2)) in
  if List.length addrs = 0 || List.length addrs = 1 then Assert (base, "assign_const", loc)
  else
    let diff = all_addrs_diff addrs in
    Assert (mk_or (mk_not diff) base, "assign_const", loc)

let rec invalid_assign_s : id list -> FuncMap.t -> stmt -> stmt -> stmt
= fun cnames fmap all stmt ->
  match stmt with
  | Assign (lv, _, _)
    when BatString.starts_with (to_string_lv lv) "Param" -> stmt (* exclude assignments from return normalization *)
  | Assign (lv, BinOp (bop,e1,e2,_), loc)
    when (bop=Add || bop=Sub) && loc.line > 0 -> (* loc > 0: exclude modelled assignment *)
    let pre = "@pre" ^ string_of_int (fresh_pid ()) in
    (* org: to maintain original name even when inlined; affecting the performance (query normalization) *)
    let org = Lv (Var (pre, mk_vinfo ~typ:(get_type_lv lv) ())) in
    let pre = Var (pre, mk_vinfo ~typ:(get_type_lv lv) ~org:(Some org) ()) in
    let assign_pre = Assign (pre, Lv lv, loc) in
    let check1 = gen_assign_const all lv loc in
    let check2 = Assert (mk_eq (Lv pre) (Lv lv), "no_effect", loc) in
    Seq (assign_pre, Seq (stmt, Seq (check1, check2)))
  | Assign (lv, e, _) when is_const_int (get_type_exp e) -> stmt (* for no-effect checker, exclude constant assignments *)
  | Assign (lv,e,loc)
    when (is_uintkind (get_type_lv lv) || is_sintkind (get_type_lv lv) || is_address_kind (get_type_lv lv)) && loc.line > 0 ->
    let pre = "@pre" ^ string_of_int (fresh_pid ()) in
    let org = Lv (Var (pre, mk_vinfo ~typ:(get_type_lv lv) ())) in
    let pre = Var (pre, mk_vinfo ~typ:(get_type_lv lv) ~org:(Some org) ()) in
    let assign_pre = Assign (pre, Lv lv, loc) in
    let check = Assert (mk_eq (Lv pre) (Lv lv), "no_effect", loc) in
    Seq (assign_pre, Seq (stmt, check))
  | Decl _ -> stmt
  | Seq (s1,s2) -> Seq (invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2)
  | If ((BinOp (bop,e1,e2,einfo)) as e, s1, s2, i) when einfo.eloc.line > 0 ->
    let check = Assert (False, "deadcode", einfo.eloc) in
    Seq (If (e, invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2, i), check)
  | If (e,s1,s2,i) -> If (e, invalid_assign_s cnames fmap all s1, invalid_assign_s cnames fmap all s2, i)
  | While (e,s) -> While (e, invalid_assign_s cnames fmap all s)

    (* built-in functions *)
  | Call (lvop,e,args,ethop,gasop,loc)
    when FuncMap.is_undef_call fmap stmt -> stmt

  | Call (lvop,e,args,ethop,gasop,loc)
    when is_internal_call fmap cnames stmt ->
    let check = Assert (False, "deadcode", loc) in
    Seq (stmt, check)

  | Call (lvop,e,args,ethop,gasop,loc) when is_external_call stmt -> stmt

    (* object calls *)
  | Call (lvop,e,args,ethop,gasop,loc) ->
    let _ = match e with Lv (MemberAccess (rcv,_,_,_)) -> rcv | _ -> assert false in
    stmt

  | _ -> stmt

(**************************************************)
(*** Desugar built-in functions that send money ***)
(**************************************************)

let rec convert_call_s : id list -> FuncMap.t -> stmt -> stmt
= fun cnames fmap stmt ->
  match stmt with
  | Assign _ | Decl _ -> stmt
  | Seq (s1,s2) -> Seq (convert_call_s cnames fmap s1, convert_call_s cnames fmap s2)

  | Call (lvop, Lv (MemberAccess (e,"call",_,_)), args, Some eth, gasop, loc)
    when is_address_kind (get_type_exp e) ->
   
    let _ = extern_generated := true in (* this may be an imprecise signal, e.g., missing calls within a library function *)

    let balance_info = mk_vinfo ~typ:(EType (UInt 256)) () in
    let this_info = mk_vinfo ~typ:(EType (Contract !main_contract)) () in
    let this = Cast (EType Address, Lv (Var ("this", this_info))) in
    let this_balance = MemberAccess (this,"balance", balance_info, EType (UInt 256)) in
    let rcv_balance = MemberAccess (e, "balance", balance_info, EType (UInt 256)) in

    let invest_map = Var ("@Invest", mk_vinfo ~typ:(Mapping (Address, EType (UInt 256))) ()) in
    let rcv_invest = IndexAccess (Lv invest_map, Some e, EType (UInt 256)) in

    let eth' =
      match eth with
      | BinOp (Mul, Lv v1, Lv (Var (v,_)), _) when BatString.starts_with v "sellPrice" -> Lv v1
      | _ -> eth in
    let invest_sum = Var ("@Invest_sum", mk_vinfo ~typ:(EType (UInt 256)) ()) in
    let trust_map = Var ("@TU", mk_vinfo ~typ:(Mapping (Address, EType Bool)) ()) in
    let not_trusted = mk_eq (Lv (IndexAccess (Lv trust_map, Some e, EType Bool))) False in
    let invest_sum_stmt = If (not_trusted, Assign (invest_sum, mk_sub (Lv invest_sum) eth', dummy_loc), Skip, dummy_ifinfo) in

    let cond1 = mk_ge (Lv this_balance) eth in
    let cond2 = mk_ge (mk_add (Lv rcv_balance) eth) (Lv rcv_balance) in
    let true_b = Seq (Assign (this_balance, mk_sub (Lv this_balance) eth, dummy_loc), Assign (rcv_balance, mk_add (Lv rcv_balance) eth, dummy_loc)) in
    let true_b = Seq (true_b, Assign (rcv_invest, mk_sub (Lv rcv_invest) eth, dummy_loc)) in
    let true_b = Seq (true_b, invest_sum_stmt) in
    let true_b = Seq (true_b, call_extern e loc) in
    (* let true_b = Seq (call_extern e loc, true_b) in *)

    let true_b = Seq (stmt, true_b) in
    let false_b = Skip in
    let true_b, false_b =
      (match lvop with
       | None -> true_b, false_b
       | Some (Tuple (Some (Lv (Var v))::_,_))
       | Some (Var v) -> Seq (true_b, Assign (Var v, True, dummy_loc)), Seq (false_b, Assign (Var v, False, dummy_loc))
       | _  -> assert false) in
    let if_stmt = If (mk_and cond1 cond2, true_b, false_b, dummy_ifinfo) in
    if_stmt

  | Call (lvop,Lv (MemberAccess (e,instr,_,_)),args, _, _, loc)
    when is_address_kind (get_type_exp e) && List.mem instr ["send"; "transfer"] ->
    let _ = assert (no_eth_gas_modifiers stmt) in
    let _ = assert (List.length args = 1) in
    let eth = List.hd args in
    let balance_info = mk_vinfo ~typ:(EType (UInt 256)) () in
    let this_info = mk_vinfo ~typ:(EType (Contract !main_contract)) () in
    let this = Cast (EType Address, Lv (Var ("this", this_info))) in
    let this_balance = MemberAccess (this,"balance", balance_info, EType (UInt 256)) in
    let rcv_balance = MemberAccess (e, "balance", balance_info, EType (UInt 256)) in

    let invest_map = Var ("@Invest", mk_vinfo ~typ:(Mapping (Address, EType (UInt 256))) ()) in
    let rcv_invest = IndexAccess (Lv invest_map, Some e, EType (UInt 256)) in

    let eth' =
     match eth with
     | BinOp (Mul, Lv v1, Lv (Var (v,_)), _) when BatString.starts_with v "sellPrice" -> Lv v1
     | _ -> eth in
    let invest_sum = Var ("@Invest_sum", mk_vinfo ~typ:(EType (UInt 256)) ()) in
    let trust_map = Var ("@TU", mk_vinfo ~typ:(Mapping (Address, EType Bool)) ()) in
    let not_trusted = mk_eq (Lv (IndexAccess (Lv trust_map, Some e, EType Bool))) False in
    let invest_sum_stmt = If (not_trusted, Assign (invest_sum, mk_sub (Lv invest_sum) eth', dummy_loc), Skip, dummy_ifinfo) in

    let cond1 = mk_ge (Lv this_balance) eth in
    let cond2 = mk_ge (mk_add (Lv rcv_balance) eth) (Lv rcv_balance) in
    let true_b = Seq (Assign (this_balance, mk_sub (Lv this_balance) eth, dummy_loc), Assign (rcv_balance, mk_add (Lv rcv_balance) eth, dummy_loc)) in
    let true_b = Seq (true_b, Assign (rcv_invest, mk_sub (Lv rcv_invest) eth, dummy_loc)) in
    let true_b = Seq (true_b, invest_sum_stmt) in
    let true_b = Seq (stmt, true_b) in
    let false_b =
      if instr = "transfer" then Throw
      else if instr = "send" then
        (match lvop with
         | None -> Skip
         | Some lv -> Assign (lv, False, dummy_loc))
      else assert false in
    let if_stmt = If (mk_and cond1 cond2, true_b, false_b, dummy_ifinfo) in
    (match instr with
     | "transfer" when !check_reg ->
       let check = Assert (False, "deadcode", loc) in
       Seq (if_stmt, check)
     | "transfer" -> if_stmt
     | "send" -> if_stmt
     | _ -> assert false)

    (* built-in functions *)
  | Call (lvop,e,args,ethop,gasop,loc)
    when FuncMap.is_undef_call fmap stmt -> stmt

    (* internal call *)
  | Call (lvop,e,args,ethop,gasop,loc)
    when is_internal_call fmap cnames stmt -> stmt

    (* object calls *)
  | Call (lvop,e,args,ethop,gasop,loc) ->
    let rcv =
      match e with
      | Lv (MemberAccess (rcv,_,_,_)) when is_contract (get_type_exp rcv) -> rcv
      | _ -> assert false
    in
    let callees = FuncMap.find_matching_funcs "" e (List.map get_type_exp args) cnames fmap in
    if BatSet.for_all is_view_pure_f callees then stmt (* non-modifiable calls are considered to be harmless *)
    else
      Seq (stmt, call_extern rcv loc)

  | Skip -> stmt
  | If (e,s1,s2,i) -> If (e, convert_call_s cnames fmap s1, convert_call_s cnames fmap s2, i)
  | While (e,s) -> While (e, convert_call_s cnames fmap s)
  | Break | Continue | Return _ | Throw
  | Assume _ | Assert _ | Assembly _ | PlaceHolder -> stmt
  | Unchecked (lst,loc) ->
    let lst' = List.map (convert_call_s cnames fmap) lst in
    Unchecked (lst',loc)

let do_all_f cnames fmap func =
  let body = get_body func in
  let body = convert_call_s cnames fmap body in
  let body =
    if is_constructor func then body
    else if not !Options.check_reg then body
    else invalid_assign_s cnames fmap body body in
  update_body body func

let do_all_c cnames fmap c =
  let funcs = get_funcs c in
  let publics = List.filter (fun f -> (is_public_func f || is_external_func f) && not (is_constructor f)) funcs in
  let funcs' = List.map (do_all_f cnames fmap) (get_funcs c) in
  let funcs' = if !Options.mode = "exploit" then funcs'@(make_extern_clones c publics) else funcs' in
  let _ = extern_generated := false in
  update_funcs funcs' c

let do_all cnames fmap p = List.map (do_all_c cnames fmap) p

let run : pgm -> pgm
= fun p ->
  let fmap = FuncMap.mk_fmap p in
  let cnames = get_cnames p in
  if !check_leak || !check_re then
    do_all cnames fmap p
  else p
