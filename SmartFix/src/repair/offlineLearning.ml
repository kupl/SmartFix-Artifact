open Lang
open Vocab
open Path
open Options
open Patch
open Patch.PatchComp
open WorkspaceRepair
open OnlineLearning

type feature = {
  def_typs : typ BatSet.t;
  use_typs : typ BatSet.t;
  payable : bool;
  send_eth : bool;
  destruct : bool;
  contain_loop : bool;
}

let empty_feature = {
  def_typs = BatSet.empty;
  use_typs = BatSet.empty;
  payable = false;
  send_eth = false;
  destruct = false;
  contain_loop = false;
}

let to_string_typs typs =
  string_of_set ~first:"{" ~last:"}" ~sep:"," to_string_typ typs

let to_string_feature feat =
  to_string_typs feat.def_typs ^ ", " ^
  to_string_typs feat.use_typs ^ ", " ^
  (if feat.payable then "1" else "0") ^ ", " ^
  (if feat.send_eth then "1" else "0") ^ ", " ^
  (if feat.destruct then "1" else "0") ^ ", " ^
  (if feat.contain_loop then "1" else "0")

let add_def_typs : typ BatSet.t -> feature -> feature
= fun typs feat -> {feat with def_typs = BatSet.union typs feat.def_typs}

let add_use_typs : typ BatSet.t -> feature -> feature
= fun typs feat -> {feat with use_typs = BatSet.union typs feat.use_typs}

let add_payable : feature -> feature
= fun feat -> {feat with payable = true}

let add_send_eth : feature -> feature
= fun feat -> {feat with send_eth = true}

let add_destruct : feature -> feature
= fun feat -> {feat with destruct = true}

let add_contain_loop : feature -> feature
= fun feat -> {feat with contain_loop = true}

let rec def_of : lv -> var BatSet.t
= fun lv ->
  match lv with
  | Var (v,vinfo) -> BatSet.singleton (v, vinfo.vtyp)
  | MemberAccess (e,x,xinfo,_) -> BatSet.singleton (x, xinfo.vtyp)
  | IndexAccess (Lv lv,_,_) -> def_of lv
  | IndexAccess (Cast (_, Lv lv),_,_) -> def_of lv
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | Some (Lv lv) -> BatSet.union (def_of lv) acc
      | None -> acc
      | _ -> failwith "def_of"
    ) BatSet.empty eops
  | _ -> failwith "def_of"

let feature_map : Global.t -> func -> feature
= fun global func ->
  let cfg = get_cfg func in
  let nodes = MakeCfg.nodesof cfg in
  let feat = if is_payable func then add_payable empty_feature else empty_feature in
  List.fold_left (fun acc node ->
    let stmt = find_stmt node cfg in
    match stmt with
    | Assign (lv,_,_) ->
      let vars = BatSet.filter (fun x -> List.mem x global.gvars) (def_of lv) in
      let typs = BatSet.map get_type_var vars in
      add_def_typs typs acc
    | Assume (e,_) ->
      let vars = BatSet.filter (fun x -> List.mem x global.gvars) (var_exp e) in
      let typs = BatSet.map get_type_var vars in
      add_use_typs typs acc
    | Call (lvop,Lv (MemberAccess (e,fname,_,_)),args,_,_,loc)
      when is_address_kind (get_type_exp e) && List.mem fname ["transfer";"send"] ->
      let _ = assert (no_eth_gas_modifiers stmt) in
      add_send_eth acc
    | Call (lvop, Lv (MemberAccess (e,"call",_,_)), args, Some eth, gasop, loc)
      when is_address_kind (get_type_exp e) ->
      add_send_eth acc
    | Call (lvop,Lv (Var (fname,_)),args,_,_,loc)
      when List.mem fname ["selfdestruct";"suicide"] ->
      let _ = assert (List.length args = 1) in
      add_destruct acc
    | _ when is_loophead node cfg -> add_contain_loop acc
    | _ -> acc
  ) feat nodes

let to_string_bops bops =
  string_of_set ~first:"{" ~last:"}" ~sep:"," to_string_bop bops

module Abs_PatchComp = struct
  type t' =
    | Abs_Insert of feature * (* function *)
                    typ BatSet.t (* patch *)
    | Abs_Replace of feature * (* function *)
                     bop BatSet.t * typ BatSet.t * (* original *)
                     bop BatSet.t * typ BatSet.t   (* patch *)
    | Abs_AddModifier of feature * feature (* (function, modifier) *)
    | Abs_NR   of feature
    | Abs_Move of feature
    | Abs_ChangeToCnstr of feature
    | Abs_ElseRevert of feature

  type t =
    | Abs_Atom of t'
    | Abs_AtomLst of t' list

  let to_string' : t' -> string
  = fun acomp ->
    match acomp with
    | Abs_Insert (feat,typs) ->
      "Abs_Insert (" ^
       to_string_feature feat ^ " @ " ^
       (* to_string_bops bops ^ " @ " ^ *)
       to_string_typs typs ^ ")"

    | Abs_Replace (feat, bops1, typs1, bops2, typs2) ->
      "Abs_Replace (" ^
       to_string_feature feat ^ " @ " ^
       to_string_feature feat ^ " @ " ^
       to_string_bops bops1 ^ " @ " ^
       to_string_typs typs1 ^ " @ " ^
       to_string_bops bops2 ^ " @ " ^
       to_string_typs typs2 ^ ")"

    | Abs_AddModifier (f,m) -> "Abs_AddModifier (" ^ to_string_feature f ^ " @ " ^ to_string_feature m ^ ")"
    | Abs_NR feat -> "Abs_NR (" ^ to_string_feature feat ^ ")"
    | Abs_Move feat -> "Abs_Move (" ^ to_string_feature feat ^ ")"
    | Abs_ChangeToCnstr feat -> "Abs_ChangeToCnstr (" ^ to_string_feature feat ^ ")"
    | Abs_ElseRevert feat -> "Abs_ElseRevert (" ^ to_string_feature feat ^ ")"

  let to_string : t -> string
  = fun abs_comp ->
    match abs_comp with
    | Abs_Atom a -> to_string' a
    | Abs_AtomLst lst -> string_of_list ~first:"Abs_AtomLst [\n" ~last:"\n]" ~sep:"\n" to_string' lst

end

type abs_patch_comp = Abs_PatchComp.t
type abs_patch = abs_patch_comp list

let to_string_abs_patch : abs_patch -> string
= fun abs_patch ->
  string_of_list ~first:"[" ~last:"]" ~sep:",\n" Abs_PatchComp.to_string abs_patch

let to_string_abs_patch2 abs_patch =
  let abs_patch = List.map (fun c -> match c with `String s -> s | _ -> assert false) abs_patch in
  string_of_list ~first:"[" ~last:"]" ~sep:",\n" Vocab.id abs_patch

(*******************)
(*** Abstraction ***)
(*******************)

let typs_stmt : Global.t -> stmt -> typ BatSet.t
= fun global stmt ->
  match stmt with
  | Assign (lv,e,loc) ->
    BatSet.filter (fun v -> List.mem v global.gvars) (var_lv lv)
    |> BatSet.map snd
  | Assume (e,loc) ->
    BatSet.filter (fun v -> List.mem v global.gvars) (var_exp e)
    |> BatSet.map snd
  | _ -> assert false

let rec bops_exp : exp -> bop BatSet.t
= fun exp ->
  match exp with
  | Int _ | Real _ | Str _ -> BatSet.empty
  | Lv lv -> bops_lv lv
  | Cast (_,e) -> bops_exp e
  | BinOp (bop,e1,e2,_) -> BatSet.add bop (BatSet.union (bops_exp e1) (bops_exp e2))
  | UnOp (_,e,_) -> bops_exp e
  | True | False -> BatSet.empty
  | ETypeName _ -> BatSet.empty
  | _ -> failwith "bops_exp: temp expressions encountered"

and bops_lv : lv -> bop BatSet.t
= fun lv ->
  match lv with
  | Var _ -> BatSet.empty
  | MemberAccess (e,x,xinfo,_) -> bops_exp e
  | IndexAccess (e1,Some e2,_) -> BatSet.union (bops_exp e1) (bops_exp e2)
  | IndexAccess (e,None,_) -> bops_exp e
  | Tuple (eops,_) ->
    List.fold_left (fun acc eop ->
      match eop with
      | None -> acc
      | Some e -> BatSet.union (bops_exp e) acc
    ) BatSet.empty eops

let bops_stmt : Global.t -> stmt -> bop BatSet.t
= fun global stmt ->
  match stmt with
  | Assign (lv,e,loc) -> BatSet.union (bops_lv lv) (bops_exp e)
  | Assume (e,loc) -> bops_exp e
  | _ -> assert false

let abstract_atom : Global.t -> atomic_comp -> Abs_PatchComp.t'
= fun global atomic ->
  match atomic with
  | InsertLine (l,stmt,_) ->
    let feat = feature_map global (Global.find_func_containing_line l global) in
    let typs = typs_stmt global stmt in
    Abs_PatchComp.Abs_Insert (feat,typs)

  | Replace (l,e1,e2) ->
    let feat = feature_map global (Global.find_func_containing_line l global) in
    let bops1 = bops_exp e1 in
    let typs1 = BatSet.filter (fun v -> List.mem v global.gvars) (var_exp e1) |> BatSet.map snd in
    let bops2 = bops_exp e2 in
    let typs2 = BatSet.filter (fun v -> List.mem v global.gvars) (var_exp e2) |> BatSet.map snd in
    Abs_PatchComp.Abs_Replace (feat,bops1,typs1,bops2,typs2)

  | AddModifier (fkey,"@nonReentrant",l) ->
    let feat = feature_map global (FuncMap.find fkey global.fmap) in
    Abs_PatchComp.Abs_NR feat

  | AddModifier (fkey,mname,l) ->
    let find_mod mname fmap =
      let lst = BatMap.bindings fmap in
      let lst = List.filter (fun ((cid,fid,_),_) -> !Options.main_contract = cid && fid = mname) lst in
      let _ = assert (List.length lst = 1) in
      let modi = snd (List.hd lst) in
      let _ = assert (is_modifier modi) in
      modi
    in
    let f_feat = feature_map global (FuncMap.find fkey global.fmap) in
    let m_feat = feature_map global (find_mod mname global.fmap) in
    Abs_PatchComp.Abs_AddModifier (f_feat, m_feat)

  | Move (lst,l,s) ->
    let feat = feature_map global (Global.find_func_containing_line l global) in
    Abs_PatchComp.Abs_Move feat

  | ChangeToCnstr fkey ->
    let feat = feature_map global (FuncMap.find fkey global.fmap) in
    Abs_PatchComp.Abs_ChangeToCnstr feat

  | ElseRevert (_,_,_,i) ->
    let line = i.if_loc.finish_line in
    let feat = feature_map global (Global.find_func_containing_line line global) in
    Abs_PatchComp.Abs_ElseRevert feat


let abstract' : Global.t -> patch_comp -> abs_patch_comp
= fun global comp ->
  match comp with
  | Atom a -> Abs_PatchComp.Abs_Atom (abstract_atom global a)
  | AtomLst lst -> Abs_PatchComp.Abs_AtomLst (List.map (abstract_atom global) lst)

let abstract : Global.t -> pgm -> patch -> abs_patch
= fun global pgm patch -> List.map (abstract' global) patch

type knowledge = (abs_patch * score)

(***************************)
(*** Build new knowledge ***)
(***************************)

let knowledge_to_json : knowledge -> Yojson.Basic.t
= fun (abs_patch, score) ->
  abs_patch
  |> List.map Abs_PatchComp.to_string
  |> List.map (fun s -> `String s)
  |> (fun lst ->
      `Assoc [("abs_patch", `List lst);
              ("score", `Float score)])

let knowledges_to_json : Global.t -> pgm -> knowledge BatSet.t -> Yojson.Basic.t
= fun global pgm knowledges ->
  let knowledges = BatSet.to_list knowledges in
  `Assoc [("src", `List [`String (Filename.basename !inputfile)]);
          ("knowledge", `List (List.map knowledge_to_json knowledges))]

let store_new_knowledges : Global.t -> pgm -> knowledge BatSet.t -> unit
= fun global pgm knowledges ->
  let json = knowledges_to_json global pgm knowledges in
  let f = open_out (!Options.outdir ^ "/" ^ "new_knowledge.json") in
  Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string json);
  close_out f

(**************************)
(*** Combine knowledges ***)
(**************************)

let combine_knowledges () =
  let kdir = knowledge_in !outdir in
  if not (Sys.file_exists kdir) then ()
  else
    let oldf = Yojson.Basic.from_file (knowledge_in !outdir) in
    let newf = Yojson.Basic.from_file (!outdir ^ "/" ^ "new_knowledge.json") in

    let old_src = oldf |> Json.value_of "src" |> Json.to_list in
    let new_src = newf |> Json.value_of "src" |> Json.to_list in

    let old_k = oldf |> Json.value_of "knowledge" |> Json.to_list in
    let new_k = newf |> Json.value_of "knowledge" |> Json.to_list in

    let json = `Assoc [("src", `List (old_src @ new_src));
                       ("knowledge", `List (old_k @ new_k))] in

    let f = open_out (!Options.outdir ^ "/" ^ "combined_knowledge.json") in
    Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string json);
    close_out f

(***********************)
(*** Load knowledges ***)
(***********************)

let contain_comp : Global.t -> string list -> patch_comp -> bool
= fun global abs_patch comp ->
  List.exists (fun abs_comp ->
    abs_comp = Abs_PatchComp.to_string (abstract' global comp)
  ) abs_patch

let to_feature : Global.t -> pgm -> patch_comp list -> Yojson.Basic.t -> OnlineLearning.feature
= fun global pgm comps json ->
  let strip j = match j with `String s -> s | _ -> assert false in
  let abs_patch = json |> Json.value_of "abs_patch" |> Json.to_list |> List.map strip in
  List.fold_left (fun acc comp ->
    if contain_comp global abs_patch comp then
      (* let _ = print_endline "HI1" in
      let _ = print_endline (PatchComp.to_string comp) in
      let _ = print_endline abs_patch in
      let _ = print_endline "" in *)
      acc@[1]
    else acc@[0]
  ) [] comps

let to_score : Yojson.Basic.t -> score
= fun json -> json |> Json.value_of "score" |> Json.to_float

let load_knowledge : Global.t -> pgm -> patch_comp list -> Yojson.Basic.t -> tr_data
= fun global pgm comps knowledge ->
  let feature = to_feature global pgm comps knowledge in
  let score = to_score knowledge in
  (* let _ =
    if not (List.for_all (fun n -> (n = 0)) feature) then
      let abs_patch = knowledge |> Json.value_of "abs_patch" |> Json.to_list |> to_string_abs_patch2 in
      let _ = print_endline abs_patch in
      let _ = print_endline (OnlineLearning.to_string_feature feature ^ " : " ^ string_of_float score) in
      print_endline ""
  in *)
  (feature, score)


let mk_onehot_vector : int -> int -> int list
= fun idx len ->
  let zeros = BatList.make len 0 in
  BatList.modify_at idx (fun _ -> 1) zeros

let find_one_idxs : int list -> int list
= fun feat ->
  BatList.fold_lefti (fun acc i n -> if n = 1 then acc @ [i] else acc) [] feat

(* [1;1;0] => {[1;0;0], [0;1;0]} *)
let onehot_children : int list -> int list list
= fun feat ->
  let len = List.length feat in
  let one_idxs = find_one_idxs feat in
  List.fold_left (fun acc idx ->
    acc @ [mk_onehot_vector idx len]
  ) [] one_idxs

(**************************************************************)
(*** Heuristic for avoiding overfitting in regression model ***)
(**************************************************************)

let augment_tr_set : tr_set -> tr_set
= fun tr_set ->
  let all_feats = BatSet.map fst tr_set in
  BatSet.fold (fun (feat,score) acc ->
    let onehots = BatSet.of_list (onehot_children feat) in
    let onehots = BatSet.filter (fun onehot -> not (BatSet.mem onehot all_feats)) onehots in
    let new_set = BatSet.map (fun onehot -> (onehot,score)) onehots in
    BatSet.union new_set acc
  ) tr_set tr_set

let copy_knowledge_src () =
  let ksrc = !Options.repair_ksrc in
  if ksrc = "" then ()
  else
    assert (Sys.command ("cp " ^ ksrc ^ " " ^ !Options.outdir ^ "/" ^ "knowledge.json") = 0)

let load_knowledges : Global.t -> Lang.pgm -> patch_comp list -> tr_set
= fun global pgm comps ->
  let _ = copy_knowledge_src () in
  let kdir = knowledge_in !outdir in
  if not (Sys.file_exists kdir) then BatSet.empty
  else
    let json = Yojson.Basic.from_file kdir in
    let knowledges = match Json.value_of "knowledge" json with `List lst -> lst | _ -> assert false in
    let tr_lst = List.map (load_knowledge global pgm comps) knowledges in
    let tr_set = BatSet.of_list tr_lst in
    (* let tr_set = augment_tr_set tr_set in *)
    tr_set
