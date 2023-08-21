open Patch
open Patch.PatchComp
open Lang
open FixTarget
open GenPatchUtil

let rec change_e : line -> exp -> patch_comp list
= fun line exp ->
  match exp with
  | Int _ | Real _ | Str _ -> []
  | Lv lv when to_string_lv lv = "tx.origin" ->
    let msg_sender = Lv (Var ("msg.sender", mk_vinfo ~typ:(EType Address) ())) in
    let tx_origin = Lv (Var ("tx.origin", mk_vinfo ~typ:(EType Address) ())) in
    [Atom (Replace (line, tx_origin, msg_sender))]
  | Lv _ -> []
  | Cast (_,e) -> change_e line e
  | BinOp (_,e1,e2,_) -> (change_e line e1) @ (change_e line e2)
  | UnOp (_,e,_) -> change_e line e
  | True | False | ETypeName _ -> []
  | _ -> assert false

let change_s : stmt -> patch_comp list
= fun stmt ->
  match stmt with
  | Assign (lv,e,loc) -> change_e loc.line e
  | Assume (e,loc) -> change_e loc.line e
  | _ -> []

let generate : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  let cfg = get_cfg f in
  let nodes = MakeCfg.nodesof cfg in
  List.fold_left (fun acc n ->
    acc @ (change_s (find_stmt n cfg))
  ) [] nodes
  |> List.sort_uniq PatchComp.compare
