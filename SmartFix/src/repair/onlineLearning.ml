open Options
open Vocab
open FixTarget
open WorkspaceRepair
open Patch
open Patch.PatchComp

type tr_data = feature * score
and feature = int list
and score = float

type tr_set = tr_data BatSet.t

let to_string_feature feat = string_of_list ~first:"[" ~sep:", " ~last:"]" string_of_int feat

let to_string_tr_data (feat,num) =
  to_string_feature feat ^ " : " ^ string_of_float num

let to_string_tr_set tr_set =
  string_of_set ~first:"{" ~last:"}" ~sep:"\n" to_string_tr_data tr_set

(*************************************************)
(*** Compute feedback from veriication results ***)
(*************************************************)

let patch_of_sol (a,b,c,d) = b

let has_no_regressions org_report report =
  (* let org_reg = BatSet.map get_fkey_target (FixTarget.get_regressions org_report) in
  let reg = BatSet.map get_fkey_target (FixTarget.get_regressions report) in *)
  let org_reg = BatSet.map (fun (kind,fsig,_,_) -> (kind,fsig)) (FixTarget.get_regressions org_report) in
  let reg = BatSet.map (fun (kind,fsig,_,_) -> (kind,fsig)) (FixTarget.get_regressions report) in
  BatSet.equal org_reg reg

let reward_core org_a cur_a no_regression len ~lpenalty_weight =
  if no_regression && cur_a <= org_a then
    (float_of_int (org_a - cur_a) -. (lpenalty_weight *. len)) /. (float_of_int org_a)

  (* else if no_regression && cur_a = org_a then
    -. (0.1 *. (float_of_int len)) *)

  else if not no_regression || cur_a > org_a then
    -. (1.0 +. len)
  else
    assert false
    (* (print_endline cand_file; assert false) *)

let reward_alarm cand_file patch lpenalty_weight =
  let org_a = (vulreport_in !outdir) |> FixTarget.get_alarms |> BatSet.cardinal in
  let cur_a = BatSet.cardinal (get_vul_alarms cand_file) in
  let org_reg_report = regreport_in !outdir in
  let cur_reg_report = cand_file |> Filename.dirname |> regreport_in in
  let no_regression = has_no_regressions org_reg_report cur_reg_report in
  let len = Patch.length2 patch in
  reward_core org_a cur_a no_regression len lpenalty_weight

let compute_reward cur_f patch ~lpenalty_weight =
  reward_alarm cur_f patch lpenalty_weight

(**********************)
(*** Learning Model ***)
(**********************)

let alpha : patch_comp list -> patch -> feature
= fun comps cand ->
  List.fold_left (fun acc comp ->
    if List.exists (fun c -> PatchComp.compare c comp = 0) cand then acc@[1]
    else acc@[0]
  ) [] comps

let train data =
  let m = Py.import "src.repair.policy" in
  let (xs, ys) = BatSet.to_list data |> List.split in (* xs: int list list, ys: float list *)
  let xs_py = List.map (Py.List.of_list_map Py.Int.of_int) xs |> Py.List.of_list in
  let ys_py = Py.List.of_list_map Py.Float.of_float ys in
  let (model,score,_,_) = Py.Module.get_function m "train" [|xs_py; ys_py|] |> Py.Tuple.to_tuple4 in
  (model, Py.Float.to_float score)
  (* let model = Py.Module.get_function m "tree_train" [|xs_py; ys_py|] in
  (model, 0.0) *)
