open Options
open Vocab
open Semantics
open Lang
open Vlang
open Path
open Patch
open Patch.PatchComp
open WorkspaceRepair
open FixTarget
open Query
open OfflineLearning
open OnlineLearning

type alarms = fix_target BatSet.t

type solution = (file * patch * found_iter * found_time)
and file = string
and found_iter = int
and found_time = float

let iter = ref 0

let first_sol_found_iter = ref 0
let first_sol_found_time = ref 0.0
let first_sol_dir = ref ""

let nr_penalty = ref false

let set_nr_penalty no_regressions alarms_patch patch =
  let b1 = BatSet.for_all (fun (kind,_,_,_) -> kind != RE_EL && kind != RE) alarms_patch in
  let b2 = not (Patch.contain_nr patch) in
  if b1 && b2 && no_regressions && (not !nr_penalty) then (* this case means: NR is unlikely to be useful *)
    nr_penalty := true
  else ()

let uncompiled : string list ref = ref []
let patch_err  : string list ref = ref []
let verify_err : string list ref = ref []

let uncompiled_len1 : string list ref = ref []
let patch_err_len1  : string list ref = ref []
let verify_err_len1 : string list ref = ref []

let run_patch_verifier : solution list -> file -> bool * string
= fun solutions file ->
  let _ = assert (Solc.is_compilable !Options.solc_ver file) in
  let _ = assert (List.length solutions <= 1) in
  let cand_dir = Filename.dirname file in
  let best_cand_dir =
    if List.length solutions = 1 then List.hd solutions |> (fun (f,_,_,_) -> Filename.dirname f)
    else ""
  in
  let _ = RunTool.verify_patch best_cand_dir cand_dir in
  let j = Yojson.Basic.from_file (json_report_in cand_dir) in
  match Yojson.Basic.Util.member "errMsg" j with
  | `Null -> (true, "")
  | `String "Unsupported Solc Provided" -> assert false
   (* returncode is not nonzero when error happens *)
   (* hence, directly check summary file to see if errors happened *)
  | `String str ->
     (match str with
      | "Runtime Exception" -> (false, str)
      | "Compilation Error" -> (false, str)
      | "Unknown" -> (false, str)
      | "Solution Inv Does Not Hold" -> (false, str)
      | _ -> assert false)
  | _ -> assert false

let train tr_set =
  if !Options.repair_no_learn then (Py.none, None)
  else
    let (model, fit_score) = OnlineLearning.train tr_set in
    (model, Some fit_score)

let model_score model comps patch = (* predict safety score using a model *)
  let hat = alpha comps patch in
  let m = Py.import "src.repair.policy" in
  let hatpy = List.map (Py.List.of_list_map Py.Int.of_int) [hat] |> Py.List.of_list in
  let score = Py.Module.get_function m "predict" [|model; hatpy|] |> Py.Float.to_float in
  score

let is_subset : patch_comp list -> patch -> solution -> bool
= fun comps cand sol ->
  let sol_feat = alpha comps (patch_of_sol sol) in
  let cand_feat = alpha comps cand in
  List.for_all2 (fun a' b' -> not (a'=1 && b'=0)) cand_feat sol_feat

let cand_is_subset_of_sols : patch_comp list -> solution list -> patch -> bool
= fun comps sols cand ->
  (* let _ = assert (!first_sol_found_iter = 0 || List.length sols > 0) in *) (* solution found -> |sols| > 0 *)
  let _ = assert (!first_sol_found_iter = 0 || List.length sols = 1) in
  List.for_all (is_subset comps cand) sols

let contain_redundant_insert : Global.t -> patch -> patch_comp list -> bool
= fun global patch comps ->
  List.exists (fun p ->
    List.exists (fun c ->
      match p,c with
      | Atom (InsertLine (l1, s1, is_bc1)), Atom (InsertLine (l2, s2, is_bc2)) ->
        let f1, f2 = Global.find_func_containing_line l1 global, Global.find_func_containing_line l2 global in
        let fkey1, fkey2 = Lang.get_fkey f1, Lang.get_fkey f2 in
        is_bc1 && is_bc2 && fkey1 = fkey2 && l1 > l2
        && to_string_stmt s1 = to_string_stmt s2
      | _ -> false
    ) comps
  ) patch

let compute_penalty global comps solutions patch =
  (* give penalty when: solution is found, but the cand is not a subset of solutions *)
  (* let penalty1 = if not (!first_sol_found_iter = 0) && not (cand_is_subset_of_sols comps solutions patch) then 0.8 else 0.0 in *)
  let penalty1 = if contain_redundant_insert global patch comps then 0.5 else 0.0 in
  (* let penalty2 = if !nr_penalty && Patch.contain_nr patch then 0.5 else 0.0 in *)
  penalty1
  (* BatList.max [penalty1; penalty2] *) (* to ensure length-based exploration first *)

let rec mk idx len acc =
  if idx = len - 1 then acc @ [OfflineLearning.mk_onehot_vector idx len]
  else mk (idx+1) len (acc @ [OfflineLearning.mk_onehot_vector idx len])

(* E.g., mk_all_onehot_vectors 3 => [[1; 0; 0]; [0; 1; 0]; [0; 0; 1]] *)
let mk_all_onehot_vectors len = mk 0 len []

let patch_comp_explored_all comps tr_set =
  let tr_vectors = BatSet.map fst tr_set in
  let all_onehots = mk_all_onehot_vectors (List.length comps) in
  let _ = assert (List.for_all (fun onehot -> BatList.sum onehot = 1) all_onehots) in
  let tested_comps = List.length (List.filter (fun onehot -> BatSet.mem onehot tr_vectors) all_onehots) in
  List.length comps <= tested_comps + List.length !patch_err_len1 + List.length !uncompiled_len1

let score model_off model_on tr_set comps patch =
  if !Options.repair_no_learn then -. (Patch.length2 patch)
  else
    if not (model_off = Py.none) then
      model_score model_off comps patch

    else if model_off = Py.none && not (patch_comp_explored_all comps tr_set) then
      -. (Patch.length2 patch)

    else if model_off = Py.none && (patch_comp_explored_all comps tr_set) then
      let _ = assert (not (model_on = Py.none)) in
      model_score model_on comps patch

    else assert false

let cost global model_off model_on tr_set comps solutions patch =
  let score = score model_off model_on tr_set comps patch in
  let penalty = compute_penalty global comps solutions patch in
  -. (score -. penalty)

module Workset = struct
  type elem = file * patch * float
  
  module OrderedType = struct
    type t = elem
    let compare (f1,p1,cost1) (f2,p2,cost2) = Stdlib.compare cost1 cost2
  end

  module Heap = BatHeap.Make (OrderedType)

  type t = Heap.t

  let empty = Heap.empty
 
  let pick_min : t -> elem option
  = fun heap ->
    try Some (Heap.find_min heap)
    with _ -> None

  let remove_min : t -> t
  = fun heap -> Heap.del_min heap

  let add : elem -> t -> t
  = fun elem heap -> Heap.add elem heap

  let workset_info : t -> string
  = fun heap -> "# of Workset Elements : " ^ string_of_int (Heap.size heap)

  let reconstruct f : t -> t
  = fun heap ->
    let elems = Heap.to_list heap in
    let elems' = List.map (fun (file,patch,_) -> (file, patch, f patch)) elems in
    let heap' = list_fold add elems' empty in
    let _ = assert (Heap.size heap = Heap.size heap') in
    heap'

  let filter sol_patch : t -> t
  = fun heap ->
    let elems = Heap.to_list heap in
    let elems' = List.filter (fun (_,patch,_) -> Patch.length2 patch < Patch.length2 sol_patch) elems in
    let heap' = list_fold add elems' empty in
    let _ = assert (Heap.size heap' <= Heap.size heap) in
    heap'
end

let gen_patch : patch_comp list -> patch -> patch list
= fun components cand -> GenPatch.gen_candidates components cand

let gen_candidates : patch_comp list -> patch -> patch list
= fun components cand -> gen_patch components cand

let cand_id = ref 0
let new_cand_id () = (cand_id:=!cand_id+1; !cand_id)

let make_files candroot (fname1,str1) (fname2,str2) =
  let _ = WorkspaceRepair.make_sub candroot in
  let fp1,fp2 = open_out (candroot ^ "/" ^ fname1), open_out (candroot ^ "/" ^ fname2) in
  Printf.fprintf fp1 "%s" str1; Printf.fprintf fp2 "%s" str2;
  close_out fp1; close_out fp2

let add2 : Global.t -> string list -> Py.Object.t -> Py.Object.t ->
           tr_set -> patch_comp list -> solution list -> patch ->
           Workset.t * string BatSet.t ->
           Workset.t * string BatSet.t
= fun global lines model_off model_on tr_set comps solutions patch (workset,explored) ->
  let (src_str, exception_encountered) = try (Patch.make_source_code global patch lines, false) with _ -> ("", true) in
  if BatSet.mem src_str explored then
    (workset,explored)
  else
    let id = new_cand_id () in
    let candroot = patdir_in !outdir ^ "/" ^ "cand_" ^ string_of_int id in
    let patch_fname = "patch.sol" in
    let _ = make_files candroot (patch_fname,src_str) ("patch.txt", Patch.to_string patch) in
    let patch_fullfname = candroot ^ "/" ^ patch_fname in
    if exception_encountered then
      let _ = patch_err := !patch_err @ [patch_fullfname] in
      let _ = if Patch.length patch = 1 then patch_err_len1 := !patch_err_len1 @ [patch_fullfname] in
      (workset, explored)
    else if not (Solc.is_compilable !Options.solc_ver patch_fullfname) then
      let _ = uncompiled := !uncompiled @ [patch_fullfname] in
      let _ = if Patch.length patch = 1 then uncompiled_len1 := !uncompiled_len1 @ [patch_fullfname] in
      (workset, explored)
    else
      let cost = cost global model_off model_on tr_set comps solutions patch in
      (Workset.add (patch_fullfname,patch,cost) workset, BatSet.add src_str explored)

let print_log global pgm file patch cur_cost model_off model_on comps =
  let score_off = if model_off = Py.none then "bot" else string_of_float (model_score model_off comps patch) in
  let score_on = if model_on = Py.none then "bot" else string_of_float (model_score model_on comps patch) in
  print_endline ("- Iter : " ^ string_of_int !iter);
  print_endline ("- Elapsed time : " ^ string_of_float (Unix.gettimeofday () -. !Profiler.start_real));
  print_endline ("- Candidate chosen : " ^ file);
  print_endline ("- Current cost : " ^ string_of_float cur_cost);
  print_endline ("- Score (offline model) : " ^ score_off);
  print_endline ("- Score (online model)  : " ^ score_on)

let print_log_err global pgm file patch err_msg =
  if err_msg = "Solution Inv Does Not Hold" then
    print_endline ("- " ^ err_msg ^ " !\n")
  else
    print_endline ("- Verification error !\n")

let print_log2 global pgm file patch (feat, reward) fit_score comps =
  print_endline ("- Computed reward : " ^ string_of_float reward);
  print_endline ("- Patch : " ^ Patch.to_string_oneline patch);
  print_endline ("- Abs_Patch :\n" ^ to_string_abs_patch (OfflineLearning.abstract global pgm patch));
  print_endline ("- Patch representation : " ^ to_string_feature feat);
  print_endline ("- Model fitting score : " ^ (match fit_score with Some s -> string_of_float s | None -> "N/A"));
  print_endline ""

let print_solution_found alarms_patch (t1,file) is_if =
  (if BatSet.cardinal alarms_patch = 0 then
    print_endline ("- Solution found, " ^ "iter " ^ string_of_int !iter ^ ", " ^ string_of_float t1 ^ "s" ^ " : " ^ file);
  if BatSet.cardinal alarms_patch = 0 && !first_sol_found_iter = 0 then
    let _ = assert (is_if) in
    first_sol_found_iter := !iter; first_sol_found_time := t1; first_sol_dir := file)

let update_solutions file patch alarms_patch no_regressions (solutions, min_alarm_num, min_patch_size) =
  if no_regressions && BatSet.cardinal alarms_patch < min_alarm_num then
    let t1 = Unix.gettimeofday () -. !Profiler.start_real in
    let _ = print_endline ("- New best candidate, " ^ "iter " ^ string_of_int !iter ^ ", " ^ string_of_float t1 ^ "s" ^ " : " ^ file) in
    let _ = print_solution_found alarms_patch (t1,file) true in
    let new_sol = (file, patch, !iter, t1) in
    ([new_sol], BatSet.cardinal alarms_patch, Patch.length2 patch)

  else if no_regressions && BatSet.cardinal alarms_patch = min_alarm_num && Patch.length2 patch < min_patch_size then
    let t1 = Unix.gettimeofday () -. !Profiler.start_real in
    let new_sol = (file, patch, !iter, t1) in
    let _ = print_endline ("- New best candidate, " ^ "iter " ^ string_of_int !iter ^ ", " ^ string_of_float t1 ^ "s" ^ " : " ^ file) in
    let _ = print_solution_found alarms_patch (t1,file) false in
    ([new_sol], min_alarm_num, Patch.length2 patch)

  else (solutions, min_alarm_num, min_patch_size)

let debug_invalidate cur_score =
  (print_endline "- Invalidate offline model !";
   print_endline ("current iter : " ^ string_of_int !iter);
   print_endline ("computed score : " ^ string_of_float cur_score))

let is_off_model_unuseful model_off new_data =
  if model_off = Py.none then false
  else
    let cur_score = (snd new_data) in
    let b1 = cur_score < 0.0 in (* using model_off was not successful in the current iteration *)
    let b2 = !first_sol_found_iter = 0 && !iter >=5 in (* assume similar vulnerable code snippets are not so many *)
    b1 || b2

let repair_loop_bound = 10

let debug_loop iter workset =
  (prerr_string ("Iter : " ^ string_of_int iter ^ ", ");
   prerr_endline
   (Workset.workset_info workset ^ ", "
    ^ "Total elapsed : " ^ string_of_float (Unix.gettimeofday () -. !Profiler.start_real)))

let rec loop : Global.t -> (string list * Lang.pgm) -> alarms -> int -> float -> patch_comp list ->
               Py.Object.t -> Py.Object.t -> tr_set ->
               Workset.t * string BatSet.t ->
               solution list * knowledge BatSet.t -> solution list * knowledge BatSet.t
= fun global (lines,pgm) alarms_org min_alarm_num min_patch_size comps model_off model_on tr_set (workset, explored) (solutions, knowledges) ->
  let iterated_enough = !first_sol_found_iter > 0 && !iter - !first_sol_found_iter >= repair_loop_bound in
  let timeout = Unix.gettimeofday () -. !Profiler.start_real > float_of_int !Options.repair_loop_timeout in
  if iterated_enough || timeout then (solutions,knowledges)
  else
  let _ = iter := !iter + 1 in
  let _ = if !iter mod 10 = 1 then debug_loop !iter workset in
  match Workset.pick_min workset with
  | None -> (solutions, knowledges)
  | Some (file, patch, cur_cost) ->
    let _ = if !Options.debug = "repair" then print_log global pgm file patch cur_cost model_off model_on comps in
    let workset = Workset.remove_min workset in
    let (no_error, err_msg) = run_patch_verifier solutions file in
    if not (no_error) then (* verifier runtime exception check *)
      let _ = if !Options.debug = "repair" then print_log_err global pgm file patch err_msg in
      let _ = if err_msg <> "Solution Inv Does Not Hold" then verify_err := !verify_err @ [file] in
      loop global (lines,pgm) alarms_org min_alarm_num min_patch_size comps model_off model_on tr_set (workset,explored) (solutions,knowledges)

    else
      let alarms_patch = file |> Filename.dirname |> vulreport_in |> FixTarget.get_alarms in
      let no_regressions = has_no_regressions (regreport_in !outdir) (file |> Filename.dirname |> regreport_in) in
      let _ = if not no_regressions then print_endline ("- Regression found, " ^ file) in
      let _ = set_nr_penalty no_regressions alarms_patch patch in
      let (solutions, min_alarm_num, min_patch_size) = update_solutions file patch alarms_patch no_regressions (solutions, min_alarm_num, min_patch_size) in

      (* Derive a model online and store a knowldge *)
      let score = compute_reward file patch ~lpenalty_weight:0.5 in
      let new_data = (OnlineLearning.alpha comps patch, score) in
      let tr_set = BatSet.add new_data tr_set in
      let (model_on, fit_score) = train tr_set in
      let new_knowledge = (OfflineLearning.abstract global pgm patch, score) in
      let knowledges = BatSet.add new_knowledge knowledges in
      let off_unuseful = is_off_model_unuseful model_off new_data in
      let (model_off, tr_set, model_on, fit_score) =
        if not off_unuseful then (model_off, tr_set, model_on, fit_score)
        else (* invalidate model_off *)
          let _ = if !Options.debug = "repair" then debug_invalidate (snd new_data) in
          let model_off = Py.none in
          let tr_set = BatSet.filter (fun (feat,score) -> BatList.sum feat = 1) tr_set in
          let (model_on, fit_score) = train tr_set in (* retraining on refined dataset *)
          (model_off, tr_set, model_on, fit_score) in

      (* let model_off =
        if not off_unuseful then model_off
        else (if !Options.debug = "repair" then debug_invalidate (snd new_data); Py.none)
      in *)

      (* Generate *)
      let new_cands = gen_candidates comps patch in
      let workset,explored = list_fold (add2 global lines model_off model_on tr_set comps solutions) new_cands (workset,explored) in

      (* if solution found, remove patches larger than solution patches *)
      let workset =
        if !first_sol_found_iter = 0 then workset
        else
          let _ = assert (!first_sol_found_iter > 0) in
          let sol_patch = assert (List.length solutions = 1); List.hd solutions |> (fun (_,p,_,_) -> p) in
          Workset.filter sol_patch workset
      in

      (* Re-sorting *)
      let workset = Workset.reconstruct (cost global model_off model_on tr_set comps solutions) workset in
      let _ = if !Options.debug = "repair" then print_log2 global pgm file patch new_data fit_score comps in
      loop global (lines,pgm) alarms_org min_alarm_num min_patch_size comps model_off model_on tr_set (workset,explored) (solutions,knowledges)


let get_reportfile () =
  if !repair_report="" then
    (ignore (Profiler.run "[STEP] Run analyzer" (RunTool.verify_s0 !inputfile !outdir));
     vulreport_in !outdir)
  else
     let _ = assert (Sys.command ("cp " ^ !repair_report ^ " " ^ vulreport_in !outdir) = 0) in
     vulreport_in !outdir


let run : Global.t -> string list -> Lang.pgm -> solution list
= fun global lines pgm ->
  let _ = assert (not (List.mem "SmartFix" (fst (BatList.split_at (List.length global.base_names -1) global.base_names)))) in
  let reportfile = get_reportfile () in
  let fix_targets = Profiler.run "[STEP] Collect fix targets" (FixTarget.get_alarms reportfile) in
  print_endline (to_string_targets fix_targets);
  print_endline "";
  if BatSet.cardinal fix_targets = 0 then
    (print_endline "[INFO] No fix targets exist, hence terminates."; [])
  else
    let _ = Py.initialize() in
    let _ = iter := 0 in
    let comps = GenPatch.extract_patch_comps global pgm fix_targets in
    let _ = print_endline ("[INFO] # of basic patch components : " ^ string_of_int (List.length comps)) in
    let _ = print_endline ("[INFO] extracted components\n" ^ PatchComp.to_string_comps comps) in
    let alarms = fix_targets in
    let min_alarm = BatSet.cardinal alarms in
    let init_cands = gen_patch comps [] in

    let empty_tr_set = BatSet.empty in
    let solutions = [] in
    let knowledges = BatSet.empty in

    let transferred = OfflineLearning.load_knowledges global pgm comps in
    let (model_off, _) = if BatSet.is_empty transferred then (Py.none, 0.0) else OnlineLearning.train transferred in
    let workset,explored = list_fold (add2 global lines model_off Py.none empty_tr_set comps []) init_cands (Workset.empty,BatSet.empty) in

    let (solutions, knowledges) =
      Profiler.run "[STEP] Enter Repair Loop"
      (loop global (lines,pgm) alarms min_alarm 0.0 comps model_off Py.none empty_tr_set (workset,explored) (solutions,knowledges))
    in

    let _ = OfflineLearning.store_new_knowledges global pgm knowledges in
    let _ = OfflineLearning.combine_knowledges () in

    solutions
