open Vocab

let repdir_in outdir = Filename.concat outdir "reports"
let csvdir_in outdir = Filename.concat outdir "csv"
let patdir_in outdir = Filename.concat outdir "candidates"
let logdir_in outdir = Filename.concat outdir "log"

let vulreport_in outdir = Filename.concat (repdir_in outdir) "vulnerability_report.csv"
let regreport_in outdir = Filename.concat (repdir_in outdir) "regression_report.csv"

let json_report_in outdir = Filename.concat (repdir_in outdir) "verismart.json"

let knowledge_in outdir = Filename.concat outdir "knowledge.json"

(***********************)
(*** Make workspace  ***)
(***********************)

let mk_mmddhhmm () =
  let tm = Unix.localtime (Unix.time()) in
  let mm = zfill 2 (string_of_int (tm.tm_mon+1)) in
  let dd = zfill 2 (string_of_int tm.tm_mday) in
  let hh = zfill 2 (string_of_int tm.tm_hour) in
  let mm' = zfill 2 (string_of_int tm.tm_min) in
  mm ^ dd ^ "_" ^ hh ^ mm'

let setup () =
  let _ = if !Options.outdir="" then Options.outdir := Sys.getcwd () ^ "/repair_results/" ^ mk_mmddhhmm () in
  let root = !Options.outdir in
  List.iter (fun d -> assert (Sys.command ("mkdir " ^ d) = 0))
  [root; repdir_in root; csvdir_in root; patdir_in root; logdir_in root]

(* Does not make patch dir *)
let make_sub outroot =
  List.iter (fun d -> assert (Sys.command ("mkdir " ^ d) = 0))
  [outroot; repdir_in outroot; csvdir_in outroot; logdir_in outroot]

(****************************)
(*** Read from workspace  ***)
(****************************)

let get_vul_alarms file = file |> Filename.dirname |> vulreport_in |> FixTarget.get_alarms

let get_reg_alarms file = file |> Filename.dirname |> regreport_in |> FixTarget.get_regressions

let read_json file =
  let json = Yojson.Basic.from_file file in
  match Yojson.Basic.Util.member "inv_size" json with
  | `Int n -> n
  | _ -> assert false

let get_inv_size_cand cand_file = read_json (Filename.dirname cand_file ^ "/reports/verismart.json")
