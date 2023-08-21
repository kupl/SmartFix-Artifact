open Options
open Vocab
open WorkspaceRepair

let cei_violated_in_s0 inputfile =
  let _ = assert (Filename.basename inputfile = "patch.sol") in
  repdir_in !outdir ^ "/" ^ "verismart.json"
  |> Yojson.Basic.from_file |> Json.value_of "cei_violated" |> Json.to_bool

let cmd_patch_verify compare_target inputfile =
  let basic =
    ["-repair_pverify"]
    @ (if cei_violated_in_s0 inputfile then ["-re_safety_enforce"] else []) in
  if !Options.repair_no_diff then basic
  else (* optimization via diff verification *)
    basic
    @ (if not (compare_target = "") then ["-repair_compdir"; compare_target] else [])

let cmd_change_summary_filename outroot_dir inputfile =
  ["mv"; (* candidates/cand_1/reports/patch.json -> candidates/cand_1/reports/verismart.json *)
   repdir_in outroot_dir ^ "/" ^ (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".json"));
   repdir_in outroot_dir ^ "/" ^ "verismart.json"]

let get_cmd ~org ~compare_target outroot_dir inputfile =
  ["timeout"; string_of_int 1200; (* string_of_int (!Options.repair_tool_timeout + 300); *)
   "./main.native"; "-input"; inputfile; "-main"; !Options.main_contract;
   "-report"; "-verbose";
   "-solv"; if !Options.solc_ver="" then "0.4.25" else !Options.solc_ver;
   "-mode"; "verify"; "-refined_vcgen"; "-uninterp_nonlinear"; "-alarm_filter";
   "io"; "kill"; "leak"; "re"; "tx"; "reg";
   "-verify_timeout"; string_of_int !Options.repair_tool_timeout;
   "-z3timeout"; string_of_int !Options.z3timeout]

  @ (if not org then cmd_patch_verify compare_target inputfile else ["-repair_overify"])

  @ ["-outdir"; repdir_in outroot_dir]
  @ [">"; logdir_in outroot_dir ^ "/" ^ "verismart.txt"; "2>&1"] (* https://linuxize.com/post/bash-redirect-stderr-stdout/ *)
  |> string_of_list ~first:"" ~last:"" ~sep:" " Vocab.id
  (* assert (Sys.command cmd = 0) *)

let verify_patch compare_target cand_dir =
  let inputfile = Filename.concat cand_dir "patch.sol" in
  let cmd = get_cmd ~org:false ~compare_target cand_dir inputfile in
  let before = Unix.gettimeofday () in
  let _ = Sys.command cmd in
  let time = Unix.gettimeofday () -. before in
  let json_report = repdir_in cand_dir ^ "/" ^ (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".json")) in
  let _ =
    if not (Sys.file_exists json_report) then
      Json.mk_err_report ~filename:inputfile ~errmsg:"Unknown" ~time:time ~reportdir:(repdir_in cand_dir) in
  let mv_cmd = string_of_list ~first:"" ~last:"" ~sep:" " Vocab.id (cmd_change_summary_filename cand_dir inputfile) in
  let _ = Sys.command mv_cmd in
  let f = open_out (Filename.concat (logdir_in cand_dir) "verismart_cmd.txt") in
  Printf.fprintf f "%s" cmd;
  close_out f

let verify_s0 inputfile outroot_dir =
  let cmd = get_cmd ~org:true ~compare_target:"" outroot_dir inputfile in
  let before = Unix.gettimeofday () in
  let _ = Sys.command cmd in
  let time = Unix.gettimeofday () -. before in
  let reportfile = repdir_in outroot_dir ^ "/" ^ (snd (BatString.replace ~str:(Filename.basename inputfile) ~sub:".sol" ~by:".json")) in
  let _ =
    if not (Sys.file_exists reportfile) then
      Json.mk_err_report ~filename:inputfile ~errmsg:"Unknown" ~time:time ~reportdir:(repdir_in outroot_dir) in
  let mv_cmd = string_of_list ~first:"" ~last:"" ~sep:" " Vocab.id (cmd_change_summary_filename outroot_dir inputfile) in
  let _ = Sys.command mv_cmd in
  let f = open_out (logdir_in outroot_dir ^ "/" ^ "verismart_cmd.txt") in
  Printf.fprintf f "%s" cmd;
  close_out f
