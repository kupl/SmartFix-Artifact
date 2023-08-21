open Lang
open Translator
open Vocab
open Options
open Verifier
open Report
open ItvDom
open Profiler
open Feature
open Query
open Solc

let set_default_inline_depth () =
  if !Options.inline_depth < 0 then
    if !Options.mode="exploit" then Options.inline_depth := 3
    else Options.inline_depth := 2
  else ()

let preprocess file =
  let lines = BatList.of_enum (BatFile.lines_of file) in
  let _ = Z3Interface.ctx := Z3.mk_context [("timeout",string_of_int (!Options.z3timeout))] in
  let _ = set_default_inline_depth () in
  let json_ast = Solc.get_json_ast !Options.solc_ver file in
  let pgm = Translator.run json_ast lines in
  let _ = main_contract := get_cname (get_main_contract pgm) in
  let pgm = Preprocess.run pgm in
  let pgm = Preprocess2.run pgm in
  let pgm = if !Options.mode="exploit" then ExploitPreprocess.run pgm else pgm in
  let pgm = MakeCfg.run pgm in
  let pgm = Inline.run pgm in (* inlining is performed on top of an initial cfg. *)
  let global = Global.make_global_info pgm lines in
  (pgm, global, lines)


let record_elapsed_time () =
  end_cpu := Sys.time () -. !Profiler.start_cpu;
  end_real := Unix.gettimeofday () -. !Profiler.start_real

let print_elapsed_time () =
  print_endline "";
  print_endline ("Time Elapsed (Real) : " ^ string_of_float !end_real);
  print_endline ("Time Elapsed (CPU)  : " ^ string_of_float !end_cpu)

let do_mode_job (pgm,global,lines) =
  let paths = pgm |> CallGraph.remove_unreachable_funcs |> Path.generate in
  match !Options.mode with
  | "exploit" ->
     let qmap = Exploit.run global paths in
     ExploitReport.report ~pathnum:(Path.PathSet.cardinal paths) global qmap lines;
     record_elapsed_time ();
     if !Options.mk_report then ExploitReport.json global qmap else ()
  | "verify" ->
     let _ = PatternAnalysis.run global pgm paths in
     let mem = ItvAnalysis.run pgm global paths in
     let global = {global with mem = mem} in
     let (qmap,reg_qmap,invmap,specmap) = Verifier.run global mem paths in
     Report.print qmap reg_qmap;
     record_elapsed_time ();
     if !Options.mk_report then Report.mk_json_report global qmap reg_qmap invmap specmap;
  | "repair" ->
     let _ = PatternAnalysis.run global pgm paths in
     let mem = ItvAnalysis.run pgm global paths in
     let global = {global with mem = mem} in
     let result = Repair.run global lines pgm in
     record_elapsed_time ();
     ReportRepair.print result
  | _ -> assert false

let setup_outdir () =
  if !Options.mode = "repair" then WorkspaceRepair.setup ()
  else () (* assume outdir already exists for other options *)

let main () =
   (activate_default_detector_if_unspecified ();
    print_detector_activation_status ();
    let (pgm,global,lines) = preprocess !inputfile in
    if !Options.il then print_endline (to_string_pgm pgm)
    else if !Options.cfg then print_endline (to_string_cfg_p pgm)
    else
      let _ = setup_outdir () in
      do_mode_job (pgm,global,lines))

let json_err : string -> unit
= fun msg ->
  let j =
    `Assoc [("fileName", `String !Options.inputfile);
            ("baseName", `String (Filename.basename !inputfile));
            ("time", `String (string_of_float !Profiler.end_cpu));
            ("errMsg", `String msg);
            ("result", `List [])] in
  let base = snd (BatString.replace (Filename.basename !inputfile) ".sol" "") in
  let fname =
    if !Options.outdir = "" then "./output/" ^ base ^ ".json"
    else !Options.outdir ^ "/" ^ base ^ ".json" in
  let f = open_out fname in
  Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string j);
  close_out f

let _ =
  let usageMsg = "./main.native -input filename" in
  Arg.parse options activate_detector usageMsg;
  Printexc.record_backtrace true;
  try
    let _ = main () in
    print_elapsed_time ()
  with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace());
    (match exc with
     | CompilationError -> if !Options.mk_report then json_err "Compilation Error"
     | UnsupportedSolc -> if !Options.mk_report then json_err "Unsupported Solc Provided"
     | Solution_Inv_Not_Hold -> if !Options.mk_report then json_err "Solution Inv Does Not Hold"
     | _ -> if !Options.mk_report then json_err "Runtime Exception")
