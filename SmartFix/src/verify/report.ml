open Lang
open Query
open Options
open Yojson.Basic
open InvMap

let iter = ref 0

module QMap = struct
  module BatMap = BatMap.Make (struct type t = Query.qid let compare = Query.compare_qid end)

  type t = v BatMap.t
  and k = Query.qid
  and v = status * fkey BatSet.t * iter
  and iter = int
          
  let mk_key = Query.get_qid
  
  let add = BatMap.add
  let find = BatMap.find
  let empty = BatMap.empty
  let bindings = BatMap.bindings
  let for_all = BatMap.for_all
  let map = BatMap.mapi
end

let get_proven : (QMap.k * QMap.v) list -> (QMap.k * QMap.v) list
= fun lst -> List.filter (fun (_,(stat,_,_)) -> stat = Proven) lst

let get_unproven : (QMap.k * QMap.v) list -> (QMap.k * QMap.v) list
= fun lst -> List.filter (fun (_,(stat,_,_)) -> stat = UnProven) lst

let filter ~kind:k lst = List.filter (fun ((k',_,_),_) -> k=k') lst
let exclude ~kind:k lst = List.filter (fun ((k',_,_),_) -> not (k=k')) lst

let print_result_per_query lst =
  List.iteri (fun i ((k,l,s) as qid, (stat,fkey,iter)) ->
    let s1 = "[" ^ string_of_int (i+1) ^ "]" ^ " " ^ "[" ^ Query.to_string_kind_simple k ^ "]" ^ " " in
    let s2 = Query.to_string_qid qid ^ " : " ^ to_string_status stat in
    print_endline (s1 ^ s2);
  ) lst

let print : QMap.t -> QMap.t -> unit
= fun qmap reg_qmap ->
  let lst = QMap.bindings qmap in
  let reg_lst = QMap.bindings reg_qmap in
  let unproven_lst = get_unproven lst in
  (* let proven_lst = get_proven lst in *)
  print_endline "\n===== Vulnerability Report =====";
  print_result_per_query lst;

  if !check_reg then
    (print_endline "";
     print_endline "===== Regression Report =====";
     if List.length reg_lst = 0 then print_endline "- Regression report is empty"
     else print_result_per_query reg_lst);

  print_endline "";
  print_endline "============ Statistics ============";
  print_endline ("# Iter                    : " ^ string_of_int !iter);
  print_endline ("# Alarm / Query           : " ^ string_of_int (List.length unproven_lst) ^ " / " ^ string_of_int (List.length lst));
  if !check_io      then print_endline ("- integer over/underflow  : " ^ string_of_int (List.length (filter ~kind:IO unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:IO lst)));
  if !check_dz      then print_endline ("- division-by-zero        : " ^ string_of_int (List.length (filter ~kind:DZ unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:DZ lst)));
  if !check_assert  then print_endline ("- assertion violation     : " ^ string_of_int (List.length (filter ~kind:ASSERT unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:ASSERT lst)));
  if !check_kill    then print_endline ("- kill-anyone             : " ^ string_of_int (List.length (filter ~kind:KILL unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:KILL lst)));
  if !check_leak    then print_endline ("- ether-leaking           : " ^ string_of_int (List.length (filter ~kind:ETH_LEAK unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:ETH_LEAK lst)));
  if !check_re      then print_endline ("- reentrancy-leaking      : " ^ string_of_int (List.length (filter ~kind:RE_EL unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:RE_EL lst)));
  if !check_re      then print_endline ("- reentrancy              : " ^ string_of_int (List.length (filter ~kind:RE unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:RE lst)));
  if !check_tx      then print_endline ("- tx.origin               : " ^ string_of_int (List.length (filter ~kind:TX_ORG unproven_lst)) ^ " / " ^ string_of_int (List.length (filter ~kind:TX_ORG lst)))

let proved_nontrivially : (QMap.k * QMap.v) list -> int
= fun lst ->
  let lst' = List.filter (fun (_,(stat,_,i)) -> stat = Proven && i > 1) lst in
  List.length lst'

let mk_json_lst qmap =
  List.mapi (fun i ((kind,line,str),(stat,fkeys,iter)) ->
    `Assoc [("no", `String (string_of_int (i+1)));
            ("kind", `String (to_string_kind_simple kind));
            ("line", `Int line);
            ("signatures", `List (fkeys
                                  |> BatSet.to_list
                                  |> List.map (fun (cname,fname,typs) ->
                                      `Assoc [("contractName", `String cname);
                                              ("methodName", `String fname);
                                              ("argTypes", `List (typs |> List.map (fun t -> `String (to_string_typ t))))])));
                                              ("exp", `String str);
                                              ("status", `String (to_string_status stat))]
  ) (QMap.bindings qmap)

let csv_header = ["no"; "kind"; "signature"; "line"; "exp"]

let qlst_to_csv global lst =
  List.mapi (fun i ((kind,line,str),(stat,_,iter)) ->
    let no = string_of_int (i+1) in
    let kind = to_string_kind_simple kind in
    let fkey = try to_string_fkey2 (get_fkey (Global.find_func_containing_line line global)) with Not_found -> "none" in
    let line = string_of_int line in
    let exp = str in
    [no; kind; fkey; line; exp]
  ) lst

(* To avoid misguiding repair procedure, as state-chaning related alarms are more than RE-EL alarms. *)
let adjust_re_alarms all unproven =
  let re_el_qs = filter ~kind:RE_EL all in
  let re_el_unproven = filter ~kind:RE_EL unproven in
  let re_qs = filter ~kind:RE all in
  let re_unproven = filter ~kind:RE unproven in

  if List.length re_qs > 0 && List.length re_unproven = 0 then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE (* re-safe *)

  else if List.length re_el_qs > 0 && List.length re_el_unproven = 0 then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE (* re-safe *)

  else if List.length re_el_qs > 0 && List.length re_el_unproven > 0 then
    unproven |> exclude ~kind:RE

  else
    let _ = assert (List.length re_el_qs = 0 && List.length re_el_unproven = 0) in
    unproven

  (* let re_el_qs = filter ~kind:RE_EL all in
  let re_qs = filter ~kind:RE all in
  let re_unproven = filter ~kind:RE unproven in
  let re_el_safe_meta = List.length re_qs > 0 && List.length re_unproven = 0 in
  if re_el_safe_meta then
    unproven |> exclude ~kind:RE_EL |> exclude ~kind:RE
  else if List.length re_el_qs = 0 then
    unproven
  else
    unproven |> exclude ~kind:RE *)

let csv_report_alarms : string -> int
= fun reportfile ->
  let rows = Csv.Rows.load ~has_header:true ~header:FixTarget.csv_header reportfile in
  List.length rows

let mk_vul_report ?(report="vulnerability_report.csv"): Global.t -> QMap.t -> string -> unit
= fun global qmap outdir ->
  let all = QMap.bindings qmap in
  let unproven = get_unproven all in
  let adjusted = adjust_re_alarms all unproven in
  let csv = [csv_header] @ (qlst_to_csv global adjusted) in
  let dir = outdir ^ report in
  Csv.save dir csv

let mk_reg_report : Global.t -> QMap.t -> string -> unit
= fun global qmap outdir ->
  let lst = QMap.bindings qmap in
  let lst = get_proven lst in
  let csv = [csv_header] @ (qlst_to_csv global lst) in
  let dir = outdir ^ "regression_report.csv" in
  Csv.save dir csv

let mk_json_invmap : InvMap.t -> Yojson.Basic.t
= fun invmap ->
  List.map (fun (k,d) ->
    `Assoc [("key", `String (InvMap.to_string_key k));
            ("formula", `String (Parse.to_string_vformula2 d))]
  ) (InvMap.bindings invmap)
  |> (fun res -> `List res)

let mk_json_specmap : SpecMap.t -> Yojson.Basic.t
= fun specmap ->
  List.map (fun (k,(pre,post)) ->
    `Assoc [("key", `String (SpecMap.to_string_key k));
            ("pre", `String (Parse.to_string_vformula2 pre));
            ("post", `String (Parse.to_string_vformula2 post))]
  ) (SpecMap.bindings specmap)
  |> (fun res -> `List res)

let mk_json_report : Global.t -> QMap.t -> QMap.t -> InvMap.t -> SpecMap.t -> unit
= fun global qmap reg_qmap invmap specmap ->
  let vul_json = mk_json_lst qmap in
  let reg_json = mk_json_lst reg_qmap in
  let j =
    `Assoc [("fileName", `String !inputfile);
            ("baseName", `String (Filename.basename !inputfile));
            ("iter", `Int !iter);
            ("time", `Float !Profiler.end_cpu);
            ("errMsg", `Null);

            ("invmap", mk_json_invmap invmap);
            ("specmap", mk_json_specmap specmap);

            ("cei_violated", `Bool !PatternAnalysis.may_violate_cei);
            ("vul_result", `List vul_json);
            ("reg_result", `List reg_json)] in
  let base = snd (BatString.replace (Filename.basename !inputfile) ".sol" "") in
  let outdir = if !Options.outdir = "" then "./output/" else !Options.outdir ^ "/" in
  let full_fname = outdir ^ base ^ ".json" in
  let fp = open_out full_fname in
  Printf.fprintf fp "%s" (Yojson.Basic.pretty_to_string j);
  close_out fp;
  mk_vul_report global qmap outdir;
  if !Options.check_reg then mk_reg_report global reg_qmap outdir

let unproven_lst_from_json_report : string -> (string * int * string) list
= fun fname ->
  fname
  |> Yojson.Basic.from_file
  |> Json.value_of "vul_result" |> Json.to_list
  |> List.filter (fun j -> j |> Json.value_of "status" |> Json.to_string = "unproven")
  |> List.map
     (fun j ->
       (j |> Json.value_of "kind" |> Json.to_string,
        j |> Json.value_of "line" |> Json.to_int,
        j |> Json.value_of "exp" |> Json.to_string))
