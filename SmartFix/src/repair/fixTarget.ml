open Options
open Query
open Lang
open Vocab

type t = kind * string * line * string option
and fix_target = t

let to_string_target : fix_target -> string
= fun (a,b,c,d) ->
  "(" ^ to_string_kind_simple a ^ ", " ^
  b ^ ", " ^
  string_of_int c ^ ", " ^
  (match d with Some s -> s | None -> "none") ^ ")"

let to_string_targets targets =
  string_of_set ~first:"    " ~sep:",\n    " ~last:"" to_string_target targets

let print targets = print_endline (to_string_targets targets)

let get_kind_target (a,b,c,d) = a
let get_fkey_target (a,b,c,d) = b
let get_line_target (a,b,c,d) = c
let get_exp_target (a,b,c,d) = d

let str_to_kind str =
  match str with
  | "IO" -> IO | "DZ" -> DZ | "KA" -> KILL
  | "ETH_LEAK" -> ETH_LEAK | "RE_EL" -> RE_EL | "RE" -> RE
  | "TX_ORG" -> TX_ORG
  | "NO_EFFECT" -> NO_EFFECT
  | "ASSIGN_CONST" -> ASSIGN_CONST
  | "DEAD" -> DEAD
  | _ -> failwith ("str_to_kind: " ^ str)

let rec str_to_typs' : string -> typ
= fun str ->
  if str = "dbytes" then EType DBytes
  else Translator.trans_str_to_typeName str

let str_to_typs str =
  match str with
  | "()" -> []
  | _ ->
    let str = snd (BatString.replace ~str:str ~sub:"(" ~by:"") in
    let str = snd (BatString.replace ~str:str ~sub:")" ~by:"") in
    let lst = BatString.split_on_char '_' str in
    List.map str_to_typs' lst

(* let str_to_fsig str =
  let fname,typs = BatString.split str ~by:"(" in
  let typs = str_to_typs ("(" ^ typs) in
  (fname,typs) *)

let str_to_line str = int_of_string str
let str_to_exp str = if str = "none" then None else Some str

let csv_header = ["no"; "kind"; "signature"; "line"; "exp"]

let read_report : string -> fix_target BatSet.t
= fun reportfile ->
  let rows = Csv.Rows.load ~has_header:true ~header:csv_header reportfile in
  List.fold_left (fun acc row ->
    (* let lst = Csv.Row.to_list row in *)
    let kind = str_to_kind (Csv.Row.find row "kind") in
    let fkey = Csv.Row.find row "signature" in
    let line = str_to_line (Csv.Row.find row "line") in
    let exp = str_to_exp (Csv.Row.find row "exp") in
    BatSet.add (kind, fkey, line, exp) acc
  ) BatSet.empty rows


let get_alarms : string -> fix_target BatSet.t
= fun reportfile ->
  let all = read_report reportfile in
  let _ = assert (BatSet.for_all (fun (kind,_,_,_) -> kind != NO_EFFECT && kind != ASSIGN_CONST && kind != DEAD) all) in
  all

let get_alarms2 : string -> fix_target BatSet.t * fix_target BatSet.t
= fun reportfile ->
  let all = get_alarms reportfile in
  let all_wo_leak = BatSet.filter (fun (kind,_,_,_) -> kind != ETH_LEAK) all in
  (all, all_wo_leak)

let get_regressions : string -> fix_target BatSet.t
= fun reportfile ->
  let all = read_report reportfile in
  let _ = assert (BatSet.for_all (fun (kind,_,_,_) -> kind = NO_EFFECT || kind = ASSIGN_CONST || kind = DEAD) all) in
  all
