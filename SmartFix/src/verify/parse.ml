open Lang
open Vlang
open InvMap
open SpecMap
open Vocab
open Global
open Component
open Path

exception VarMatchingFailed

(*****************************************)
(*****************************************)
(*** Generate Strings for easy parsing ***)
(*****************************************)
(*****************************************)

(* owner__@2 -> owner *)
let peel_lab : var -> var
= fun (x,t) -> try (fst (BatString.split x "__@"), t) with Not_found -> (x,t)

let to_string_var (x,t) = x ^ ":" ^ to_string_typ t

let rec to_string_vformula2 vf =
  match vf with
  | VTrue -> "true"
  | VFalse -> "false"
  | VNot f -> "lnot(" ^ to_string_vformula2 f ^ ")"
  | VAnd (f1,f2) -> "land(" ^ to_string_vformula2 f1 ^ "," ^ to_string_vformula2 f2 ^ ")"
  | VOr (f1,f2) -> raise NotImplemented
  | VBinRel (vbrel,ve1,ve2) ->
    (match vbrel with
     | VGeq -> "geq(" ^ to_string_vexp2 ve1 ^ "," ^ to_string_vexp2 ve2 ^ ")"
     | VGt -> "gt(" ^ to_string_vexp2 ve1 ^ "," ^ to_string_vexp2 ve2 ^ ")"
     | VEq -> "eq(" ^ to_string_vexp2 ve1 ^ "," ^ to_string_vexp2 ve2 ^ ")")
  | Imply (f1,f2) -> "imply(" ^ to_string_vformula2 f1 ^ "," ^ to_string_vformula2 f2 ^ ")"
  | SigmaEqual ((m,t),e) -> "sigeq(" ^ to_string_var (m,t) ^ "," ^ to_string_vexp2 e ^ ")"
  | NoOverFlow (x,t) -> "noflow(" ^ to_string_var (x,t) ^ ")"
  | UntrustSum (isum,map) -> "usum(" ^ to_string_var isum ^ "," ^ to_string_var map ^ ")"
  | UntrustSum2 (isum,structure,member) -> "usum2(" ^ to_string_var isum ^ "," ^ to_string_var structure ^ "," ^ to_string_var member ^ ")"
  | ForAll (vars,f) ->
    let quant = Vocab.string_of_list ~first:"[" ~sep:";" ~last:"]" to_string_var vars in
    "forall(" ^ quant ^ "," ^ to_string_vformula2 f ^ ")"
  | Label (_,f) -> raise NotImplemented

and to_string_vexp2 ve =
  match ve with
  | VInt n -> BatBig_int.to_string n
  | VVar v -> to_string_var v
  | Read (ve1,ve2) -> "read(" ^ to_string_vexp2 ve1 ^ "," ^ to_string_vexp2 ve2 ^ ")"
  | VCond f -> "cond(" ^ to_string_vformula2 f ^ ")"
  | Write _ | VBinOp _ | VUnOp _ | VCast _
  | Ite _ | Uninterp _ -> raise NotImplemented


(********************************)
(********************************)
(*** From strings to formulas ***)
(********************************)
(********************************)

(* eq(read(TU,owner),true) -> read(TU,owner),true *)
let remove_parens : string -> string
= fun str ->
  let _ = assert (BatString.ends_with str ")") in
  let idx = BatString.index str '(' in
  str |> BatString.lchop ~n:(idx+1) |> BatString.rchop ~n:1

let find_spliting_comma : string -> int
= fun str ->
  BatString.fold_lefti (fun (found,stack,idx) i c ->
    if found then (found,stack,idx)
    else if c = ')' && stack = 1 then (true,stack,i)
    else if c = '(' then (found,stack+1,idx)
    else if c = ')' then (found,stack-1,idx)
    else (found,stack,idx)
  ) (false,0,-1) str
  |> (fun (found,stack,idx) ->
      let _ = assert (found) in
      let _ = assert (BatString.get str (idx+1) = ',') in
      idx+1)

(* Eg1. read(TU,owner),true -> read(TU,owner) ~ true *)
(* Eg2. x,y -> x ~ y *)
let split : string -> string * string
= fun s ->
  if BatString.count_char s ',' = 1 then
    BatString.split s ~by:","
  else
    let _ = assert (BatString.count_char s ',' >= 2) in
    let mid = find_spliting_comma s in
    let s1,s2 = BatString.left s mid, BatString.right s ((BatString.length s - 1) - (mid+1) + 1) in
    (s1,s2)

let find_matching_var vars v =
  let found = BatSet.filter (fun v' -> peel_lab v = peel_lab v') vars in
  if BatSet.cardinal found = 1 then BatSet.choose found
  else raise VarMatchingFailed

let parse_typ : string -> typ
= fun str ->
  let trans_mapping2 : string -> typ
  = fun str ->
    let _ = assert (BatString.starts_with str "mapping2") in
    let (left, right) = BatString.split str " => " in (* "mapping2(key => val)" -> ( "mapping2(key", "val)" )*)
    let left' = BatString.lchop ~n:9 left in (* "mapping2(key" -> key *)
    let right' = BatString.rchop ~n:1 right in (* "val)" -> "val" *)
    let key = Translator.trans_str_to_typeName left' in
    let value = Translator.trans_str_to_typeName right' in
    Mapping2 (key, value)
  in
  if BatString.starts_with str "mapping2" then trans_mapping2 str
  else Translator.trans_str_to_typeName str

let parse_var vars str =
  let (x,typ) = BatString.split str ~by:":" in
  let typ = parse_typ typ in
  if List.mem x global_ghost_var_names
     || BatString.starts_with x "@" (* bound vars *)
    then (x,typ)
  else
    find_matching_var vars (x,typ)

let parse_bound_vars vars str =
  let _ = assert (BatString.starts_with str "[" && BatString.ends_with str "]") in
  let s = str |> BatString.lchop ~n:1 |> BatString.rchop ~n:1 in
  let lst = BatString.split_on_char ';' s in
  let _ = assert (List.length lst > 0) in
  List.map (parse_var vars) lst

let rec parse_vf : var BatSet.t -> string -> vformula
= fun vars str ->
  match str with
  | "true" -> VTrue
  | "false" -> VFalse
  | _ when BatString.starts_with str "lnot(" ->
    let s = remove_parens str in
    let f = parse_vf vars s in
    VNot f

  | _ when BatString.starts_with str "land(" ->
    let s = remove_parens str in
    let (s1,s2) = split s in
    let f1,f2 = parse_vf vars s1, parse_vf vars s2 in
    VAnd (f1,f2)

  | _ when BatString.starts_with str "geq(" ->
    let s = remove_parens str in
    let s1,s2 = split s in
    let e1,e2 = parse_ve vars s1, parse_ve vars s2 in
    VBinRel (VGeq,e1,e2)

  | _ when BatString.starts_with str "gt(" ->
    let s = remove_parens str in
    let s1,s2 = split s in
    let e1,e2 = parse_ve vars s1, parse_ve vars s2 in
    VBinRel (VGt,e1,e2)

  | _ when BatString.starts_with str "eq(" ->
    let s = remove_parens str in
    let s1,s2 = split s in
    let e1,e2 = parse_ve vars s1, parse_ve vars s2 in
    VBinRel (VEq,e1,e2)

  | _ when BatString.starts_with str "imply(" ->
    let s = remove_parens str in
    let s1,s2 = split s in
    let f1,f2 = parse_vf vars s1, parse_vf vars s2 in
    Imply (f1,f2)

  | _ when BatString.starts_with str "sigeq(" ->
    let s = remove_parens str in
    let s1,s2 = split s in
    let map,e = parse_var vars s1, parse_ve vars s2 in
    SigmaEqual (map,e)

  | _ when BatString.starts_with str "noflow(" ->
    let s = remove_parens str in
    let map = parse_var vars s in
    NoOverFlow map

  | _ when BatString.starts_with str "usum(" ->
    let s = remove_parens str in
    let s1,s2 = BatString.split s ~by:"," in
    let isum,map = parse_var vars s1, parse_var vars s2 in
    UntrustSum (isum,map)

  | _ when BatString.starts_with str "usum2(" ->
    let s = remove_parens str in
    let lst = BatString.split_on_char ',' s in
    let _ = assert (List.length lst = 3) in
    let s1,s2,s3 = BatList.at lst 0, BatList.at lst 1, BatList.at lst 2 in
    let isum,structure,member = parse_var vars s1, parse_var vars s2, parse_var vars s3 in
    UntrustSum2 (isum,structure,member)

  | _ when BatString.starts_with str "forall(" ->
    let s = remove_parens str in (* forall([i;j], f) -> [i;j], f *)
    let s1,s2 = BatString.split s ~by:"," in (* [i;j], f -> [i;j] ~ f *)
    let vars,f = parse_bound_vars vars s1, parse_vf vars s2 in
    ForAll (vars,f)

  | _ -> assert false

and parse_ve : var BatSet.t -> string -> vexp
= fun vars str ->
  match str with
  | _ when BatString.starts_with str "read(" ->
    let s = remove_parens str in
    let s1,s2 = BatString.split s ~by:"," in
    let e1,e2 = parse_ve vars s1, parse_ve vars s2 in
    Read (e1, e2)

  | _ when BatString.starts_with str "cond(" ->
    let s = remove_parens str in
    let f = parse_vf vars s in
    VCond f

  | _ when BatString.contains str ':' ->
    let var = parse_var vars str in
    VVar var

  | _ when not (BatString.contains str ':') ->
    let n = BatBig_int.of_string str in
    VInt n

  | _ -> assert false

let parse_spec : var BatSet.t -> Yojson.Basic.t list -> SpecMap.t -> SpecMap.t
= fun vars lst spec0 ->
  let bindings = SpecMap.bindings spec0 in
  List.fold_left2 (fun acc elem (k,_) ->
    let pre_str = elem |> Json.value_of "pre" |> Json.to_string in
    let post_str = elem |> Json.value_of "post" |> Json.to_string in
    let pre = parse_vf vars pre_str in
    let post = parse_vf vars post_str in
    SpecMap.add k (pre,post) acc
  ) SpecMap.empty lst bindings

let parse_inv : var BatSet.t -> Yojson.Basic.t list -> InvMap.t -> InvMap.t
= fun vars lst inv0 ->
  let bindings = InvMap.bindings inv0 in
  List.fold_left2 (fun acc elem (k,_) ->
    let f_str = elem |> Json.value_of "formula" |> Json.to_string in
    let f = parse_vf vars f_str in
    InvMap.add k f acc
  ) InvMap.empty lst bindings

let do_parse : var BatSet.t -> string -> InvMap.t * SpecMap.t -> InvMap.t * SpecMap.t
= fun vars file (inv0,spec0) ->
  let json = Yojson.Basic.from_file file in
  let inv_json = json |> Json.value_of "invmap" |> Json.to_list in
  let spec_json = json |> Json.value_of "specmap" |> Json.to_list in
  let inv = parse_inv vars inv_json inv0 in
  let spec = parse_spec vars spec_json spec0 in
  (* let _ = print_endline (InvMap.to_string inv) in
  let _ = print_endline (SpecMap.to_string spec) in
  let _ = assert false in *)
  (inv,spec)

let collect_comps global paths =
  PathSet.fold (fun (fk,nodes) acc ->
    let f = FuncMap.find fk global.fmap in
    let cfg = get_cfg f in
    let comps = Component.collect_bp global nodes cfg in
    Component.union comps acc
  ) paths Component.empty_comps

let collect_vars : Global.t -> PathSet.t -> var BatSet.t
= fun global paths ->
  let comps = collect_comps global paths in
  let vars1 = comps.mapvars in
  let vars2 = BatSet.union (BatSet.map fst comps.mapmems) (BatSet.map snd comps.mapmems) in
  let vars3 = ExpSet.fold (fun ve acc -> BatSet.union (free_ve ve) acc) comps.reads BatSet.empty in
  let vars4 = comps.ivars in
  let vars5 = comps.avars in
  let vars6 = comps.a_arrs in
  let vars7 = comps.a_maps in
  let vars8 = comps.bvars in
  BatSet.union vars1 (BatSet.union vars2
    (BatSet.union vars3 (BatSet.union vars4 (BatSet.union vars5 (BatSet.union vars6 (BatSet.union vars7 vars8))))))

let run : Global.t -> PathSet.t -> string -> InvMap.t * SpecMap.t -> bool * InvMap.t * SpecMap.t
= fun global paths file (inv0,spec0) ->
  let vars = collect_vars global paths in
  try
    let (inv,spec) = do_parse vars file (inv0,spec0) in
    (true,inv,spec)
  with
    | VarMatchingFailed -> (false, inv0, spec0)
    | Invalid_argument _ -> (false, inv0, spec0)
    | _ -> (false, inv0, spec0)
