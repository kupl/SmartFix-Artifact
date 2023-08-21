open Lang
open Vlang
open Query
open Vocab
open Path
open Options
open WorkspaceRepair
open FixTarget

module PatchComp = struct
  type t' =
    | InsertLine of int * stmt * bool           (* if bool is true, bound check *)
    | Move of (line * stmt) list * line * stmt  (* Move (lst,l,s)      : move statements at 'lst' forward to the statement at l *)
    | Replace of line * exp * exp               (* Replace (l,e,e')    : e with e' at line l *)
    | ChangeToCnstr of fkey                     (* ChangeToCnstr(fkey) : change a nomral function to cnstr *)
    | AddModifier of fkey * string * line       (* AddModifier(f,name) : add a modifier to a function *)
    | ElseRevert of exp * stmt * stmt * ifinfo
    (* | Delete of line * stmt *)               (* Delete (l,s)        : delete the statement at line l *)

  type t =
    | Atom of t'
    | AtomLst of t' list

  let length_of comp =
    match comp with
    | Atom (ChangeToCnstr _) -> 1.3
    | Atom _ -> 1.0
    | AtomLst lst ->
      let nrs = List.filter (fun a -> match a with AddModifier (_,"@nonReentrant",_) -> true | _ -> false) lst in
      min (1.0 +. (0.03 *. float_of_int (List.length nrs))) 1.9
      (* 1.0 +. (0.01 *. float_of_int (List.length nrs)) *)
      (* min (0.9 +. (0.1 *. float_of_int (List.length nrs))) 1.5 *)

  let rec to_string' : t' -> string
  = fun a ->
    match a with
    | InsertLine (l,s,_) -> "InsertLine (" ^ string_of_int l ^ ", " ^  to_string_stmt ~report:true s ^ ")"
    | Move (lst,l,s) ->
      let lines = List.map fst lst in
      let str = string_of_list string_of_int ~sep:", " lines in
      "Move (" ^ str ^ " -> " ^ string_of_int l ^ ")"
    | Replace (l,e,e') ->
      "Replace (" ^ string_of_int l ^ ", " ^
      to_string_exp ~report:true e ^ ", " ^ to_string_exp ~report:true e' ^ ")"
    | ChangeToCnstr fkey -> "Change " ^ to_string_fkey fkey ^ " to constructor"
    | AddModifier (fkey,s,l) -> "Add " ^ s ^ " to " ^ to_string_fkey fkey ^ " (line " ^ string_of_int l ^ ")"
    | ElseRevert (_,_,_,i) -> "ElseRevert (line " ^ string_of_int i.if_loc.finish_line ^ ")"

  let to_string : t -> string
  = fun comp ->
    match comp with
    | Atom a -> to_string' a
    | AtomLst lst -> string_of_list ~first:"AtomLst [\n" ~last:"\n]" ~sep:"\n" to_string' lst

  let to_string_comps comps =
    string_of_list ~first:"[\n" ~last:"\n]" ~sep:",\n" to_string comps

  let lines_of' : t' -> line BatSet.t
  = fun a ->
    match a with
    | InsertLine (l,_,_) -> BatSet.singleton l
    | Move (lst,l,_) -> BatSet.add l (BatSet.of_list (List.map fst lst))
    | Replace (l,_,_) -> BatSet.singleton l
    | ChangeToCnstr fkey -> BatSet.singleton (-1)
    | AddModifier (_,_,l) -> BatSet.singleton l
    | ElseRevert (_,_,_,ifinfo) -> BatSet.singleton ifinfo.if_loc.finish_line

  let lines_of : t -> line BatSet.t
  = fun comp ->
    match comp with
    | Atom a -> lines_of' a
    | AtomLst lst -> List.fold_left (fun acc a -> BatSet.union (lines_of' a) acc) BatSet.empty lst

  let tag_of' a =
    match a with
    | InsertLine _ -> 0
    | Move _ -> 1
    | Replace _ -> 2
    | ChangeToCnstr _ -> 3
    | AddModifier _ -> 4
    | ElseRevert _ -> 5

  let tag_of comp =
    match comp with
    | Atom _ -> 1
    | AtomLst _ -> 2
  
  let rec compare_lst cmp lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> 0
    | [], h::t -> -1
    | h::t, [] -> 1
    | h1::t1, h2::t2 ->
      if cmp h1 h2 = 0 then compare_lst cmp t1 t2
      else cmp h1 h2

  let compare' a1 a2 =
    match a1,a2 with
    | InsertLine (l1,s1,b1), InsertLine (l2,s2,b2) ->
      if Stdlib.compare l1 l2 = 0 then
        let cmp_s = Stdlib.compare (to_string_stmt ~report:true s1) (to_string_stmt ~report:true s2) in
        if cmp_s = 0 then Stdlib.compare b1 b2
        else cmp_s
      else Stdlib.compare l1 l2
    | Move (lst1,l1,s1), Move (lst2,l2,s2) ->
      if Stdlib.compare l1 l2 = 0 then
        if Stdlib.compare (to_string_stmt ~report:true s1) (to_string_stmt ~report:true s2) = 0 then
          let cmp =
            (fun (l1,s1) (l2,s2) ->
             if Stdlib.compare l1 l2 = 0 then Stdlib.compare (to_string_stmt ~report:true s1) (to_string_stmt ~report:true s2)
             else Stdlib.compare l1 l2) in
          compare_lst cmp lst1 lst2
        else Stdlib.compare (to_string_stmt ~report:true s1) (to_string_stmt ~report:true s2)
      else Stdlib.compare l1 l2
    | Replace (l1,e1,e1'), Replace (l2,e2,e2') ->
      if Stdlib.compare l1 l2 = 0 then
        (* function return vars in modifiers at different locations could be different. *)
        if Stdlib.compare (to_string_exp ~report:true e1) (to_string_exp ~report:true e2) = 0 then
          Stdlib.compare (to_string_exp ~report:true e1') (to_string_exp ~report:true e2')
        else Stdlib.compare (to_string_exp ~report:true e1) (to_string_exp ~report:true e2)
      else Stdlib.compare l1 l2
    | ChangeToCnstr f1, ChangeToCnstr f2 -> Stdlib.compare f1 f2
    | AddModifier (f1,m1,_), AddModifier (f2,m2,_) ->
      if Stdlib.compare f1 f2 = 0 then Stdlib.compare m1 m2
      else Stdlib.compare f1 f2
    | ElseRevert (_,_,_,i1), ElseRevert (_,_,_,i2) -> Stdlib.compare i1 i2
    (* | Delete (l1,s1), Delete (l2,s2) ->
      if Stdlib.compare l1 l2 = 0 then
        Stdlib.compare (to_string_stmt ~report:true s1) (to_string_stmt ~report:true s2)
      else Stdlib.compare l1 l2 *)
    | _ -> Stdlib.compare (tag_of' a1) (tag_of' a2)

  let compare comp1 comp2 =
    match comp1,comp2 with
    | Atom a1, Atom a2 -> compare' a1 a2
    | AtomLst lst1, AtomLst lst2 -> compare_lst compare' lst1 lst2
    | _ -> Stdlib.compare (tag_of comp1) (tag_of comp2)

  let is_move : t' -> bool
  = fun a ->
    match a with
    | Move _ -> true
    | _ -> false

  let is_nr : t' -> bool
  = fun a ->
    match a with
    | AddModifier (_,"@nonReentrant",_) -> true
    | _ -> false

  let contain_move : t -> bool
  = fun comp ->
    match comp with
    | Atom a -> is_move a
    | AtomLst lst -> List.exists is_move lst

  let contain_nr : t -> bool
  = fun comp ->
    match comp with
    | Atom a -> is_nr a
    | AtomLst lst -> List.exists is_nr lst
end

type patch_comp = PatchComp.t
type atomic_comp = PatchComp.t'

type t = patch
and patch = patch_comp list

let length = List.length

let length2 : patch -> float
= fun patch -> List.fold_left (fun acc c -> acc +. PatchComp.length_of c) 0.0 patch

let to_string : patch -> string
= fun patch -> string_of_list ~first:"[\n" ~last:"\n]" ~sep:",\n" PatchComp.to_string patch

let to_string_oneline : patch -> string
= fun patch -> string_of_list ~first:"" ~last:"" ~sep:", " PatchComp.to_string patch

let print patch = print_endline (to_string patch)

let lines_of : patch -> line BatSet.t
= fun patch ->
  List.fold_left (fun acc comp ->
    BatSet.union (PatchComp.lines_of comp) acc
  ) BatSet.empty patch

let contain_nr : patch -> bool
= fun patch -> List.exists PatchComp.contain_nr patch

(*********************************************)
(*** Functions for making compilable patch ***)
(*********************************************)

(* special characters: "$^\.*+?[]." *)

let rspace = "\\( \\)*"

let regex_money n (eth_unit, unit_value) =
  let n' = (float_of_string (BatBig_int.to_string n)) /. unit_value in
  if n' < 1. then
    if BatString.starts_with (string_of_float n') "1e-" then
      let zero_num = snd (BatString.split ~by:"1e-" (string_of_float n')) |> int_of_string in
      "0\\." ^ BatString.repeat "0" (zero_num - 1) ^ "1" ^ rspace ^ eth_unit
    else string_of_float n' ^ rspace ^ eth_unit
  else BatBig_int.to_string (BatBig_int.of_float n') ^ rspace ^ eth_unit

(* e.g., 100 => 1e2 *)
let enumber n =
  if BatBig_int.equal (BatBig_int.modulo n (BatBig_int.of_int 10)) BatBig_int.zero (* n mod 10 = 0 *)
     && BatString.starts_with (BatBig_int.to_string n) "1" then
    let zero_num = snd (BatString.split ~by:"1" (BatBig_int.to_string n)) |> BatString.length in
    ["1e" ^ string_of_int zero_num]
  else []

let regex_int n =
  (* try hexadecimal first, because trying decimal would result in changes somehow *)
  ["0x" ^ BatBig_int.to_string_in_hexa n; BatBig_int.to_string n;
   regex_money n ("wei", 1.); regex_money n ("szabo", 1e12);
   regex_money n ("finney", 1e15); regex_money n ("ether", 1e18)]
  @ (enumber n)
  @
  (if BatBig_int.equal (BatBig_int.modulo n (BatBig_int.of_int 604800)) BatBig_int.zero then
     [BatBig_int.to_string (BatBig_int.div n (BatBig_int.of_int 604800)) ^ rspace  ^ "weeks"]
   else if BatBig_int.equal (BatBig_int.modulo n (BatBig_int.of_int 86400)) BatBig_int.zero then
     [BatBig_int.to_string (BatBig_int.div n (BatBig_int.of_int 86400)) ^ rspace  ^ "days"]
   else [])

let rec regex_exp : exp -> string list
= fun exp ->
  match exp with
  | Int n -> regex_int n
  | Real n -> [string_of_float n]
  | Str s -> [s]
  | Lv lv -> regex_lv lv
  | Cast (typ,e) ->
    let lst1, lst2 = regex_typ typ, regex_exp e in
    let product = BatList.cartesian_product lst1 lst2 in
    List.map (fun (r1,r2) -> r1 ^ rspace ^ "(" ^ rspace ^ r2 ^ rspace ^ ")") product
    @ List.map (fun (r1,r2) -> "(" ^ rspace ^ r1 ^ rspace ^ ")" ^ rspace ^ "(" ^ rspace ^ r2 ^ rspace ^ ")") product
  | BinOp (bop,e1,e2,_) ->
    let lst1, lst2 = regex_exp e1, regex_exp e2 in
    List.map (fun (r1,r2) -> r1 ^ rspace ^ regex_bop bop ^ rspace ^ r2) (BatList.cartesian_product lst1 lst2)
    @ List.map (fun (r1,r2) -> "(" ^ rspace ^ r1 ^ rspace ^ regex_bop bop ^ rspace ^ r2 ^ rspace ^ ")") (BatList.cartesian_product lst1 lst2)
  | UnOp (uop,e,_) ->
    let lst = regex_exp e in
    List.map (fun r -> regex_uop uop ^ rspace ^ r) lst
    @ List.map (fun r -> regex_uop uop ^ rspace ^ "(" ^ rspace ^ r ^ rspace ^ ")") lst
    @ List.map (fun r -> "(" ^ rspace ^ regex_uop uop ^ rspace ^ r ^ rspace ^ ")") lst
    @ List.map (fun r -> "(" ^ rspace ^ regex_uop uop ^ rspace ^ "(" ^ rspace ^ r ^ rspace ^ ")" ^ rspace ^ ")") lst
  | True -> ["true"]
  | False -> ["false"]
  | ETypeName etyp -> regex_etyp etyp
  | IndexRangeAccess _ | TypeInfo _ -> raise NotImplemented
  | IncTemp (e,prefix,_) ->
    let lst = regex_exp e in
    if prefix then List.map (fun r -> "\\+\\+" ^ rspace ^ r) lst
    else List.map (fun r -> r ^ rspace ^ "\\+\\+") lst
  | DecTemp (e,prefix,_) ->
    let lst = regex_exp e in
    if prefix then List.map (fun r -> "--" ^ rspace ^ r) lst
    else List.map (fun r -> r ^ rspace ^ "--") lst
  | CondTemp (e1,e2,e3,_,_) -> raise NotImplemented
  | AssignTemp (lv,e,_) -> raise NotImplemented
  | CallTemp (e,args,ethop,gasop,_) ->
    let lst1 = regex_exp e in (* string list *)
    let lst2 = List.map (fun a -> regex_exp a) args |> BatList.n_cartesian_product in (* string list list, list of args *)
    let lst3 = match ethop with None -> [""] | Some e -> List.map (fun r -> ".value(" ^ rspace ^ r ^ rspace ^ ")") (regex_exp e) in (* string list *)
    let lst4 = match gasop with None -> [""] | Some e -> List.map (fun r -> ".gas(" ^ rspace ^ r ^ rspace ^ ")") (regex_exp e) in (* string list *)
    List.fold_left (fun acc1 e ->
      List.fold_left (fun acc2 args ->
        List.fold_left (fun acc3 eth ->
          List.fold_left (fun acc4 gas ->
            acc4 @
            [ 
              e ^ rspace ^ eth ^ rspace ^ gas ^ rspace ^ (string_of_list ~first:"(" ~last:")" ~sep:("," ^ rspace) Vocab.id args)
            ]
          ) acc3 lst4
        ) acc2 lst3
      ) acc1 lst2
    ) [] lst1

and regex_exp_opt eop =
  match eop with
  | Some e -> regex_exp e
  | None -> [""]

and regex_bop bop =
  match bop with
  | Add -> "\\+" | Sub -> "-" | Mul -> "\\*" | Div -> "/"
  | Mod -> "%"   | Exponent -> "\\*\\*" | GEq -> ">="  | Gt -> ">"
  | LEq -> "<="  | Lt -> "<" | LAnd -> "&&" | LOr -> "||"
  | Eq -> "=="   | NEq -> "!=" | ShiftL -> "<<" | ShiftR -> ">>"
  | BXor -> "\\^" | BAnd -> "&" | BOr -> "|"

and regex_uop uop =
  match uop with
  | Pos -> "\\+"
  | Neg -> "-"
  | LNot -> "!"
  | BNot -> "~"

and regex_lv : lv -> string list
= fun lv ->
  match lv with
  | Var (v,vinfo) -> (* may be a return variable of a function call *)
    (* let f = BatString.replace_chars (fun c -> match c with '[' -> "\\[" | ']' -> "\\]" | ' ' -> rspace | _ -> BatString.of_char c) in *)
    regex_vinfo_org v vinfo.org
  | MemberAccess (e,x,xinfo,_) ->
    let lst1, lst2 = regex_exp e, regex_vinfo_org x xinfo.org in
    List.map (fun (r1,r2) -> r1 ^ "\\." ^ r2) (BatList.cartesian_product lst1 lst2)
  | IndexAccess (e,None,_) -> List.map (fun r -> r ^ rspace ^ "\\[\\]") (regex_exp e)
  | IndexAccess (e1,Some e2,_) ->
    let lst1, lst2 = regex_exp e1, regex_exp e2 in
    List.map (fun (r1,r2) -> r1 ^ rspace ^ "\\[" ^ rspace ^ r2 ^ rspace ^ "\\]") (BatList.cartesian_product lst1 lst2)
  | Tuple (elst,t) -> raise NotImplemented
    (* if is_array t then
      string_of_list ~first:"\\[" ~last:"\\]" ~sep:", " to_string_exp_opt elst
    else string_of_list ~first:"(" ~last:")" ~sep:", " to_string_exp_opt elst *)

and regex_vinfo_org x org =
  match org with
  | None -> [x]
  | Some e -> regex_exp e

and regex_typ : typ -> string list
= fun typ ->
  match typ with
  | ConstInt | ConstReal -> assert false
  | ConstString -> ["string"]
  | EType etyp -> regex_etyp etyp
  | Mapping (etyp,typ) ->
    let lst1, lst2 = regex_etyp etyp, regex_typ typ in
    List.map (fun (r1,r2) ->
      "mapping" ^ rspace ^ "(" ^ rspace ^ r1 ^ rspace ^ "=>" ^ rspace ^ r2 ^ rspace ^ ")"
    ) (BatList.cartesian_product lst1 lst2)
  | Mapping2 _ -> assert false
  | Array (typ,None) -> raise NotImplemented
  | Array (typ,Some n) -> raise NotImplemented (* to_string_typ typ ^ "[" ^ string_of_int n ^ "]" *)
  | Struct lst -> [string_of_list ~first:"" ~last:"" ~sep:"." Vocab.id lst]
  | TupleType _ | FuncType _ -> assert false
  | Void -> assert false

and regex_etyp : elem_typ -> string list
= fun etyp ->
  match etyp with
  | Address -> ["address"]
  | AddressPayable -> ["payable"]
  | Contract id -> [id]
  | Enum id -> [id]
  | Bool -> ["bool"]
  | String -> ["string"]
  | UInt n ->
    if n = 256 then ["uint"; "uint" ^ string_of_int n]
    else ["uint" ^ string_of_int n]
  | SInt n ->
    if n = 256 then ["int"; "int" ^ string_of_int n]
    else ["int" ^ string_of_int n]
  | Bytes n -> raise NotImplemented (* ["bytes" ^ string_of_int n] *)
  | DBytes -> raise NotImplemented (* ["dbytes"] *)

let space_of : string -> string
= fun x ->
  if BatString.starts_with x " " || BatString.starts_with x "\t" then
    snd (BatString.fold_left (fun (found,acc) c ->
      if found then (found, acc)
      else
        match c with
        | '\t' | ' ' -> (found, acc ^ BatString.of_char c)
        | _ -> (true, acc)
    ) (false,"") x)
  else ""

let strip_space : string -> string
= fun x -> snd (BatString.replace ~str:x ~sub:(space_of x) ~by:"")

let inc_dec op x =
  if not (BatString.exists x "++" || BatString.exists x "--")
    then x
  else
    let _ = assert (op = "+" || op = "-") in
    let opop = if op = "+" then "\\+\\+" else "--" in
    let regex = Str.regexp ("\\([A-Za-z]\\|\\[\\|\\]\\|\\.\\|_\\)+[\t ]*" ^ opop) in
    try
      let _ = Str.search_forward regex x 0 in
      let y = Str.matched_string x |> (fun s -> fst (BatString.split s ~by:(op^op))) |> strip_space in
      let subst = y ^ " = " ^ y ^ " " ^ op ^ " 1" in
      Str.global_replace regex subst x
    with Not_found -> assert false

let inc x = inc_dec "+" x
let dec x = inc_dec "-" x

let normalize x =
  let x = try let (a,b) = BatString.split x ~by:"+=" in a ^ " = " ^ strip_space a ^ "+" ^ b with Not_found -> x in (* "x += a" => "x = x + a" *)
  let x = try let (a,b) = BatString.split x ~by:"-=" in a ^ " = " ^ strip_space a ^ "-" ^ b with Not_found -> x in (* "x -= a" => "x = x - a" *)
  let x = x |> inc |> dec in (* x++ => x = x+1 *)
  x

let rec replace : string -> string list -> string -> string
= fun x sub_regexps rep ->
  let x = normalize x in
  match sub_regexps with
  | [] -> assert false
  | h::t ->
    let res = Str.global_replace (Str.regexp h) rep x in
    (* let _ = print_endline ("x: " ^ x) in
    let _ = print_endline ("sub: " ^ h) in
    let _ = print_endline ("rep: " ^ rep) in
    let _ = print_endline ("res: " ^ res) in
    let _ = print_endline "" in *)
    if res = x then replace x t rep else res

let to_string_stmt_wrapper stmt =
  match stmt with
  | Assume (e,loc) -> "require" ^ "(" ^ to_string_exp ~report:true e ^ ")" ^ ";"
  | _ -> Lang.to_string_stmt ~report:true stmt

let should_use_safemath : int -> string list -> stmt -> bool
= fun loc lines stmt ->
  let str = BatList.at lines (loc-1) |> (fun s -> try fst (BatString.split s ~by:"//") with Not_found -> s) in
  let contain_forloop loc lines = BatString.exists str "for" in
  let is_plus_one_guard stmt =
    match stmt with
    | Assume (BinOp (GEq, BinOp (Add,_,Int n,_),_,_),_) when BatBig_int.equal n BatBig_int.one -> true
    | _ -> false in
  contain_forloop loc lines
  && is_plus_one_guard stmt

let cache = ref BatSet.empty

let insert_safeguard : stmt -> string -> string
= fun stmt x ->
  let extract_exp stmt =
    match stmt with
    | Assume ((BinOp (GEq, (BinOp (Add,_,Int n,_) as exp), _, _)), _) when BatBig_int.equal n BatBig_int.one -> exp
    | _ -> assert false in
  let exp_to_safeguard exp =
    match exp with
    | BinOp (Add,e1,e2,einfo) ->
      (match einfo.etyp with
       | EType (UInt n) ->
         let _ = cache := BatSet.add ("add_uint", string_of_int n) !cache in
         "add_uint" ^ string_of_int n ^ "(" ^ to_string_exp ~report:true e1 ^ "," ^ to_string_exp ~report:true e2 ^ ")"
       | _ -> assert false)
    | _ -> assert false in
  let exp = extract_exp stmt in
  let regexps = regex_exp exp in
  let rep = exp_to_safeguard exp in
  replace x regexps rep

let apply' : atomic_comp -> string list -> string list
= fun a lines ->
  match a with
  | InsertLine (loc,stmt,true) when should_use_safemath loc lines stmt ->
    BatList.modify_at (loc-1) (fun x ->
      insert_safeguard stmt x ^ " /* <FIX> Insert:BC */"
    ) lines

  | InsertLine (loc,stmt,true) ->
    BatList.modify_at (loc-1) (fun x ->
      space_of x ^ to_string_stmt_wrapper stmt ^ " /* <FIX> Insert:BC */" ^ "\n" ^ x
      (* space_of x ^ to_string_stmt_wrapper stmt ^ " /* <FIX> Insert */" ^ " " ^ strip_space x *)
    ) lines

  | InsertLine (loc, (Assign (lv,e,_) as stmt), bc)
    when BatString.starts_with (to_string_lv lv) "tmp__" ->
    BatList.modify_at (loc-1) (fun x ->
      space_of x ^ to_string_typ (get_type_lv lv) ^ " " ^ to_string_stmt_wrapper stmt
      ^ " /* <FIX> Insert */" ^ "\n" ^ x
    ) lines

  | InsertLine (loc, (Assume (BinOp (Eq,e1,e2,_), _) as stmt), _)
    when to_string_exp e1 = "smartfix_owner" && to_string_exp e2 = "msg.sender" ->
    let _ = cache := BatSet.add ("fresh_owner", "") !cache in
    BatList.modify_at (loc-1) (fun x ->
      space_of x ^ to_string_stmt_wrapper stmt ^ " /* <FIX> Insert */" ^ "\n" ^ x
    ) lines

  | InsertLine (loc,stmt,_) ->
    BatList.modify_at (loc-1) (fun x ->
      space_of x ^ to_string_stmt_wrapper stmt ^ " /* <FIX> Insert */" ^ "\n" ^ x
      (* space_of x ^ to_string_stmt_wrapper stmt ^ " /* <FIX> Insert */" ^ " " ^ strip_space x *)
    ) lines

  | Move (lst,loc,s) ->
    (* comment (remove) all lines that will be moved *)
    let lines' = List.fold_left (fun acc (l',s') ->
                   BatList.modify_at (l'-1) (fun x -> space_of x ^ "/* <FIX> Move: " ^ strip_space x ^ " */") acc
                 ) lines lst in
    (* move lst in front of the statement at loc *)
    BatList.modify_at (loc-1) (fun x ->
      let targets = List.map (fun (l,s) -> space_of x ^ to_string_stmt_wrapper s ^ " /* <FIX> Move */") lst in
      (* let str = string_of_list ~first:"" ~sep:"" ~last:"" Vocab.id targets in
      str ^ " " ^ strip_space x *)
      let str = string_of_list ~first:"" ~sep:"\n" ~last:"" Vocab.id targets in
      str ^ "\n" ^ x (* strip_space x *)
    ) lines'

  | Replace (loc,e1,e2) ->
    BatList.modify_at (loc-1) (fun x ->
      replace x (regex_exp e1) (to_string_exp ~report:true e2)
      ^ " /* <FIX> Replace: " ^ "\"" ^ to_string_exp ~report:true e1 ^ "\"" ^ " => " ^ "\"" ^ to_string_exp ~report:true e2 ^ "\"" ^ " */"
    ) lines

  | ChangeToCnstr (_,fname,_) ->
    BatList.fold_left (fun acc x ->
      acc @ [Str.global_replace (Str.regexp ("function" ^ "\\( \\)*" ^ fname)) " /* <FIX> Change to Cnstr: */ constructor" x]
    ) [] lines

  | AddModifier (f,m,loc) ->
    let sign = if m = "@nonReentrant" then "/* <FIX> Add Modifier:NR */" else "/* <FIX> Add Modifier */" in
    let _ = if m = "@nonReentrant" then cache := BatSet.add ("_nonReentrant", "") !cache in
    let m = if m = "@nonReentrant" then "_nonReentrant" else m in
    BatList.modify_at (loc-1) (fun x ->
      if BatString.exists x "returns" then
        snd (BatString.replace ~str:x ~sub:"returns" ~by:(m ^ " " ^ sign ^ " returns"))
      else
        let _ = assert (BatString.exists x ")") in
        (* add blank: internalnonReentrant (X) *)
        snd (BatString.replace ~str:x ~sub:")" ~by:(") " ^ m ^ " " ^ sign))
    ) lines

  | ElseRevert (_,_,_,ifinfo) ->
    BatList.modify_at (ifinfo.if_loc.finish_line -1) (fun x ->
      snd (BatString.replace ~str:x ~sub:"}" ~by:("} else {revert (); } /* <FIX> add revert */"))
    ) lines

let apply_patch_to_source : patch_comp -> string list -> string list
= fun comp lines ->
  match comp with
  | Atom a -> apply' a lines
  | AtomLst lst -> list_fold apply' lst lines

let apply_patch_to_source : patch -> string list -> string list
= fun patch lines -> list_fold apply_patch_to_source patch lines

let template_add =
"  function add_uint(uint a, uint b) internal pure returns (uint) {
    require (a + b >= a);
    uint c = a + b;
    return c;
  }"

let template_re =
"  bool public _locked = false;

  modifier _nonReentrant() {
    require(!_locked);
    _locked = true;
    _;
    _locked = false;
  }"

let template_owner =
"  address public smartfix_owner = msg.sender;"

let to_template () =
  BatSet.fold (fun (name,bit) acc ->
    acc ^ "\n" ^
    (match name with
     | "_nonReentrant" -> template_re
     | "fresh_owner" -> template_owner
     | _ -> BatString.nreplace ~str:template_add ~sub:"uint" ~by:("uint" ^ bit))
  ) !cache ""

let smartfix_lib () = "contract SmartFix {" ^ to_template () ^ "\n}"

let base_contracts : string list ref = ref []

(* let re = Str.regexp ("contract\\( \\)+" ^ cname ^ "\\([A-Za-z]\\|[0-9]\\| \\|,\\|_\\|is\\|\n\\)*{") in *)
(* note: above re fails for the below example. *)
(* "contract BB { } contract B is BB { }" *)
(* => "contract BB is SmartFix, SmartFix { } contract B is BB { }" *)

let specify_inheritance' cname acc =
  let re = Str.regexp (
             "\\(" ^ "contract\\( \\)+" ^ cname ^ "{" ^ "\\)" ^ "\\|" ^
             "\\(" ^ "contract\\( \\)+" ^ cname ^ "\\( \\|\n\\)+" ^ "\\([A-Za-z]\\|[0-9]\\| \\|,\\|_\\|is\\|\n\\|(\\|)\\|\"\\)*{" ^ "\\)"
           ) in
  let _ = try Str.search_forward re acc 0 with Not_found -> assert false in
  let s = Str.matched_string acc in
  if BatString.exists s " is" then
    let s' = snd (BatString.replace ~str:s ~sub:" is" ~by:" is SmartFix, ") in
    let by = snd (BatString.replace ~str:s' ~sub:"  " ~by:" ") in
    snd (BatString.replace ~str:acc ~sub:s ~by:by)
  else
    let s' = BatString.rchop ~n:1 s in (* without '{' *)
    let by = s' ^ " is SmartFix {" in
    let by = snd (BatString.replace ~str:by ~sub:"  " ~by:" ") in
    snd (BatString.replace ~str:acc ~sub:s ~by:by)

let specify_inheritance : string list -> string -> string
= fun base_names file -> list_fold specify_inheritance' base_names file

let make_source_code : Global.t -> patch -> string list -> string
= fun global patch lines ->
  apply_patch_to_source patch lines
  (* add newline at the end following standard: https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline *)
  |> string_of_list ~first:"" ~last:"\n" ~sep:"\n" Vocab.id
  |> (fun file ->

      if BatSet.is_empty !cache then file
      else
        let res = smartfix_lib () ^ "\n\n" ^ specify_inheritance global.base_names_wo_interface file in
        let _ = cache := BatSet.empty in
        res)
