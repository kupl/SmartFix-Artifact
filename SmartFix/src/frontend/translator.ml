open Lang
open Options
open Vocab

let end_of_lines = ref [-1]

(* store cumulative byte size at the end of each line *)
let record_end_of_lines lines =
  List.fold_left (fun (acc_eol,acc_lst) cur ->
    let eol = Bytes.length (Bytes.of_string cur) + 1 in
    let acc_eol' = eol + acc_eol in
    (acc_eol', acc_lst @ [acc_eol'])
  ) (0,[]) lines
  |> (fun (_,lst) -> end_of_lines := lst)

let line_indicator : int list -> int -> int
= fun lst byte ->
  List.fold_left (fun (set,line) cur ->
    if not set && byte < cur then (true,line)
    else if set then (set,line)
    else (set,line+1)
  ) (false,1) lst
  |> (fun (_,n) -> n)

let get_loc : Yojson.Basic.t -> loc
= fun json ->
  let lst = BatString.split_on_char ':' (json |> Json.value_of "src" |> Json.to_string) in
  let offset,len = int_of_string (BatList.at lst 0), int_of_string (BatList.at lst 1) in
  {
    line = line_indicator !end_of_lines offset;
    finish_line = line_indicator !end_of_lines (offset+ len);
    offset = offset;
    len = len
  }

let get_id : Yojson.Basic.t -> int
= fun json -> json |> Json.value_of "id" |> Json.to_int

(******************************)
(******************************)
(***** Type-related parts *****)
(******************************)
(******************************)

let elem_typs =
  ["address"; "address payable"; "contract"; "library"; "enum"; "bool"; "int"; "uint"; "byte"; "fixed"; "ufixed"; "bytes"; "string"]

(******************************)
(*** typeIdentifier to type ***)
(******************************)

let rec trans_typeIdentifier : string -> string -> typ
= fun ts_all str ->
  let preprocess str =
    str
    |> (fun res -> if BatString.starts_with res "_" then BatString.lchop ~n:1 res else res)
    |> (fun res -> if BatString.ends_with res "_" then BatString.rchop ~n:1 res else res)
  in
  let get_tups lst =
    if List.length lst = 1 && List.hd lst = "__" then []
    else lst |> List.map preprocess |> List.map (trans_typeIdentifier ts_all)
  in
  match str with
  | _ when BatString.starts_with str "t_stringliteral" -> ConstString

  | "t_address_payable" -> EType AddressPayable
  | "t_address" -> EType Address
  | _ when BatString.starts_with str "t_uint" -> EType (UInt (int_of_string (BatString.tail str 6)))
  | _ when BatString.starts_with str "t_int" -> EType (SInt (int_of_string (BatString.tail str 5)))
  | "t_bool" -> EType Bool
  | _ when BatString.starts_with str "t_bytes_" -> EType DBytes
  | _ when BatString.starts_with str "t_bytes" ->
    EType (Bytes (int_of_string (BatString.lchop ~n:7 str)))
  | _ when BatString.starts_with str "t_contract" ->
    let lst = BatString.split_on_char '$' str in
    let cname = BatString.chop ~l:1 ~r:1 (BatList.at lst 1) in (* _ContractName_ => ContractName *)
    EType (Contract cname)
  | _ when BatString.starts_with str "t_string_" -> EType String

  | _ when BatString.starts_with str "t_tuple" ->
    let str = (assert (BatString.ends_with str "$"); BatString.rchop ~n:1 str) in
    let lst = BatString.split_on_char '$' str |> List.tl |> get_tups in
    TupleType lst

  | _ when BatString.starts_with str "t_function" ->
    let str = (* remove "bound_to ... " - unnecessary to identify return typ *)
      if BatString.exists str "$bound_to" then BatString.left str ((BatString.find str "$bound_to") + 1)
      else str in
    let str =
      let str = snd (BatString.replace ~str:str ~sub:"$value_$" ~by:"$") in
      let str = snd (BatString.replace ~str:str ~sub:"$value$" ~by:"$") in
      snd (BatString.replace ~str:str ~sub:"$value" ~by:"$") in
    let str = (assert (BatString.ends_with str "$"); BatString.rchop ~n:1 str) in
    let lst = BatString.split_on_char '$' str in
    let ret_idx =
      if BatString.starts_with str "t_function_setvalue_nonpayable$" then
        (match BatList.index_of "returns" lst with Some i -> i | None -> assert false)
      else
        (match BatList.rindex_of "returns" lst with Some i -> i | None -> assert false) in
    (* ["t_function_..."; "_t_..._"; "returns"; "__"] => (["t_function_..."; "_t_..._"], ["returns"; "__"]) *)
    let (ilst,olst) = BatList.split_at ret_idx lst in
    let (ilst,olst) = (List.tl ilst, List.tl olst) in (* ["_t_..._"], ["__"] *)

    let rec recover lst =
      match lst with
      | [] -> []
      | h::[] -> [h]
      | "_t_array"::tl ->
        let rec find_arr_size_idx depth idx lst =
          match lst with
          | [] -> assert false
          | h::tl when not (BatString.starts_with h "_") ->
            if depth = 1 then idx (* solution found *)
            else
              let _ = assert (depth > 1) in
              find_arr_size_idx (depth-1) (idx+1) tl
          | "_t_array"::tl -> find_arr_size_idx (depth+1) (idx+1) tl
          | "_t_struct"::tl -> find_arr_size_idx (depth+1) (idx+1) tl
          | h::tl -> find_arr_size_idx depth (idx+1) tl
        in
        let size_idx = find_arr_size_idx 0 0 lst in
        let arr,tl' = BatList.split_at (size_idx + 1) lst in (* find 'arr' including the size part *)
        let h' = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id arr in
        h'::(recover tl')
      | "_t_contract"::cname::cid::tl ->
        let h' = "_t_contract" ^ "$" ^ cname ^ "$" ^ cid in
        h'::(recover tl)
      | "_t_struct"::sname::sid::tl ->
        let h' = "_t_struct" ^ "$" ^ sname ^ "$" ^ sid in
        h'::(recover tl)
      | "_t_enum"::ename::eid::tl ->
        let h' = "_t_enum" ^ "$" ^ ename ^ "$" ^ eid in
        h'::(recover tl)
      | "_t_function_barecall_payable"::"_t_bytes_memory_ptr_"::"returns"::"_t_bool_"::"_t_bytes_memory_ptr_"::tl ->
        let h' = "_t_function_barecall_payable$_t_bytes_memory_ptr_$returns$_t_bool_$_t_bytes_memory_ptr_$" in
        h'::(recover tl)
      | "_t_function_barecall_payable"::"__"::"returns"::"_t_bool_"::tl -> (* solc < 0.5.0 *)
        let h' = "_t_function_barecall_payable$__$returns$_t_bool_$" in
        h'::(recover tl)
      (* _t_function_external_payable$_t_uint256_$_t_string_memory_ptr_$_t_string_memory_ptr_$returns$_t_bytes32_$value_$ *)
      | "_t_function_external_payable"::tl ->
        let rec find_end_idx idx lst =
          match lst with
          | [] -> idx (* 'value_' has been removed by the front routine *)
          (* | "value_"::tl -> idx *)
          | h::tl -> find_end_idx (idx+1) tl
        in
        let end_idx = find_end_idx 0 lst in
        let (func,tl') = BatList.split_at end_idx lst in (* including 'end_idx' *)
        let h' = Vocab.string_of_list ~first:"" ~last:"$" ~sep:"$" Vocab.id func in
        h'::(recover tl')
      | "_t_function_internal_view"::tl ->
        let rec find_end_idx idx lst =
          match lst with
          | [] -> assert false
          | "_"::tl -> idx
          | h::tl -> find_end_idx (idx+1) tl
        in
        let end_idx = find_end_idx 0 lst in
        let (func,tl') = BatList.split_at end_idx lst in
        let h' = Vocab.string_of_list ~first:"" ~last:"$" ~sep:"$" Vocab.id func in
        h'::(recover tl')
      | h::tl -> h::(recover tl)
    in

    FuncType (get_tups (recover ilst), get_tups (recover olst))

  | _ when BatString.starts_with str "t_mapping" ->
    let str = (assert (BatString.ends_with str "$"); BatString.rchop ~n:1 str) in
    let lst = BatString.split_on_char '$' str in
    let _ = assert (List.length lst = 3) in
    let k,v = BatList.at lst 1, BatList.at lst 2 in
    let _ = assert (BatString.starts_with k "_" && BatString.ends_with k "_") in
    let _ = assert (BatString.starts_with v "_" && BatString.ends_with v "_") in
    let k,v = BatString.chop ~l:1 ~r:1 k, BatString.chop ~l:1 ~r:1 v in
    let k = match trans_typeIdentifier ts_all k with EType t -> t | _ -> assert false in
    Mapping (k, trans_typeIdentifier ts_all v)

  (* e.g: "t_array$_t_array$_t_uint256_$3_memory_$5_memory_ptr" => uint256[3][5] *)
  | _ when BatString.starts_with str "t_array" ->
    let lst = BatString.split_on_char '$' str in (* e.g: t_array$_t_uint256_$dyn_memory_ptr *)
    let typ_part_lst = lst |> List.tl |> (fun res -> BatList.remove_at (List.length res - 1) res) in
    let typ_str = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id typ_part_lst in
    let _ = assert (BatString.starts_with typ_str "_" && BatString.ends_with typ_str "_") in
    let typ_str = BatString.chop ~l:1 ~r:1 typ_str in
    let size_str = BatList.last lst in
    let typ = trans_typeIdentifier ts_all typ_str in
    let size =
      match size_str with
      | _ when BatString.starts_with size_str "dyn_" -> None
      | _ -> Some (BatString.split_on_char '_' size_str |> (fun res -> BatList.at res 0) |> int_of_string)
    in
    Array (typ,size)

  | _ when BatString.starts_with str "t_struct" -> (* $_t_struct$_TokenOwnership_$1418_memory_ptr_$ *)
    let lst = BatString.split_on_char '$' str in
    let sname = BatList.at lst 1 in
    let _ = assert (BatString.starts_with sname "_" && BatString.ends_with sname "_") in
    let sname = BatString.chop ~l:1 ~r:1 sname in (* TokenOwnership *)
    let ts_all_lst =
      let ts_all = BatString.replace_chars (fun c -> match c with ',' | '(' | ')' -> " " | _ -> BatString.of_char c) ts_all in
      let ts_all = BatString.nreplace ~str:ts_all ~sub:"  " ~by:" " in
      BatString.split_on_char ' ' ts_all in
    let sname = ts_all_lst |> BatList.find_all (fun s -> BatString.ends_with s sname) |> BatSet.of_list in
    let sname = (assert (BatSet.cardinal sname = 1); BatSet.choose sname) in
    if not (BatString.exists sname ".") then Struct [sname] (* global struct *)
    else
      let (cname, sname) = BatString.split sname "." in
      Struct [cname; sname]

  | _ when BatString.starts_with str "t_enum" -> (* "t_enum$_RecoverError_$875" *)
    let lst = BatString.split_on_char '$' str in
    let ename = BatList.at lst 1 in
    let _ = assert (BatString.starts_with ename "_" && BatString.ends_with ename "_") in
    let ename = BatString.chop ~l:1 ~r:1 ename in (* RecoverError *)
    let ts_all_lst =
      (* To handle "function (uint256,uint256) pure returns (enum CarefulMath.MathError,uint256)" *)
      let ts_all = BatString.replace_chars (fun c -> match c with ',' | '(' | ')' -> " " | _ -> BatString.of_char c) ts_all in
      let ts_all = BatString.nreplace ~str:ts_all ~sub:"  " ~by:" " in
      BatString.split_on_char ' ' ts_all in
    let ename = ts_all_lst |> BatList.find_all (fun s -> BatString.ends_with s ename) |> BatSet.of_list in
    let ename = (assert (BatSet.cardinal ename = 1); BatSet.choose ename) in
    if not (BatString.exists ename ".") then EType (Enum ename)
    else
      let (_, ename) = BatString.split ename "." in
      EType (Enum ename)

  | _ when BatString.starts_with str "t_type$_t_super" -> Void (* 'super' keyword *)
  | _ when BatString.starts_with str "t_type" -> (* type conversion case *)
    let lst = BatString.split_on_char '$' str in (* e.g: "t_type$_t_string_storage_ptr_$", "t_type$_t_contract$_IERC721_$125_$" *)
    let arg_part_lst = lst |> List.tl in (* ["_t_contract"; "_IERC721_"; "125_"] *)
    let arg_str = Vocab.string_of_list ~first:"" ~last:"" ~sep:"$" Vocab.id arg_part_lst in (* _t_contract$_IERC721_$125_ *)
    let _ = assert (BatString.starts_with arg_str "_" && BatString.ends_with arg_str "_") in
    let arg_str = BatString.chop ~l:1 ~r:1 arg_str in (* t_contract$_IERC721_$125 *)
    trans_typeIdentifier ts_all arg_str

  | "t_magic_message" -> Void (* msg *)
  | _ ->
    let _ = print_endline ("[WARNING] type conversion : " ^ str) in
    Void

(**********************)
(*** string to type ***)
(**********************)

let rec trans_typeString : string -> typ
= fun str ->
  let str = if BatString.exists str "type(" then BatString.chop ~l:5 ~r:1 str else str in
  let str = BatString.nreplace ~str:str ~sub:" storage " ~by:" " in
  let str = BatString.nreplace ~str:str ~sub:" ref" ~by:"" in
  let str = BatString.nreplace ~str:str ~sub:" memory" ~by:"" in
  let str = BatString.nreplace ~str:str ~sub:" calldata" ~by:"" in
  let str = BatString.nreplace ~str:str ~sub:" super " ~by:"" in
  match str with
  | _ when BatString.ends_with (BatString.nreplace ~str:str ~sub:" pointer" ~by:"") "]" -> trans_array str
  | _ when BatString.starts_with str "int_const" -> ConstInt
  | _ when BatString.starts_with str "rational_const" -> ConstReal
  | _ when BatString.starts_with str "literal_string" -> ConstString
  | _ when BatString.starts_with str "tuple" -> trans_tuple str
  | _ when BatString.starts_with str "struct" -> trans_struct_type str
  | _ when BatString.starts_with str "mapping" -> trans_mapping str
  | _ when List.exists (fun e -> BatString.starts_with str e) elem_typs ->
    EType (trans_elementaryTypeName str)
  | _ -> Void

and trans_elementaryTypeName : string -> elem_typ
= fun str ->
  match str with
  | _ when BatString.starts_with str "contract" ->
    let (_, str) = BatString.replace str "contract " "" in
    Contract str
  | _ when BatString.starts_with str "library" ->
    let (_, str) = BatString.replace str "library " "" in
    Contract str
  | _ when BatString.starts_with str "enum" ->
   (match trans_enum_type str with
    | EType (Enum ename) -> Enum ename
    | _ -> assert false)
  | _ when BatString.exists str "string" -> String
  | "address" -> Address
  | "address payable" -> AddressPayable
  | "bool" -> Bool
  | s when BatString.starts_with s "uint" ->
     let n_str = BatString.tail s 4 in
     if BatString.equal n_str "" then UInt 256
     else UInt (int_of_string n_str)
  | s when BatString.starts_with s "int" ->
     let n_str = BatString.tail s 3 in
     if BatString.equal n_str "" then SInt 256
     else SInt (int_of_string n_str)
  | "byte" -> Bytes 1
  | s when BatString.starts_with s "bytes" ->
     let n_str = BatString.tail s 5 in
     if BatString.equal n_str "" || BatString.exists s " " then DBytes
     else Bytes (int_of_string n_str)
  (* currently, (u)fixed point numbers are unstable in Solidity. *)
  | "fixed" -> failwith "Unsupported: fixed"
  | "ufixed" -> failwith "Unsupported: ufixed"
  | s -> failwith ("Unsupported: trans_elementraryTypeName - " ^ s)

and trans_struct_type : string -> typ
= fun str ->
  let _ = assert (BatString.starts_with str "struct") in
  let (_, str') = BatString.replace str " pointer" "" in
  let (_, final) = BatString.replace str' "struct " "" in (* struct Name => Name *)
  if not (BatString.exists final ".") then Struct [final] (* global struct *)
  else
    let (cname,sname) = BatString.split final "." in
    Struct [cname;sname]

and trans_enum_type : string -> typ
= fun str ->
  let _ = assert (BatString.starts_with str "enum") in
  let (_, str') = BatString.replace str " pointer" "" in
  let (_, final) = BatString.replace str' "enum " "" in (* struct tmp.tmp1 => tmp.tmp1 *)
  if not (BatString.exists final ".") then EType (Enum final)
  else
    let (_,ename) = BatString.split final "." in
    EType (Enum ename)

and trans_mapping : string -> typ
= fun str ->
  let _ = assert (BatString.exists str "mapping") in
  if BatString.ends_with str "]" then trans_array str
  else
    let (left, right) = BatString.split str " => " in (* "mapping(key => val)" -> ( "mapping(key", "val)" )*)
    let left' = BatString.lchop ~n:8 left in (* "mapping(key" -> key *)
    let right' = BatString.rchop ~n:1 right in (* "val)" -> "val" *)
    let key = trans_elementaryTypeName left' in
    let value = trans_typeString right' in
    Mapping (key, value)

and trans_array : string -> typ
= fun str ->
  let str = BatString.strip str in
  let (left, right) = BatString.rsplit str "[" in (* type[25][30] => (type[25], 30] *)
  let (size, _) = BatString.split right "]" in (* 30] => (30, _) *)
  let t = trans_typeString left in
  let arr_size = try Some (int_of_string size) with _ -> None in
  Array (t, arr_size)

and trans_tuple : string -> typ
= fun str ->
  let str = BatString.chop ~l:6 ~r:1 str in (* tuple(uint,,string,) => uint,,string, *)
  if str = "" then TupleType []
  else
    let strs = BatString.split_on_char ',' str in (* uint,,string, => [uint,"",string,""] *)
    TupleType (List.map trans_typeString strs)

let trans_typeDescriptions : Yojson.Basic.t -> typ
= fun json ->
  let ts = json |> Json.value_of "typeDescriptions" |> Json.value_of "typeString" |> Json.to_string in
  (if not (BatString.starts_with ts "function") then trans_typeString ts
  else
    json
    |> Json.value_of "typeDescriptions"
    |> Json.value_of "typeIdentifier"
    |> Json.to_string
    |> trans_typeIdentifier ts)

let trans_str_to_typeName = trans_typeString

(* nodeType: X *)
let rec trans_expression : Yojson.Basic.t -> exp
= fun json ->
  let typ = trans_typeDescriptions json in
  let node_typ = Json.value_of "nodeType" json in
  let loc = get_loc json in
  let nid = get_id json in
  match node_typ with
  | `String "BinaryOperation" ->
     let left = json |> Json.value_of "leftExpression" |> trans_expression in
     let right = json |> Json.value_of "rightExpression" |> trans_expression in
     (match Json.value_of "operator" json with
      | `String "+" -> BinOp (Add, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "-" -> BinOp (Sub, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "*" ->
         (match left,right with
          | Int n1, Real n2 -> Int (BatBig_int.of_float (BatBig_int.float_of_big_int n1 *. n2))
          | Real n1, Int n2 -> Int (BatBig_int.of_float (n1 *. BatBig_int.float_of_big_int n2))
          | Real n1, BinOp (Exponent, Int n2, Int n3, _) -> Int (BatBig_int.of_float (n1 *. (BatBig_int.float_of_big_int (BatBig_int.pow n2 n3))))
          | _ -> BinOp (Mul, left, right, {eloc=loc; etyp=typ; eid=nid}))
      | `String "/" -> BinOp (Div, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "%" -> BinOp (Mod, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "**" -> BinOp (Exponent, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String ">=" -> BinOp (GEq, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String ">" -> BinOp (Gt, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "<=" -> BinOp (LEq, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "<" -> BinOp (Lt, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "&&" -> BinOp (LAnd, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "||" -> BinOp (LOr, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "==" -> BinOp (Eq, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "!=" -> BinOp (NEq, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "<<" -> BinOp (ShiftL, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String ">>" -> BinOp (ShiftR, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "^" -> BinOp (BXor, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "&" -> BinOp (BAnd, left, right, {eloc=loc; etyp=typ; eid=nid})
      | `String "|" -> BinOp (BOr, left, right, {eloc=loc; etyp=typ; eid=nid})
      | _ -> raise (Failure "Unsupported: trans_expression1"))
  | `String "Identifier" ->
     (try
       let vname = json |> Json.value_of "name" |> Json.to_string in
       let typ = trans_typeDescriptions json in
       let vinfo = (* the information is not exact at the moment, but will be updated in the preprocessing. *)
         { vloc = loc;
           is_gvar = false;
           vtyp = typ;
           vvis = Private; 
           vid = (try json |> Json.value_of "id" |> Json.to_int with _ -> assert false);
           refid = (try json |> Json.value_of "referencedDeclaration" |> Json.to_int with _ -> assert false);
           vscope = -1;
           storage = "";
           flag = false; (* should be marked as false *)
           org = Some (Lv (Var (vname, mk_vinfo ~typ:typ ())))
         } in
       Lv (Var (vname,vinfo))
     with _ -> assert false)
  | `String "MemberAccess" ->
     (try
       let exp = json |> Json.value_of "expression" |> trans_expression in
       let id = json |> Json.value_of "memberName" |> Json.to_string in
       let typ = trans_typeDescriptions json in
       let id_info = 
         {dummy_vinfo with 
            refid = (try json |> Json.value_of "referencedDeclaration" |> Json.to_int with _ -> -1);
            vtyp = typ;
            org = Some (Lv (Var (id, mk_vinfo ~typ:typ ())))
         }
       in
       (match exp with
        | CallTemp (Lv (Var (fname,_)),args,_,_,einfo)
          when fname = "type" && List.length args = 1 ->
          let arg = List.hd args in
          let einfo = {eloc=loc; etyp=typ; eid=nid} in
          (match arg with
           | Lv (Var (x,xinfo)) when is_contract (get_type_exp arg) -> TypeInfo (EType (Contract x), id, einfo)
           | ETypeName etyp -> TypeInfo (EType etyp, id, einfo)
           | _ -> raise NotImplemented)
        | _ -> Lv (MemberAccess (exp,id,id_info,typ)))
      with _ -> assert false)
  | `String "IndexAccess" ->
     let base = json |> Json.value_of "baseExpression" |> trans_expression in
     let idx = try json |> Json.value_of "indexExpression" |> trans_expression 
               with _ -> raise (Failure "indexExpression may be null: trans_expression") in
     Lv (IndexAccess (base,Some idx,typ))
  | `String "Literal" ->
     (match Json.value_of "kind" json with
      | `String "number" ->
         let factor =
           (match Json.value_of "subdenomination" json with
            | `Null -> 1.
            | `String "wei" -> 1. 
            | `String "szabo" -> 1e12
            | `String "finney" -> 1e15
            | `String "ether" -> 1e18
            | `String "seconds" -> 1.
            | `String "minutes" -> 60.
            | `String "hours" -> 3600.
            | `String "days" -> 86400. (* 24*3600 *)
            | `String "weeks" -> 604800. (* 7*24*3600 *)
            | `String "years" -> 31536000. (* 365 * 86400 *)
            | `String "gwei" -> 1e9
            | _ -> assert false)
         in
         let str = json |> Json.value_of "value" |> Json.to_string in 
         (match typ with
            (* float_of_string "0xffffffffffffffffffffffff0000000000000000000000000000000000000000" loses precision *)
            (* Thus, directly convert into BatBig_int *)
          | ConstInt ->
            (match Json.value_of "subdenomination" json with
             | `Null when not (BatString.contains str 'e') ->
                Int (BatBig_int.of_string str)
             | _ ->
               let value = float_of_string str in
               Int (BatBig_int.of_float (value *. factor))) (* e.g., 0.00001 ether *)
          | EType Address | EType AddressPayable -> (* in solv 0.6.12, constant address seems to become 'payable' type *)
            let value = try BatBig_int.of_string str with _ -> BatBig_int.of_float (float_of_string str) in
            Cast (typ, Int (BatBig_int.mul value (BatBig_int.of_float factor)))
          | ConstReal ->
            let value = float_of_string str in
            Real (value *. factor)
          | _ -> assert false)
      | `String "bool" ->
         let b = json |> Json.value_of "value" in
         (match b with
          | `String "true" -> True
          | `String "false" -> False
          | _ -> assert false)
      | `String "string" ->
         let s = try json |> Json.value_of "value" |> Json.to_string with _ -> json |> Json.value_of "hexValue" |> Json.to_string in
         Str s
      | `String "hexString" ->
         let s = json |> Json.value_of "hexValue" |> Json.to_string in
         Str s
      | `String "unicodeString" -> Str (json |> Json.value_of "value" |> Json.to_string)
      | `String s -> raise (Failure ("Unsupported: trans_expression2 - " ^ s ^ " : line " ^ string_of_int loc.line))
      | _ -> assert false)
  | `String "FunctionCall" ->
     (match Json.value_of "kind" json with
      | `String "functionCall" when json |> Json.value_of "expression" |> Json.value_of "nodeType" = `String "FunctionCallOptions" ->
         let json' = json |> Json.value_of "expression" in
         let _ = assert (Json.value_of "nodeType" json' = `String "FunctionCallOptions") in
         let args = json |> Json.value_of "arguments" |> trans_functionCallArguments in (* should be be json, not json' *)
         let exp = json' |> Json.value_of "expression" |> trans_expression in (* for the rest, should be json', not json *)
         let opnames = json' |> Json.value_of "names" |> Json.to_list |> List.map Json.to_string in
         let ops = json' |> Json.value_of "options" |> Json.to_list |> List.map trans_expression in
         let _ = assert (List.length opnames = List.length ops) in
         let _ = assert (List.length opnames <=2 && List.length ops <=2) in
         let ethop = match BatList.index_of "value" opnames with Some n -> Some (BatList.at ops n) | None -> None in
         let gasop = match BatList.index_of "gas" opnames with Some n -> Some (BatList.at ops n) | None -> None in
         CallTemp (exp, args, ethop, gasop, {eloc=loc; etyp=typ; eid=nid})
      | `String "functionCall" ->
         let exp = json |> Json.value_of "expression" |> trans_expression in
         let args = json |> Json.value_of "arguments" |> trans_functionCallArguments in
         if not (to_string_exp exp = "contract_init") then
           CallTemp (exp,args,None,None,{eloc=loc; etyp=typ; eid=nid})
         else
           let cname = Lv (Var (get_name_userdef typ, mk_vinfo ~typ:typ ())) in
           CallTemp (exp,cname::args,None,None,{eloc=loc; etyp=typ; eid=nid})
      | `String "typeConversion" ->
         let arg = json |> Json.value_of "arguments" |> Json.to_list in
         let _ = assert (List.length arg = 1) in
         let exp = trans_expression (List.hd arg) in
         Cast (typ, exp)
      | `String "structConstructorCall" ->
         let exp = json |> Json.value_of "expression" |> trans_expression in
         let args = json |> Json.value_of "arguments" |> trans_functionCallArguments in
         let names = json |> Json.value_of "names" |> Json.to_list in
         if List.length names = 0 then (* member names are not given *)
           CallTemp (Lv (Var ("struct_init",dummy_vinfo)), exp::args, None, None, {eloc=loc; etyp=typ; eid=nid})
         else
           let args_names = List.map (fun name -> Lv (Var (Json.to_string name, dummy_vinfo))) names in
           CallTemp (Lv (Var ("struct_init2",dummy_vinfo)), exp::args_names@args, None, None, {eloc=loc; etyp=typ; eid=nid})
      | `String s -> raise (Failure ("Unsupported: trans_expression3-"^s))
      | _ -> assert false)
  | `String "UnaryOperation" ->
     let sub = json |> Json.value_of "subExpression" |> trans_expression in
     (match Json.value_of "operator" json with
      | `String "+" -> UnOp (Pos,sub,typ)
      | `String "-" -> UnOp (Neg,sub,typ) 
      | `String "!" -> UnOp (LNot,sub,typ)
      | `String "~" -> UnOp (BNot,sub,typ)
      | `String "++" ->
         let pre = json |> Json.value_of "prefix" |> Json.to_bool in
         IncTemp (sub,pre,loc)
      | `String "--" ->
         let pre = json |> Json.value_of "prefix" |> Json.to_bool in
         DecTemp (sub,pre,loc)
      | _ -> raise (Failure "Unsupported: trans_expression4"))
  | `String "TupleExpression" ->
     let tuples = json |> Json.value_of "components" |> Json.to_list in
     if is_array typ then Lv (Tuple ((List.map (fun e -> try Some (trans_expression e) with _ -> None) tuples), typ)) else
     if List.length tuples = 1 then trans_expression (List.hd tuples)
     else Lv (Tuple ((List.map (fun e -> try Some (trans_expression e) with _ -> None) tuples), typ))
  | `String "Conditional" -> (* cond ? t : f *)
     let cond = json |> Json.value_of "condition" |> trans_expression in
     let t = json |> Json.value_of "trueExpression" |> trans_expression in
     let f = json |> Json.value_of "falseExpression" |> trans_expression in
     CondTemp (cond,t,f,typ,loc)
  | `String "NewExpression" ->
     let ret_typ = match typ with FuncType (_,rt) -> (assert (List.length rt = 1); List.hd rt) | _ -> assert false in
     if is_array ret_typ then Lv (Var ("array_init", mk_vinfo ~typ:typ ())) else
     if is_contract ret_typ then Lv (Var ("contract_init", mk_vinfo ~typ:typ ())) else
     if is_struct ret_typ then Lv (Var ("struct_init", mk_vinfo ~typ:typ ())) else
     if is_enum ret_typ then Lv (Var ("enum_init", mk_vinfo ~typ:typ ())) else
     if is_dbytes ret_typ then Lv (Var ("dbytes_init", mk_vinfo ~typ:typ ())) else
     if is_string ret_typ then Lv (Var ("string_init", mk_vinfo ~typ:typ ()))
     else failwith ("NewExpression : " ^ to_string_typ ret_typ)
  | `String "Assignment" ->
     let lv = json |> Json.value_of "leftHandSide" |> trans_expression |> exp_to_lv in
     let exp = json |> Json.value_of "rightHandSide" |> trans_expression in
     let typ = json |> Json.value_of "leftHandSide" |> trans_typeDescriptions in 
     let op = Json.value_of "operator" json in 
     (match op with
       | `String "=" -> AssignTemp (lv, exp, loc)
       | `String "+=" -> AssignTemp (lv, BinOp (Add,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "-=" -> AssignTemp (lv, BinOp (Sub,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "*=" -> AssignTemp (lv, BinOp (Mul,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "/=" -> AssignTemp (lv, BinOp (Div,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "%=" -> AssignTemp (lv, BinOp (Mod,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "|=" -> AssignTemp (lv, BinOp (BOr,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "^=" -> AssignTemp (lv, BinOp (BXor,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "&=" -> AssignTemp (lv, BinOp (BAnd,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "<<=" -> AssignTemp (lv, BinOp (ShiftL,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String ">>=" -> AssignTemp (lv, BinOp (ShiftR,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | _ -> raise (Failure " Unsupported: trans_expression5"))
  | `String "ElementaryTypeNameExpression" ->
     (* json AST from solc is slightly differnt per version. *)
     (try
       let etyp = json |> Json.value_of "typeName" |> Json.to_string |> trans_elementaryTypeName in
       ETypeName etyp
     with _ ->
       let etyp = json |> Json.value_of "typeName" |> Json.value_of "name" |> Json.to_string |> trans_elementaryTypeName in
       ETypeName etyp)
  | `String "IndexRangeAccess" ->
     let base = json |> Json.value_of "baseExpression" |> trans_expression |> (fun res -> match res with Lv lv -> lv | _ -> assert false) in
     let start = try json |> Json.value_of "startExpression" |> trans_expression |> (fun res -> Some res) with _ -> None in
     let fin = try json |> Json.value_of "endExpression" |> trans_expression |> (fun res -> Some res) with _ -> None in
     let einfo = {eloc=loc; etyp=typ; eid=nid} in
     IndexRangeAccess (base,start,fin,einfo)
  | `String s -> failwith ("trans_expression6 - " ^ s ^ " @ " ^ string_of_int loc.line)
  | _ -> failwith "Unsupported: trans_expression7"

(* nodeType: X *)  
and trans_functionCallArguments : Yojson.Basic.t -> exp list
= fun json ->
  match json with 
  | `List l ->
     List.fold_left (fun acc j -> acc @ [(trans_expression j)]) [] l
  | `Null -> [] (* no arguments: `Null, not `List [] *)
  | _ -> assert false

(* nodeType : O *)
and trans_expressionStatement : Yojson.Basic.t -> stmt
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "ExpressionStatement") in
  let json' = Json.value_of "expression" json in
  let loc = get_loc json' in
  let nid = get_id json' in
  match Json.value_of "nodeType" json' with
  | `String "FunctionCall" ->
     (match Json.value_of "kind" json' with
      | `String "functionCall" when json' |> Json.value_of "expression" |> Json.value_of "nodeType" = `String "FunctionCallOptions" ->
         let json'' = json' |> Json.value_of "expression" in
         let _ = assert (Json.value_of "nodeType" json'' = `String "FunctionCallOptions") in
         let args = json' |> Json.value_of "arguments" |> trans_functionCallArguments in (* should be be json', not json'' *)
         let exp = json'' |> Json.value_of "expression" |> trans_expression in (* for the rest, should be json'', not json' *)
         let opnames = json'' |> Json.value_of "names" |> Json.to_list |> List.map Json.to_string in
         let ops = json'' |> Json.value_of "options" |> Json.to_list |> List.map trans_expression in
         let _ = assert (List.length opnames = List.length ops) in
         let _ = assert (List.length opnames <=2 && List.length ops <=2) in
         let ethop = match BatList.index_of "value" opnames with Some n -> Some (BatList.at ops n) | None -> None in
         let gasop = match BatList.index_of "gas" opnames with Some n -> Some (BatList.at ops n) | None -> None in
         Call (None, exp, args, ethop, gasop, get_loc json')
      | `String "functionCall" ->
         let exp = json' |> Json.value_of "expression" |> trans_expression in (* function name *)
         let args = json' |> Json.value_of "arguments" |> trans_functionCallArguments in
         (* Call (None, exp, args) *)
           (* Built-in function checkers. Lists need to be updated. *)
           if is_require exp then
             (assert (List.length args = 1 || List.length args = 2);
              If (List.hd args, Skip, Throw, {if_loc=loc;if_tloc=loc; if_floc=Some loc}))
           else if is_assert exp then
             (assert (List.length args = 1);
              Seq (Assert (List.hd args, "default", get_loc json'),
                   If (List.hd args, Skip, Throw, dummy_ifinfo)))
           else Call (None, exp, args, None, None, get_loc json') (* normal case *) 
      | _ -> raise (Failure "Unsupported: trans_expressionStatement1"))
  | `String "Assignment" ->
     let lv = json' |> Json.value_of "leftHandSide" |> trans_expression |> exp_to_lv in
     let exp = json' |> Json.value_of "rightHandSide" |> trans_expression in
     let typ = json' |> Json.value_of "leftHandSide" |> trans_typeDescriptions in 
     let op = Json.value_of "operator" json' in 
       (match op with
       | `String "=" -> Assign (lv, exp, loc)
       | `String "+=" -> Assign (lv, BinOp (Add,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "-=" -> Assign (lv, BinOp (Sub,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "*=" -> Assign (lv, BinOp (Mul,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "/=" -> Assign (lv, BinOp (Div,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "%=" -> Assign (lv, BinOp (Mod,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "|=" -> Assign (lv, BinOp (BOr,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "^=" -> Assign (lv, BinOp (BXor,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "&=" -> Assign (lv, BinOp (BAnd,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String "<<=" -> Assign (lv, BinOp (ShiftL,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | `String ">>=" -> Assign (lv, BinOp (ShiftR,Lv lv,exp, {eloc=loc; etyp=typ; eid=nid}), loc)
       | _ -> raise (Failure " Unsupported: trans_expressionStatement2"))
  | `String "UnaryOperation" ->
     let op = Json.value_of "operator" json' in
     (match op with 
     | `String "++" ->
        let sub = json' |> Json.value_of "subExpression" |> trans_expression in
        let lv = exp_to_lv sub in
        Assign (lv, BinOp (Add,Lv lv,Int (BatBig_int.of_int 1),{eloc=loc; etyp=get_type_lv lv; eid=nid}), loc)
     | `String "--" ->
        let sub = json' |> Json.value_of "subExpression" |> trans_expression in
        let lv = exp_to_lv sub in
        Assign (lv, BinOp (Sub,Lv lv,Int (BatBig_int.of_int 1),{eloc=loc; etyp=get_type_lv lv; eid=nid}), loc)
     | `String "delete" ->
        let sub = json' |> Json.value_of "subExpression" |> trans_expression in
        let lv = Var ("delete",dummy_vinfo) in
        Call (None, Lv lv, [sub], None, None, loc)
     | `String s -> raise (Failure ("Unsupported Unary Op: " ^ s))
     | _ -> assert false)
  | `String "Identifier" -> Skip
  | `String "BinaryOperation" -> Skip
  | `String "IndexAccess" -> Skip
  | `String "Conditional" -> (* cond ? t : f *)
     let cond = json' |> Json.value_of "condition" |> trans_expression in
     (* since json generated by solc does not have proper structure,
      * this additional manipulation step should be forced. *)
     let t = `Assoc [("expression", Json.value_of "trueExpression" json'); ("nodeType", `String "ExpressionStatement")] |> trans_expressionStatement in
     let f = `Assoc [("expression", Json.value_of "falseExpression" json'); ("nodeType", `String "ExpressionStatement")] |> trans_expressionStatement in
     If (cond, t, f, dummy_ifinfo)
  | `String "TupleExpression" -> Skip
  | `String "FunctionCallOptions" -> Skip (* e.g., "msg.sender.call{value: msg.value-amountEth};" does nothing. E.g., '("")' should be appended. *)
  | `String s -> raise (Failure ("Unsupported: trans_expressionStatement3 - " ^ s ^ " : line " ^ string_of_int loc.line))
  | _ -> assert false

(* nodeType: X *)
let trans_visibility : Yojson.Basic.t -> visibility
= fun json ->
  match json with
  | `String "public" -> Public 
  | `String "internal" -> Internal
  | `String "external" -> External
  | `String "private" -> Private
  | _ -> raise (Failure "trans_visibility")

(* nodeType : O *)
let trans_variable_declaration : Yojson.Basic.t -> var_decl
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "VariableDeclaration") in
  let vname = json |> Json.value_of "name" |> Json.to_string in
  let typ = json |> trans_typeDescriptions in
  let vinfo =
    { vloc = json |> get_loc;
      is_gvar = json |> Json.value_of "stateVariable" |> Json.to_bool;
      vtyp = typ;
      vvis = json |> Json.value_of "visibility" |> trans_visibility;
      vid = (try json |> Json.value_of "id" |> Json.to_int with _ -> assert false);
      refid = (try json |> Json.value_of "id" |> Json.to_int with _ -> assert false); (* for the declarations, assign themselves. *)
      vscope = (try json |> Json.value_of "scope" |> Json.to_int with _ -> assert false);
      storage = (try json |> Json.value_of "storageLocation" |> Json.to_string with _ -> assert false);
      flag = true; (* true only for variable declarations *)
      org = Some (Lv (Var (vname, mk_vinfo ~typ:typ ())))
    } in
  (vname,vinfo)

let rec trans_yul_exp : (string * int) list -> int -> Yojson.Basic.t -> (id * int) list
= fun ref ast_id json ->
  let node_typ = json |> Json.value_of "nodeType" in
  match node_typ with
  | `String "YulIdentifier" ->
     let name = json |> Json.value_of "name" |> Json.to_string in
     (* Locals in assembly block do not have references in external blocks. Thus, assign assembly block's AST id. *)
     let refid = try List.assoc (json |> Json.value_of "src" |> Json.to_string) ref with Not_found -> ast_id in
     [(name, refid)]
  | `String "YulLiteral" -> []
  | `String "YulFunctionCall" ->
     (* let fname = json |> Json.value_of "functionName" |> Json.value_of "name" |> Json.to_string in *)
     let args = json |> Json.value_of "arguments" |> Json.to_list in
     let args = List.fold_left (fun acc arg -> acc @ (trans_yul_exp ref ast_id arg)) [] args in
     args
  | _ -> failwith "trans_yul_exp"

let rec trans_yul_stmt : Yojson.Basic.t -> (string * int) list -> int -> (id * int) list
= fun json ref ast_id ->
  let node_typ = json |> Json.value_of "nodeType" in
  match node_typ with
  | `String "YulVariableDeclaration" ->
     let lhs = json |> Json.value_of "variables" |> Json.to_list in
     let lhs =
       List.map (fun j ->
         let name = j |> Json.value_of "name" |> Json.to_string in
         (* let _ = print_endline name in
         let _ = print_endline (j |> Json.value_of "src" |> Json.to_string) in
         let _ = print_endline (Vocab.string_of_list (fun (src,refid) -> src ^ " : " ^ string_of_int refid) ref) in *)
         (* Locals in assembly block do not have references in external blocks. Thus, assign assembly block's AST id. *)
         let refid = try List.assoc (j |> Json.value_of "src" |> Json.to_string) ref with Not_found -> ast_id 
         in
         (name,refid)
       ) lhs in
     let rhs = json |> Json.value_of "value" |> trans_yul_exp ref ast_id in
     rhs@lhs
  | `String "YulAssignment" ->
     let lhs = json |> Json.value_of "variableNames" |> Json.to_list in
     let lhs =
       List.map (fun j ->
         let name = j |> Json.value_of "name" |> Json.to_string in
         let refid = try List.assoc (j |> Json.value_of "src" |> Json.to_string) ref with Not_found -> ast_id in
         (name,refid)
       ) lhs in
     let rhs = json |> Json.value_of "value" |> trans_yul_exp ref ast_id in
     rhs@lhs
  | `String "YulExpressionStatement" ->
     json |> Json.value_of "expression" |> trans_yul_exp ref ast_id
  | `String "YulForLoop" ->
     let condition = json |> Json.value_of "condition" |> trans_yul_exp ref ast_id in
     let pre = json |> Json.value_of "pre" |> Json.value_of "statements" |> Json.to_list in
     let pre = List.fold_left (fun acc j -> acc @ (trans_yul_stmt j ref ast_id)) [] pre in
     let post = json |> Json.value_of "post" |> Json.value_of "statements" |> Json.to_list in
     let post = List.fold_left (fun acc j -> acc @ (trans_yul_stmt j ref ast_id)) [] post in
     let body = json |> Json.value_of "body" |> Json.value_of "statements" |> Json.to_list in
     let body = List.fold_left (fun acc j -> acc @ (trans_yul_stmt j ref ast_id)) [] body in
     condition @ pre @ post @ body
     (* let _ = print_endline (Vocab.string_of_list fst s) in
     let _ = assert false in *)
  | `String "YulIf" ->
     let condition = json |> Json.value_of "condition" |> trans_yul_exp ref ast_id in
     let body = trans_yul_body json ref ast_id in
     condition @ body
  | `String "YulBreak" -> []
  | `String "YulFunctionDefinition" ->
     (* let _ = print_endline (Yojson.Basic.pretty_to_string json) in *)
     trans_yul_body json ref ast_id
  | `String "YulSwitch" ->
     (* let _ = print_endline (Yojson.Basic.pretty_to_string json) in *)
     let cases = json |> Json.value_of "cases" |> Json.to_list in
     let bodies = List.fold_left (fun acc j -> acc @ (trans_yul_body j ref ast_id)) [] cases in
     bodies
  | `String s -> failwith ("trans_yul_stmt: " ^ s ^ " @ line " ^ string_of_int (get_loc json).line)
  | _ -> assert false

and trans_yul_body : Yojson.Basic.t -> (string * int) list -> int -> (id * int) list
= fun json ref ast_id ->
  let body = json |> Json.value_of "body" |> Json.value_of "statements" |> Json.to_list in
  let body = List.fold_left (fun acc j -> acc @ (trans_yul_stmt j ref ast_id)) [] body in
  body

let trans_yul_block : Yojson.Basic.t -> (id * int) list
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "InlineAssembly") in
  let ext_refs = json |> Json.value_of "externalReferences" |> Json.to_list in
  let ext_refs =
    List.map (fun er ->
      (er |> Json.value_of "src" |> Json.to_string,  er |> Json.value_of "declaration" |> Json.to_int)
    ) ext_refs in
  let ast_id = json |> Json.value_of "id" |> Json.to_int in
  let j = Json.value_of "AST" json in
  let _ = assert (Json.value_of "nodeType" j = `String "YulBlock") in
  let statements = j |> Json.value_of "statements" |> Json.to_list in
  List.fold_left (fun acc j' ->
    acc @ (trans_yul_stmt j' ext_refs ast_id)
  ) [] statements


let param_cnt = ref 0
let param_name = "Param"
let gen_param_name () =
  param_cnt:=!param_cnt+1;
  param_name ^ (string_of_int !param_cnt)

(* nodeType: O *)
let trans_parameterList : Yojson.Basic.t -> param list
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "ParameterList") in
  let parameters = json |> Json.value_of "parameters" |> Json.to_list in (* 0 parameters => `List [] *)
  let reversed_params =
    List.fold_left (fun acc j ->
      let (vname,vinfo) = trans_variable_declaration j in
      let vname = if BatString.equal vname "" then gen_param_name () else vname in
      (vname, vinfo)::acc
    ) [] parameters in
  let params = List.rev reversed_params
  in params

(* nodeType : X *)
let rec trans_statement : Yojson.Basic.t -> stmt
= fun json ->
  let node_typ = Json.value_of "nodeType" json in
  let loc = get_loc json in
  match node_typ with
  | `String "VariableDeclarationStatement" -> (* declaration := initialValue *)
     let decl = json |> Json.value_of "declarations" |> Json.to_list in
     let _ = assert (List.length decl > 0) in
     let lv = (
       if List.length decl = 1 then   (* uint a; *)
         let var_decl = List.hd decl in
         let (vname,vinfo) = trans_variable_declaration var_decl in
         Var (vname,vinfo) 
       else  (* (a, b, c); *)
         let elst =  List.map (fun v -> 
           try
             let (vname,vinfo) = trans_variable_declaration v in
             Some (Lv (Var (vname, vinfo)))
           with _ -> None
         ) decl in
         Tuple (elst, Void)
     ) in
     (match Json.value_of "initialValue" json with
      | `Null -> Decl lv
      | exp -> Assign (lv, trans_expression exp, loc))
  | `String "ExpressionStatement" -> trans_expressionStatement json
  | `String "PlaceholderStatement" -> PlaceHolder
  | `String "ForStatement" ->
     let init = try json |> Json.value_of "initializationExpression" |> trans_statement with _ -> Skip in (* for ( ;cond;a++) *)
     let cond = try json |> Json.value_of "condition" |> trans_expression with _ -> True in (* for (init; ;a++) *)
     let body_wo_last = json |> Json.value_of "body" |> trans_statement in 
     let body_last = try json |> Json.value_of "loopExpression" |> trans_statement with _ -> Skip in (* for (init;cond; ) *)
     let body = Seq (body_wo_last,body_last) in 
     Seq (init,While (cond,body))
  | `String "WhileStatement" ->
     let cond = json |> Json.value_of "condition" |> trans_expression in
     let body = json |> Json.value_of "body" |> trans_statement in
     While (cond,body)
  | `String "DoWhileStatement" ->
     let cond = json |> Json.value_of "condition" |> trans_expression in
     let body = json |> Json.value_of "body" |> trans_statement in
     Seq (body, While (cond,body))
  | `String "IfStatement" ->
     let cond = json |> Json.value_of "condition" |> trans_expression in
     let tbody, tloc = json |> Json.value_of "trueBody" |> trans_statement, json |> Json.value_of "trueBody" |> get_loc in
     let fbody, floc =
       match json |> Json.value_of "falseBody" with
       | `Null -> (Skip, None)
       | fb -> (trans_statement fb, Some (get_loc fb)) in
     let ifinfo = {if_loc = loc; if_tloc = tloc; if_floc = floc} in
     If (cond, tbody, fbody, ifinfo)

  | `String "Return" ->
     (match Json.value_of "expression" json with
      | `Null -> Return (None, loc)
      | exp -> Return (Some (trans_expression exp), loc))
  | `String "Throw" -> Throw
  | `String "Block" -> trans_block json
  | `String "EmitStatement" -> Skip
  | `String "Break" -> Break
  | `String "Continue" -> Continue
  | `String "InlineAssembly" ->
     (try
        let ext_refs = json |> Json.value_of "externalReferences" |> Json.to_list in
        let var_refid_pairs =
          List.map (fun j ->
            match j with
            | `Assoc ((s,j')::[]) -> (s, j' |> Json.value_of "declaration" |> Json.to_int)
            | _ -> raise (Failure "InlineAssembly")
        ) ext_refs in
        Assembly (var_refid_pairs, loc)
     with _ ->
       let var_refid_pairs = trans_yul_block json in
       Assembly (var_refid_pairs, loc))
  | `String "UncheckedBlock" ->
     let slst = json |> Json.value_of "statements" |> Json.to_list |> List.map trans_statement in
     Unchecked (slst,loc)
  | `String "TryStatement" ->
     let json' = json |> Json.value_of "externalCall" in
     let _ = assert (Json.value_of "nodeType" json' |> Json.to_string = "FunctionCall") in
     let extern_call = trans_expression json' in
     let _ = assert (match extern_call with CallTemp _ -> true | _ -> false) in
     let clauses = json |> Json.value_of "clauses" |> Json.to_list in
     let rec f i lst =
       match lst with
       | [] -> Throw
       | j::tl when i=0 -> (* try *)
         let _ = assert (j |> Json.value_of "nodeType" |> Json.to_string = "TryCatchClause") in
         let assign =
           if Json.value_of "parameters" j = `Null then Skip (* no parameters *)
           else (* parameter exists *)
             let params = j |> Json.value_of "parameters" |> trans_parameterList in
             let lv = params_to_lv params in
             Assign (lv, extern_call, loc)
         in
         let stmt = trans_block (j |> Json.value_of "block") in
         If (Lv (gen_tmpvar (EType Bool)), Seq (assign, stmt), f (i+1) tl, dummy_ifinfo)
       | j::tl -> (* catch *)
         let stmt = trans_block (j |> Json.value_of "block") in
         If (Lv (gen_tmpvar (EType Bool)), stmt, f (i+1) tl, dummy_ifinfo)
     in
     f 0 clauses
  | `String "RevertStatement" -> Throw
  | `String s -> raise (Failure ("Unsupported: trans_statement - " ^ s ^ " : line " ^ string_of_int loc.line))
  | _ -> assert false

(* nodeType : O *)
and trans_block : Yojson.Basic.t -> stmt
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "Block") in
  let statements = json |> Json.value_of "statements" |> Json.to_list in
  List.fold_left (fun acc j ->
    let new_stmt = trans_statement j in
    Seq (acc, new_stmt)
  ) Skip statements 

(* usual: defined modifiers appear as invocations *)
(* unusual: consturctor invocations appear as modifiers *)
let is_usual_modifier: string list -> Yojson.Basic.t -> bool
= fun cnames json ->
  let _ = assert (Json.value_of "nodeType" json = `String "ModifierInvocation") in
  let modname = json |> Json.value_of "modifierName" |> Json.value_of "name" |> Json.to_string in
  not (List.mem modname cnames)

(* nodeType: O *)
let trans_modifierInvocation : Yojson.Basic.t -> mod_call
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "ModifierInvocation") in
  let name = json |> Json.value_of "modifierName" |> Json.value_of "name" |> Json.to_string in
  let args = json |> Json.value_of "arguments" |> trans_functionCallArguments in
  let loc = get_loc json in
  (name, args, loc)

(* generate Constructor call *)
let trans_inheritanceSpecifier : Yojson.Basic.t -> mod_call
= fun json ->
  let _ = assert (Json.value_of "nodeType" json = `String "InheritanceSpecifier") in
  let name = json |> Json.value_of "baseName" |> Json.value_of "name" |> Json.to_string in
  let args = json |> Json.value_of "arguments" |> trans_functionCallArguments in
  let loc = get_loc json in
  (name, args, loc)

let resolve_cnstr_calls : mod_call list -> mod_call list ->
                          mod_call list
= fun inherit_calls mod_calls ->
  (* In solc 0.4x, mod_calls has the priority over the inherit_calls. *)
  (* In recent solc, specifying arguments for both places is an error. *)
  (* E.g.,
   * Inherit: A(5) B(3), Mod: A(8) C(7) => B(3) A(8) C(7) *)
  let combined = inherit_calls @ mod_calls in
  let combined = List.rev combined in (* rev list to give the priority to mod_calls *)
  List.fold_left (fun acc m ->
    let b = List.exists (fun (x,_,_) -> x = triple_fst m) acc in
    if b then acc
    else m::acc
  ) [] combined

let trans_isConstructor : Yojson.Basic.t -> bool
= fun j ->
  let _ = assert (Json.value_of "nodeType" j = `String "FunctionDefinition") in
  try
    j |> Json.value_of "isConstructor" |> Json.to_bool (* solc 0.4x *)
  with _ -> (* solc 0.5x *)
   (match Json.value_of "kind" j with
    | `String "constructor" -> true
    | `String "function" -> false
    | `String "fallback" -> false
    | `String "receive" -> false
    | `String s -> failwith ("trans_isConstructor: " ^ s)
    | _ -> assert false)

let trans_fname : Yojson.Basic.t -> bool -> string -> string
= fun j is_constructor cid ->
  let _ = assert (Json.value_of "nodeType" j = `String "FunctionDefinition") in
  try
    (match Json.value_of "kind" j with (* solc 0.5x *)
    | `String "constructor" -> cid
    | `String "function" -> j |> Json.value_of "name" |> Json.to_string
    | `String "fallback" -> "@fallback"
    | `String "receive" -> "@receive" (* solc 0.6x *)
    | `String s -> failwith ("trans_fname : " ^ s)
    | _ -> assert false)
  with _ -> (* solc 0.4x *)
    let fname = j |> Json.value_of "name" |> Json.to_string in
    let fname = if is_constructor then cid else if fname = "" then "@fallback" else fname in
    fname

let trans_payable : Yojson.Basic.t -> bool
= fun j ->
  let _ = assert (Json.value_of "nodeType" j = `String "FunctionDefinition") in
  try
    j |> Json.value_of "payable" |> Json.to_bool (* 0.4x *)
  with _ -> (* 0.5x *)
    (match Json.value_of "stateMutability" j with
     | `String "payable" -> true
     | `String "nonpayable" -> false
     | `String "view" -> false
     | `String "pure" -> false
     | _ -> failwith "stateMutability")

let trans_mutability : Yojson.Basic.t -> state_mutability
= fun j ->
  let _ = assert (Json.value_of "nodeType" j = `String "FunctionDefinition") in
  (match Json.value_of "stateMutability" j with
   | `String "payable" -> Payable
   | `String "nonpayable" -> NonPayable
   | `String "view" -> View
   | `String "pure" -> Pure
   | _ -> failwith "stateMutability")

let trans_structDefinition : Yojson.Basic.t -> structure
= fun j ->
  let name = j |> Json.value_of "canonicalName" |> Json.to_string in
  let decls = List.map trans_variable_declaration (j |> Json.value_of "members" |> Json.to_list) in
  (name,decls)

(* nodeType : O *)
let trans_contractDefinition : string list -> structure list -> Yojson.Basic.t -> contract
= fun cnames global_structs json ->
  let cid = json |> Json.value_of "name" |> Json.to_string in
  let contract_parts = json |> Json.value_of "nodes" |> Json.to_list in
  let cinfo =
    { numid = json |> Json.value_of "id" |> Json.to_int;
      inherit_order = List.map Json.to_int (json |> Json.value_of "linearizedBaseContracts" |> Json.to_list);
      lib_typ_lst = [];
      ckind = json |> Json.value_of "contractKind" |> Json.to_string
    } in
  let base_contracts = json |> Json.value_of "baseContracts" |> Json.to_list in (* A is B,C,D => base_contracts : [B; C; D] *)
  let cnstr_calls_inherit =
    List.fold_left (fun acc base ->
      let cnstr_call = trans_inheritanceSpecifier base in
      if List.length (triple_snd cnstr_call) > 0 then
        acc @ [cnstr_call] (* constructors are called starting from parents *)
      else acc
    ) [] base_contracts in
  let (cid, gvar_decls, structs, enums, func_defs, cinfo) =
    List.fold_left (fun ((cid, gvar_decls, structs, enums, func_defs, cinfo) as acc) j ->
      let node_typ = Json.value_of "nodeType" j in
      match node_typ with
      | `String "VariableDeclaration" ->
         let (vname,vinfo) = trans_variable_declaration j in
         let expop =
           (match j |> Json.value_of "value" with
            | `Null -> None
            | exp -> Some (trans_expression exp)) in
         let decl = (vname, expop, vinfo) in
         (cid, decl::gvar_decls, structs, enums, func_defs, cinfo)
      | `String "EventDefinition" -> (* Event is modeled as a function with internal visibility and a skip body *)
         let fname = j |> Json.value_of "name" |> Json.to_string in
         let params = j |> Json.value_of "parameters" |> trans_parameterList in
         let finfo =
           { is_constructor = false;
             is_payable = false;
             is_modifier = false;
             mod_list = [];
             mod_list2 = [];
             param_loc = dummy_loc;
             ret_param_loc = dummy_loc;
             fvis = Internal;
             mutability = Pure; (* event can be considered as pure *)
             fid = j |> Json.value_of "id" |> Json.to_int;
             floc = get_loc j;
             scope = cinfo.numid;
             scope_s = cid; (* to be filled by preprocessor *)
             org_scope_s = cid;
             cfg = empty_cfg
           } in
         let stmt = Skip in
         let func = (fname,params,[],stmt,finfo) in
         (cid, gvar_decls, structs, enums, func::func_defs, cinfo)
      | `String "EnumDefinition" ->
         let name = j |> Json.value_of "name" |> Json.to_string in
         let members = List.map (fun j' -> j' |> Json.value_of "name" |> Json.to_string) (j |> Json.value_of "members" |> Json.to_list) in
         let enum = (name,members) in
         (cid, gvar_decls, structs, enums@[enum], func_defs, cinfo)
      | `String "StructDefinition" ->
         let structure = trans_structDefinition j in
         (cid, gvar_decls, structs @ [structure], enums, func_defs, cinfo)
      | `String "UsingForDirective" -> 
         let lib_name = j |> Json.value_of "libraryName" |> Json.value_of "name" |> Json.to_string in
         (* let typ = trans_typeDescriptions j in *)
         let typ = j |> Json.value_of "typeName" |> trans_typeDescriptions in
         let cinfo = {cinfo with lib_typ_lst = (lib_name,typ)::cinfo.lib_typ_lst} in
         (cid, gvar_decls, structs, enums, func_defs, cinfo)
      | `String "FunctionDefinition" ->
         let is_constructor = trans_isConstructor j in
         let fname = trans_fname j is_constructor cid in
         let params = j |> Json.value_of "parameters" |> trans_parameterList in
         let ret_params = j |> Json.value_of "returnParameters" |> trans_parameterList in
         let stmt =
           if j |> Json.value_of "implemented" |> Json.to_bool then j |> Json.value_of "body" |> trans_block
           else Skip (* function without definitions *)
         in
         let modifiers = j |> Json.value_of "modifiers" |> Json.to_list in
         (* executed in the order of usual mod call => constructor mod call *)
         let mod_calls =
           List.fold_left (fun acc j' ->
             if is_usual_modifier cnames j' then acc @ [(trans_modifierInvocation j')]
             else acc
           ) [] modifiers
         in
         let cnstr_calls_mod =
           List.fold_left (fun acc j' ->
             if not (is_usual_modifier cnames j') then acc @ [(trans_modifierInvocation j')]
             else acc
           ) [] modifiers
         in
         let cnstr_calls = resolve_cnstr_calls cnstr_calls_inherit cnstr_calls_mod in
         let finfo =
           { is_constructor = is_constructor;
             is_payable = trans_payable j;
             is_modifier = false;
             mod_list = mod_calls;
             mod_list2 = if is_constructor then cnstr_calls else [];
             param_loc = j |> Json.value_of "parameters" |> get_loc;
             ret_param_loc = j |> Json.value_of "returnParameters" |> get_loc;
             fvis = j |> Json.value_of "visibility" |> trans_visibility;
             mutability = trans_mutability j;
             fid = j |> Json.value_of "id" |> Json.to_int;
             floc = get_loc j;
             scope = cinfo.numid;
             scope_s = cid;
             org_scope_s = cid;
             cfg = empty_cfg
           }
         in
         let func = (fname, params, ret_params, stmt, finfo) in
         (cid, gvar_decls, structs, enums, func::func_defs, cinfo)
      | `String "ModifierDefinition" ->
         let fname = j |> Json.value_of "name" |> Json.to_string in
         let params = j |> Json.value_of "parameters" |> trans_parameterList in
         let finfo =
           { is_constructor = false;
             is_payable = false;
             is_modifier = true;
             mod_list = []; (* no modifier invocations in modifier definition *)
             mod_list2 = []; (* same as above *)
             param_loc = dummy_loc;
             ret_param_loc = dummy_loc;
             fvis = j |> Json.value_of "visibility" |> trans_visibility;
             mutability = NonPayable; (* field does not exist *)
             fid = j |> Json.value_of "id" |> Json.to_int;
             floc = get_loc j;
             scope = cinfo.numid;
             scope_s = cid;
             org_scope_s = cid;
             cfg = empty_cfg
           } in
         let stmt = j |> Json.value_of "body" |> trans_block in
         let func = (fname, params, [], stmt, finfo) in
         (cid, gvar_decls, structs, enums, func::func_defs, cinfo)
      | `String "ErrorDefinition" -> acc
      | `String s -> failwith ("Unsupported: trans_contractDefinition - " ^ s)
      | _ -> assert false
    ) (cid, [], global_structs, [], [], cinfo) contract_parts in
  let (gvar_decls, func_defs) = (List.rev gvar_decls, List.rev func_defs) in 
  let b = List.exists (fun (_,_,_,_,finfo) -> finfo.is_constructor) func_defs in
    if b then (cid, gvar_decls, structs, enums, func_defs, cinfo)
    else
      (* make a new constructor if does not exist *)
      let fname = cid in
      let params = [] in
      let cnstr_calls = resolve_cnstr_calls cnstr_calls_inherit [] in
      let finfo =
        {is_constructor = true;
         is_payable = false;
         is_modifier = false;
         mod_list = []; mod_list2 = cnstr_calls;
         param_loc = dummy_loc; ret_param_loc = dummy_loc;
         fvis = Public; fid = (-1);
         mutability = NonPayable;
         floc = dummy_loc;
         scope = cinfo.numid; scope_s = cid; org_scope_s = cid; cfg = empty_cfg}
      in
      let cnstr = (fname, params, [], Skip, finfo) in
      (cid, gvar_decls, structs, enums, cnstr::func_defs, cinfo) 

let translate : Yojson.Basic.t -> string list -> pgm
= fun json lines ->
  let _ = record_end_of_lines lines in
  let _ = assert (Json.value_of "nodeType" json = `String "SourceUnit") in
  let l = json |> Json.value_of "nodes" |> Json.to_list in (* 0 nodes => `List [] *)
  let global_structs = List.filter (fun j -> Json.value_of "nodeType" j = `String "StructDefinition") l in
  let global_structs = List.map trans_structDefinition global_structs in
  let contracts = List.filter (fun j -> Json.value_of "nodeType" j = `String "ContractDefinition") l in
  let cnames = List.map (fun j -> j |> Json.value_of "name" |> Json.to_string) contracts in
  List.map (trans_contractDefinition cnames global_structs) contracts

let run json = translate json
