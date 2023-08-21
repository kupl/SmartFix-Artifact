
let value_of = Yojson.Basic.Util.member
let to_string = Yojson.Basic.Util.to_string
let to_list = Yojson.Basic.Util.to_list
let to_bool = Yojson.Basic.Util.to_bool
let to_int = Yojson.Basic.Util.to_int
let to_float = Yojson.Basic.Util.to_float

let print j = print_endline (Yojson.Basic.show j) (* for debugging *)

let mk_err_report ?(reportdir="./output") ~filename ~errmsg ~time =
  let j =
    `Assoc [("fileName", `String filename);
            ("baseName", `String (Filename.basename filename));
            ("time", `Float time);
            ("errMsg", `String errmsg);
            ("result", `List [])] in
  let reportfile = Filename.concat reportdir (BatString.rchop ~n:4 (Filename.basename filename) ^ ".json") in
  let f = open_out reportfile in
  Printf.fprintf f "%s" (Yojson.Basic.pretty_to_string j);
  close_out f
