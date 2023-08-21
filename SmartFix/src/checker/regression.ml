open Query
open Vlang
open Lang
open Options
open Semantics
open Z3Interface
open ExploitPreprocess

let collect_queries : Global.t -> vformula -> Path.t -> stmt -> query list
= fun global vf path stmt ->
  match stmt with
  | Assert (e, "no_effect", loc) ->
    let sc = convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [{vc=vc; vc2=sc; kind=NO_EFFECT; qloc=loc.line; org_q=Org_Exp e; path=path; src_f=Path.get_fkey path; sc_src=sc_src; attacker_src=""; eth_src=""}]
  | Assert (e, "assign_const", loc) ->
    let sc = convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [{vc=vc; vc2=sc; kind=ASSIGN_CONST; qloc=loc.line; org_q=Org_Exp e; path=path; src_f=Path.get_fkey path; sc_src=sc_src; attacker_src=""; eth_src=""}]
  | Assert (e, "deadcode", loc) ->
    let sc = convert_bexp e in
    let vc = Imply (vf, sc) in
    let sc_src = "No Need" in
    [{vc=vc; vc2=sc; kind=DEAD; qloc=loc.line; org_q=Org_Exp e; path=path; src_f=Path.get_fkey path; sc_src=sc_src; attacker_src=""; eth_src=""}]
  (* | Assert (e,"reentrancy",loc) ->
    let sc = convert_bexp e in
    let vc = Imply (vf, sc) in
    [{vc=vc; vc2=sc; kind=RE_ENT; loc=loc; org_q=Org_Exp e; path=path; src_f=Path.get_fkey path; sc_src=""}] *)
  | _ -> []
