open Patch
open Lang
open FixTarget

let generate : Global.t -> pgm -> func -> patch_comp list
= fun global pgm f ->
  PatchACC.report_aware_template global pgm f
