open Mpst
open Usecase_util

let elf_session () =
  fix (fun t ->
      (gather elf santa) knock @@
      (scatter santa elf) maketoy @@
      t)

let reindeer_session () =
  fix (fun t ->
      (gather reindeer santa) knock @@
      (scatter santa elf) deliver @@
      t)

let finish =
  with_multirole ~at:elf @@
  with_multirole ~at:reindeer @@
  finish

let main () =
  scatter santa elf (start >>: get_ty_list elf (elf_session ())) @@
  scatter santa reindeer (start >>: get_ty_list reindeer (reindeer_session ())) @@
  finish
