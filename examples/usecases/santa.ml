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

  let main () =
    scatter santa elf (start >>: prot elf (elf_session ())) @@
      scatter santa reindeer (start >>: prot reindeer (reindeer_session ())) @@
        finish
