open Mpst
open Usecase_util

  let santa = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Santa=v end); call_obj=(fun o->o#role_Santa)}}
  let reindeer = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Reindeer=v end); call_obj=(fun o->o#role_Reindeer)}}
  let elf = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Elf=v end); call_obj=(fun o->o#role_Elf)}}

  let to_santa m = to_ m santa santa santa
  let to_reindeer m = to_ m reindeer reindeer reindeer
  let to_elf m = to_ m elf elf elf

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
