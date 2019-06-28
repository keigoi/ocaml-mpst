open Mpst
open Usecase.Protocols

let () =
  let open TwoBuyer in
  print_endline "TwoBuyer...";
  let g = gen @@ TwoBuyer.g () in
  let _epa = get_ep a g in
  let _epb = get_ep b g in
  let _epc = get_ep s g in
  print_endline "Done."

let () =
  (* let open ThreeBuyer in *)
  print_endline "ThreeBuyer...";
  let g = gen @@ ThreeBuyer.g () in
  let _eps = get_ep b g in
  let _epc = get_ep c g in
  print_endline "Done."

let () =
  (* let open Fibo in *)
  print_endline "Fibo...";
  let g = gen @@ Fibo.g () in
  let _eps = get_ep a g in
  let _epc = get_ep b g in
  print_endline "Done"

let () =
  let open SH in
  print_endline "SH...";
  let g = gen @@ SH.g () in
  let _epp = get_ep p g in
  let _epr = get_ep r g in
  let _epc = get_ep c g in
  print_endline "Done"

let () =
  let open SapNego in
  print_endline "SapNego...";
  let g = gen @@ SapNego.g () in
  let _epp = get_ep p g in
  let _epc = get_ep c g in
  print_endline "Done"

let () =
  let open SimpleVoting in
  print_endline "SimpleVoting...";
  let g = gen @@ SimpleVoting.g () in
  let _eps = get_ep s g in
  let _epv = get_ep v g in
  print_endline "Done"

let () =
  let open TravelAgency in
  print_endline "TravelAgency...";
  let g = gen @@ TravelAgency.g () in
  let _epa = get_ep c g in
  let _epc = get_ep a g in
  let _eps = get_ep s g in
  print_endline "Done"

let () =
  let open SupplierInfo_microservice in
  print_endline "SupplierInfo_microservice...";
  let g = gen @@ SupplierInfo_microservice.partnership_supplier () in
  let _epl = get_ep loginsvc g in
  let _epr = get_ep requestor g in
  let _epa = get_ep authorisesvc g in
  let _epf = get_ep filtersvc g in
  let _eps = get_ep suppliersvc g in
  let _epc = get_ep contractsvc g in
  print_endline "Done"

let () =
  let open Smtp in
  print_endline "Smtp...";
  let g = gen @@ Smtp.smtp () in
  let _epc = get_ep c g in
  let _eps = get_ep s g in
  print_endline "Done"

let () =
  let open SleepingBarber in
  print_endline "SleepingBarber...";
  let g = gen @@ SleepingBarber.haircut () in
  let _epc = get_ep customer g in
  let _epb = get_ep barber g in
  let g = gen @@ SleepingBarber.g () in
  let _epc = get_ep customer g in
  let _epb = get_ep barber g in
  let _eps = get_ep shop g in
  print_endline "Done"

(* let () =
 *   let open SigSmoker in
 *   print_endline "SigSmoker...";
 *   let g = gen @@ SigSmoker.g () in
 *   let _epa = get_ep a g in
 *   let _eps = get_ep s g in
 *   print_endline "Done" *)

let () =
  let open Game in
  print_endline "Game...";
  let g = gen @@ Game.main () in
  let _eps = get_ep srv g in
  let _epc = get_ep cli g in
  let g = gen @@ Game.game () in
  let _epa = get_ep a g in
  let _epb = get_ep b g in
  let _epc = get_ep c g in
  print_endline "Done"

let () =
  let open MapReduce in
  print_endline "MapReduce...";
  let g = gen @@ MapReduce.g () in
  let _epm = get_ep mst g in
  let _epw = get_ep wrk g in
  print_endline "Done"

let () =
  let open NQueen in
  print_endline "NQueen...";
  let g = gen @@ NQueen.g () in
  let _epm = get_ep mst g in
  let _epw = get_ep wrk g in
  let g = gen @@ NQueen.reply () in
  let _epa = get_ep mst g in
  let _epb = get_ep wrk g in
  print_endline "Done"

let () =
  let open Santa in
  print_endline "Santa...";
  let g = gen @@ Santa.elf_session () in
  let _eps = get_ep santa g in
  let _epe = get_ep elf g in
  let g = gen @@ Santa.reindeer_session () in
  let _epe = get_ep santa g in
  let _epr = get_ep reindeer g in
  let g = gen @@ Santa.main () in
  let _epa = get_ep santa g in
  let _epb = get_ep elf g in
  let _epb = get_ep reindeer g in
  print_endline "Done"

let () =
  print_endline "All done."
