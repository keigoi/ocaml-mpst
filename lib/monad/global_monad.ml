type 'a lin = 'a Linocaml.lin
type 'a data = 'a Linocaml.data

module Make
         (M:Mpst.S.MONAD)
         (EV:Mpst.S.EVENT with type 'a monad = 'a M.t)
         (C:Mpst.S.SERIAL with type 'a monad = 'a M.t)
         (L:Linocaml.S.S with type 'a IO.io = 'a EV.monad)
  = struct
  include
    Mpst.Global.Make
      (Linocaml_lin.EP)
      (Linocaml_lin.Lin)
      (M)
      (EV)
      (C)

  let linret f = {L.__m=(fun pre -> M.return (pre, {Linocaml.__lin=f ()}))}

  let raw_gen = gen
  let raw_gen_ipc = gen_ipc
  let raw_gen_mult = gen_mult
  let raw_gen_mult_ipc = gen_mult_ipc
  let raw_gen_with_kinds = gen_with_kinds
  let raw_gen_with_kinds_mult = gen_with_kinds_mult

  let gen g = linret (fun () -> gen g)

  let gen_ipc g = linret (fun () -> gen_ipc g)

  let gen_mult ps g = linret (fun () -> gen_mult ps g)

  let gen_mult_ipc ps g = linret (fun () -> gen_mult_ipc ps g)

  let gen_with_kinds ps g = linret (fun () -> gen_with_kinds ps g)

  let gen_with_kinds_mult ps g = linret (fun () -> gen_with_kinds_mult ps g)

  let degen : (([`cons of Mpst.close * 't] as 't) Seq.t lin, unit, unit data) L.monad =
    {L.__m=(fun _ -> M.return ((), {Linocaml.data=()}))}

  let raw_get_ch = get_ch

  let mclose =
    Linocaml_lin.EP.make_simple [Mpst.Close]

  let get_ch r =
    let open Linocaml in
    let open L in
    {__m=(fun lpre ->
       let g = lpre.__lin in
       let ep = List.hd @@ Linocaml_lin.EP.fresh_all (Seq.lens_get r.role_index g) in
       let g' = Seq.lens_put r.role_index g mclose in
       M.return ((), ({__lin=({__lin=g'},{__lin=ep})})))}

  let rec all_empty = `cons((), all_empty)

  let thread_create l f x =
    {L.__m=(fun lpre ->
       let ep = Linocaml.lens_get l lpre in
       let lpost = Linocaml.lens_put l lpre () in
       let open Linocaml in
       let open L in
       let th () =
         M.bind ((f x).__m (`cons(ep,all_empty))) (fun b ->
         let ((_:all_empty),{data=()}) = b in
         M.return ())
       in
       M.async th;
       M.return (lpost,{data=()}))}

  let accept sh r =
    {L.__m=(fun lpre ->
       M.bind (accept sh r) (fun ep ->
       M.return (lpre, {Linocaml.__lin=ep}))
    )}

  let connect sh r =
    {L.__m=(fun lpre ->
       M.bind (connect sh r) (fun ep ->
       M.return (lpre, {Linocaml.__lin=ep}))
    )}
end[@@inline]
