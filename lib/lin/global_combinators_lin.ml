open Concur_shims

module Lin = struct
  type 'a lin = 'a Linocaml.lin
  let[@inline] mklin x = {Linocaml.__lin=x}
end

type 'a data = 'a Linocaml.data

open Linocaml
include Mpst.Global_combinators.Make(Mpst.Dyn_lin.NoCheck)(Lin)


type kind = [ `IPCProcess | `Local | `Untyped ]

let (let*) = IO.bind

type all_empty = Linocaml.all_empty
type ('a,'b,'c,'d) lens = ('a,'b,'c,'d) Linocaml.lens
type ('a,'b,'c) monad = ('a,'b,'c) Linocaml.monad

let[@inline] unlin x = x.__lin

let send_raw = send
let receive_raw = receive
let close_raw = close

let send_many_raw = send_many

let receive_many_raw = receive_many

let[@inline] send idx sel v = {__m=(fun[@inline] lpre ->
    let s = lens_get idx lpre in
    IO.bind (send_raw (sel (unlin s)) {data=v}) (fun[@inline] t ->
    IO.return (lens_put idx lpre (), {__lin=t}))
  )}

let deleg_send idx1 sel idx2 = {__m=(fun lpre ->
    let t = lens_get idx2 lpre in
    let lmid = lens_put idx2 lpre () in
    let s = lens_get idx1 lmid in
    let* u[@inline] = send_raw (sel (unlin s)) t in
    IO.return (lens_put idx1 lmid (), {__lin=u})
  )}

let[@inline] receive idx sel = {__m=(fun[@inline] lpre ->
    let s = lens_get idx lpre in
    IO.bind (receive_raw (sel (unlin s))) (fun[@inline] var ->
    IO.return (lens_put idx lpre (), {__lin=var}))
  )}

let[@inline] send_many idx sel f = {__m=(fun[@inline] lpre ->
  let s = lens_get idx lpre in
  IO.bind (send_many_raw (sel (unlin s)) (fun x->{data=f x})) (fun[@inline] t ->
  IO.return (lens_put idx lpre (), {__lin=t}))
)}

let[@inline] receive_many idx sel = {__m=(fun[@inline] lpre ->
  let s = lens_get idx lpre in
  IO.bind (receive_many_raw (sel (unlin s))) (fun[@inline] var ->
  IO.return (lens_put idx lpre (), {__lin=var}))
)}

let[@inline] close idx = {__m=(fun[@inline] lpre ->
    let s = lens_get idx lpre in
    IO.bind (close_raw (unlin s)) (fun[@inline] () ->
    IO.return (lens_put idx lpre (), {data=()}))
  )}

let create_thread_lin idx idx' m =
  {__m=(fun lpre ->
        let s = lens_get idx lpre in
        let lpost = lens_put idx lpre () in
        let rec all_empty = `cons((), all_empty) in
        let (_ : Thread.t) =
          Thread.create
            (fun s -> Syntax.Internal._run (m ()) (lens_put idx' all_empty s))
            s
        in
        IO.return (lpost, {data=()})
      )}

module LinocamlStyle = struct
  (* "root" index *)
  let s0 = Other ((fun[@inline] x -> x), (fun[@inline] _ x -> x))

  let[@inline] ( @* ) l1 l2 =
    let open Linocaml in
    let[@inline] get p =
      let c,q = lens_get l2 p, lens_put l2 p () in
      let a = lens_get l1 q in
      a,c
    and[@inline] put p b =
      let q = lens_put l2 p () in
      let r = lens_put l1 q b in
      r
    in
    Other(get,put)

  let[@inline] send sel v = {__m=(fun[@inline] lpre ->
      let* s[@inline] = send_raw (sel (unlin lpre)) {data=v} in
      IO.return ((), {__lin=s})
    )}

  let[@inline] deleg_send sel = {__m=(fun[@inline] lpre ->
      let subj, obj = lpre in
      let* ep[@inline] = send_raw (sel (unlin subj)) obj in
      IO.return ((), {__lin=ep})
    )}

  let[@inline] receive sel = {__m=(fun[@inline] lpre ->
      let* var[@inline] = receive_raw (sel (unlin lpre)) in
      IO.return ((), {__lin=var})
    )}

  let close = {__m=(fun[@inline] lpre ->
      IO.bind (close_raw (unlin lpre)) (fun[@inline] () -> 
      IO.return ((), {data=()}))
    )}

  let create_thread_lin (m: unit -> ('a lin, unit, unit data) monad) =
    {__m=(fun lpre ->
          let (_ : Thread.t) =
            Thread.create
              (fun x -> Syntax.Internal._run (m ()) x)
              lpre
          in
          IO.return ((), {data=()})
        )}
end

let linret f = {Linocaml.__m=(fun pre -> IO.return (pre, {Linocaml.__lin=f ()}))}

let raw_gen = gen
(* let raw_gen_ipc = gen_ipc *)
let raw_gen_mult = gen_mult
(* let raw_gen_mult_ipc = gen_mult_ipc *)
let raw_gen_with_kinds = gen_with_kinds
let raw_gen_with_kinds_mult = gen_with_kinds_mult
let gen g = linret (fun () -> gen g)

(* let gen_ipc g = linret (fun () -> gen_ipc g) *)

let gen_mult ps g = linret (fun () -> gen_mult ps g)

(* let gen_mult_ipc ps g = linret (fun () -> gen_mult_ipc ps g) *)

let gen_with_kinds ps g = linret (fun () -> gen_with_kinds ps g)

let gen_with_kinds_mult ps g = linret (fun () -> gen_with_kinds_mult ps g)

let degen : (([`cons of close one * 't] as 't) tup lin, unit, unit data) Linocaml.monad =
  {Linocaml.__m=(fun _ -> IO.return ((), {Linocaml.data=()}))}

let degen_ : (([`cons of close one * 't] as 't) tup lin, unit, 'pre, 'post) lens -> ('pre, 'post, unit data) Linocaml.monad =
  fun s ->
  {Linocaml.__m=(fun lpre -> IO.return (lens_put s lpre (), {Linocaml.data=()}))}

let raw_get_ch = get_ch

let get_ch r =
  let open Linocaml in
  {__m=(fun lpre ->
     let g = lpre.__lin in
     let ep, g' = get_ch_ r g in
     IO.return ((), ({__lin=({__lin=g'},{__lin=ep})})))}

let get_ch_list r =
  let open Linocaml in
  {__m=(fun lpre ->
     let g = lpre.__lin in
     let ep, g' = get_ch_list_ r g in
     IO.return ((), ({__lin=({__lin=g'},{__lin=ep})})))}

let get_ch_ s r =
  let open Linocaml in
  {__m=(fun lpre ->
      let g = (lens_get s lpre).__lin in
      let ep, g' = get_ch_ r g in
      IO.return (lens_put s lpre (), ({__lin=({__lin=g'},{__lin=ep})})))}

let get_ch_list_ s r =
  let open Linocaml in
  {__m=(fun lpre ->
      let g = (lens_get s lpre).__lin in
      let ep, g' = get_ch_list_ r g in
      IO.return (lens_put s lpre (), ({__lin=({__lin=g'},{__lin=ep})})))}
