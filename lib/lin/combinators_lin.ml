open Concur_shims

open Mpst.Internal
open Mpst.Internal.Base

module Lin : Combinators.LIN with type 'a lin = 'a Linocaml.lin = struct
  type 'a lin = 'a Linocaml.lin
  let mklin x = {Linocaml.__lin=x}
end

include Combinators.Make(Dyn_lin.NoCheck)(Lin)

type 'a lin = 'a Linocaml.lin
type 'a data = 'a Linocaml.data

module Static : sig
  open Linocaml

  val send :
    ('s lin, unit, 'pre, 'post) lens ->
    ((< .. > as 's) -> ('v data, 't) out) -> 'v ->
    ('pre, 'post, 't lin) monad

  val deleg_send :
    ('s lin, unit, 'mid, 'post) lens ->
    ((< .. > as 's) -> ('t lin, 'u) out) ->
    ('t lin, unit, 'pre, 'mid) lens ->
    ('pre, 'post, 'u lin) monad

  val receive :
    ('s lin, unit, 'pre, 'post) lens ->
    ((< .. > as 's) -> 'var inp) ->
    ('pre, 'post, 'var lin) monad

  val close :
    (close lin, unit, 'pre, 'post) lens ->
    ('pre, 'post, unit data) monad

  val create_thread_lin :
    ('s lin, unit, 'pre, 'post) lens ->
    (unit, 's lin, all_empty, 'pre2) lens ->
    (unit -> ('pre2, all_empty, unit data) monad) ->
    ('pre, 'post, unit data) monad

  module LinocamlStyle : sig
    val s0 : ('x, 'y, 'x, 'y) lens

    val ( @* ) :
      ('a,'b,'q,'r) lens
      -> ('c,unit,'p,'q) lens
      -> ('a * 'c,'b,'p,'r) lens

    val send : ((< .. > as 's) -> ('v data, 't) out) -> 'v -> ('s lin, unit, 't lin) monad

    val deleg_send : ((< .. > as 's) -> ('t lin, 'u) out) -> ('s lin * 't lin, unit, 'u lin) monad

    val receive : ((< .. > as 's) -> 'var inp) -> ('s lin, unit, 'var lin) monad

    val close : (close lin, unit, unit data) monad

    val create_thread_lin :
      (unit -> ('a lin, unit, unit data) monad) -> ('a lin, unit, unit data) monad
  end
end = struct
  open Linocaml
      
  let (let*) = IO.bind

  let unlin x = x.__lin

  let send_raw = send
  let receive_raw = receive
  let close_raw = close

  let send idx sel v = {__m=(fun lpre ->
      let s = lens_get idx lpre in
      let* t = send_raw (sel (unlin s)) {data=v} in
      IO.return (lens_put idx lpre (), {__lin=t})
    )}

  let deleg_send idx1 sel idx2 = {__m=(fun lpre ->
      let t = lens_get idx2 lpre in
      let lmid = lens_put idx2 lpre () in
      let s = lens_get idx1 lmid in
      let* u = send_raw (sel (unlin s)) t in
      IO.return (lens_put idx1 lmid (), {__lin=u})
    )}

  let receive idx sel = {__m=(fun lpre ->
      let s = lens_get idx lpre in
      let* var = receive_raw (sel (unlin s)) in
      IO.return (lens_put idx lpre (), {__lin=var})
    )}

  let close idx = {__m=(fun lpre ->
      let s = lens_get idx lpre in
      let* () = close_raw (unlin s) in
      IO.return (lens_put idx lpre (), {data=()})
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
    let s0 = Other ((fun x -> x), (fun _ x -> x))

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

    let send sel v = {__m=(fun lpre ->
        let* s = send_raw (sel (unlin lpre)) {data=v} in
        IO.return ((), {__lin=s})
      )}

    let deleg_send sel = {__m=(fun lpre ->
        let subj, obj = lpre in
        let* ep = send_raw (sel (unlin subj)) obj in
        IO.return ((), {__lin=ep})
      )}

    let receive sel = {__m=(fun lpre ->
        let* var = receive_raw (sel (unlin lpre)) in
        IO.return ((), {__lin=var})
      )}

    let close = {__m=(fun lpre ->
        let* () = close_raw (unlin lpre) in
        IO.return ((), {data=()})
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
end

include Static

let linret f = {Linocaml.__m=(fun pre -> IO.return (pre, {Linocaml.__lin=f ()}))}

let raw_gen = gen
(* let raw_gen_ipc = gen_ipc *)
let raw_gen_mult = gen_mult
(* let raw_gen_mult_ipc = gen_mult_ipc *)
let raw_gen_with_kinds = gen_with_kinds
let raw_gen_with_kinds_mult = gen_with_kinds_mult

let gen_raw g = gen g

let gen g = linret (fun () -> gen g)

(* let gen_ipc g = linret (fun () -> gen_ipc g) *)

let gen_mult ps g = linret (fun () -> gen_mult ps g)

(* let gen_mult_ipc ps g = linret (fun () -> gen_mult_ipc ps g) *)

let gen_with_kinds ps g = linret (fun () -> gen_with_kinds ps g)

let gen_with_kinds_mult ps g = linret (fun () -> gen_with_kinds_mult ps g)

let degen : (([`cons of close one * 't] as 't) tup lin, unit, unit data) Linocaml.monad =
  {Linocaml.__m=(fun _ -> IO.return ((), {Linocaml.data=()}))}

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
