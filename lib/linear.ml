(* open BasicCombinators *)
open Session_ocaml.NonPolar

type _ inp
type _ out

module LSeq = Hlist.Make (struct
  type 'a t = 'a lazy_t

  let unit = Lazy.from_val ()
end)

type 't global =
  | Finish : ([ `cons of unit * 'a ] as 'a) global
  | ForkOffer :
      ([ `branch of 'var ] cli, 'v cli, 'x, 'y) Hlist.idx
      * ('var, [ `recv of 'a * 'v ] cli) Rows.constr
      * ([ `cons of 'a * 'c ] as 'y) global
      -> ([ `cons of unit * 'd ] as 'x) global
  | ForkSelect :
      ([ `select of 'var ] cli, 'v cli, 'x, 'y) Hlist.idx
      * ('var, [ `send of 'a * 'v ] srv) Rows.constr
      * ([ `cons of unit * 'c ] as 'y) global
      -> ([ `cons of 'a * 'd ] as 'x) global
(* | ForkOfferMany :
    ([ `branch of 'var ] cli, unit, 'x, 'y) Hlist.idx
    * ('var, 'var1, 'var2) Rows.disj
    * (unit, [ `branch of 'var1 ] cli, 'y, 'y1) Hlist.idx
    * ([ `cons of 'a1 * 'c1 ] as 'y1) global
    * (unit, [ `branch of 'var2 ] cli, 'y, 'y2) Hlist.idx
    * ([ `cons of 'a2 * 'c2 ] as 'y2) global
    -> ([ `cons of unit * 'd ] as 'x) global *)

let rec go : type t. t LSeq.seq -> t global -> unit =
  let open LSeq in
  fun seq -> function
    | Finish -> ()
    | ForkOffer (idx, constr, cont) ->
        let ch' =
          fork (fun ch' ->
              let ch = Lazy.force @@ seq_get idx seq in
              let var = offer ch in
              let ch =
                match constr.match_var var with
                | Some v -> v
                | None -> assert false
              in
              let ch, v = receive ch in
              Server.close (Server.send (ch, v) ch'))
        in
        let ch_v =
          lazy
            (let ch', (ch, v) = receive ch' in
             close ch';
             (ch, v))
        in
        let ch = lazy (fst @@ Lazy.force ch_v) in
        let v = lazy (snd @@ Lazy.force ch_v) in
        let seq = seq_put Zero (seq_put idx seq ch) v in
        go seq cont
    | ForkSelect (idx, constr, cont) ->
        let ch' =
          fork (fun ch' ->
              let v = Lazy.force @@ seq_get Zero seq in
              let ch = Lazy.force @@ seq_get idx seq in
              let ch = select constr.make_var ch in
              let ch = send v ch in
              let ch' = Server.send ch ch' in
              Server.close ch')
        in
        let ch =
          lazy
            (let ch', ch = receive ch' in
             close ch';
             ch)
        in
        let seq = seq_put idx seq ch in
        let seq = seq_put Zero seq (Lazy.from_val ()) in
        go seq cont
(* | ForkOfferMany (idx, disj, idx1, cont1, idx2, cont2) ->
    let ch = seq_get idx seq and seq = seq_put idx seq (Lazy.from_val ()) in
    let ch' = fork (fun ch' -> let var = offer (Lazy.force ch) in
                               ()) in
    () *)
