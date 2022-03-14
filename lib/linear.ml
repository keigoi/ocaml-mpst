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
              let v, ch = receive ch in
              Server.close (Server.send (ch, v) ch'))
        in
        let ch_v =
          lazy
            (let (ch, v), ch' = receive ch' in
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
            (let ch, ch' = receive ch' in
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
(*
   let f idx idx2 seq (constr, constr2) =
     let open Rows in
     let open LSeq in
     (* sender *)
     let ch' =
       fork (fun ch' ->
           let ch = Lazy.force @@ seq_get idx seq in
           let var = offer ch in
           let ch =
             match constr.match_var var with Some v -> v | None -> assert false
           in
           Server.close (Server.send ch ch'))
     in
     let ch_lazy =
       lazy
         (let ch, ch' = receive ch' in
          close ch';
          ch)
     in
     let ch = lazy (Lazy.force ch_lazy) in
     let ch2 = fork (fun ch2 -> ()) in
     let v = lazy (ignore (Lazy.force ch_lazy); constr2.make_var) in
     let seq = seq_put Zero (seq_put idx seq ch) v in
     (* receiver *)
     let ch' =
       fork (fun ch' ->
           let v = Lazy.force @@ seq_get Zero seq in
           let ch = Lazy.force @@ seq_get idx2 seq in
           let ch = select constr2.make_var ch in
           let ch = send v ch in
           Server.close (Server.send ch ch'))
     in
     let ch =
       lazy
         (let ch, ch' = receive ch' in
          close ch';
          ch)
     in
     let seq = seq_put idx2 seq ch in
     let _seq = seq_put Zero seq (Lazy.from_val ()) in
     ()

   let f ch2 =
     let ch3 = fork (fun ch3 -> Server.send_and_forward () ch2 ch3) in
     let (), ch3 = receive ch3 in
     ch3 *)

let f ch1 ch2 ch3 ch4 =
  let ch =
    fork
      Server.(
        fun ch ->
          match offer (Lazy.force ch1) with
          | `l ch1 ->
              let ch2 = select (fun x -> `l x) (Lazy.force ch2) in
              close (send (`l ch1, `l ch2) ch)
          | `r ch1 ->
              let ch2 = select (fun x -> `r x) (Lazy.force ch2) in
              close (send (`r ch1, `r ch2) ch))
  in
  let ch1_ch2 =
    lazy
      (let ret, ch = receive ch in
       close ch;
       ret)
  in
  let ch1 = lazy (fst @@ Lazy.force ch1_ch2) in
  let ch2 = lazy (snd @@ Lazy.force ch1_ch2) in
  let ch =
    fork
      Server.(
        fun ch ->
          match offer (Lazy.force ch3) with
          | `m ch3 -> (
              match Lazy.force ch2 with
              | `l ch2 ->
                  let ch2 = select (fun x -> `m x) ch2 in
                  close (send (`l ch2, ch3) ch)
              | `r ch2 ->
                  let ch2 = select (fun x -> `m x) ch2 in
                  close (send (`r ch2, ch3) ch)))
  in
  let ch2_ch3 =
    lazy
      (let ret, ch = receive ch in
       close ch;
       ret)
  in
  let ch2 = lazy (fst @@ Lazy.force ch1_ch2) in
  let ch3 = lazy (snd @@ Lazy.force ch1_ch2) in
  let ch =
    fork
      Server.(
        fun ch ->
          match (Lazy.force ch1, Lazy.force ch2) with
          | `l ch1, `l ch2 ->
              let (`l1 ch1) = offer ch1 in
              let ch2 = select (fun x -> `l1 x) ch2 in
              close (send (`l ch1, `l ch2) ch)
          | `r ch1, `r ch2 ->
              let ch2 = select (fun x -> `r2 x) ch2 in
              close (send (`r ch1, `r ch2) ch)
          | _ -> assert false)
  in
  ()
