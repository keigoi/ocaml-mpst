(* open Rows
open State

(* let out role lab name s =
   Deterministic
     ( StateHash.make_key (),
       Lazy.from_val
         {
           head = role.make_obj @@ lab.make_obj @@ Out (name, Lazy.from_val s);
           determinise_list = OutMerge.out_determinise role lab;
           force_determinised = OutMerge.out_force role lab;
           to_string = OutMerge.out_to_string role lab;
         } ) *)

(* let inp role constr name s =
   Deterministic
     ( StateHash.make_key (),
       Lazy.from_val
         {
           head = role.make_obj @@ lazy [ ExternalChoiceItem (constr, name, s) ];
           determinise_list = InpMerge.inp_determinise role;
           force_determinised = InpMerge.inp_force role;
           to_string = InpMerge.inp_to_string role;
         } ) *)

type ('var, 't) open_constr = {
  match_var : 'var -> 't option;
  make_var : 't -> 'var;
}

let eq_constr :
      'h 'k1 'k2.
      ('h, 'k1) open_constr -> ('h, 'k2) open_constr -> 'k1 -> 'k2 option =
 fun cstr1 cstr2 k1 -> cstr2.match_var (cstr1.make_var k1)

(* let eq_method : 'h 'k1 'k2. ('h, 'k1) method_ -> ('h, 'k2) method_ -> 'k1 -> 'k2 option =
   fun meth1 meth2 k1 ->
     meth2.method_call (meth1.make_obj k1) *)

type 'h ext_choice =
  | ExtChoice : (unit Name.t * ('h, 'k) method_ * 'k runner) -> 'h ext_choice

and 'h int_choice =
  | IntChoice : (unit Name.t * ('h, 'k) constr * 'k runner) -> 'h int_choice

and 'h runner =
  | IntC : 'h int_choice list -> 'h runner
  | ExtC : 'h ext_choice list -> 'h runner
  | Out : ('v Name.t * 'k runner) -> ('v * 'k) runner
  | Inp : ('v Name.t * 'k runner) -> ('v -> 'k) runner
  | End : unit runner

let rec merge_runner : type a. a runner -> a runner -> a runner =
 fun sl sr ->
  match (sl, sr) with
  | IntC xs, IntC ys -> IntC (xs @ ys)
  | ExtC xs, ExtC ys -> ExtC (xs @ ys)
  | Out (n1, k1), Out (n2, k2) ->
      Name.unify n1 n2;
      Out (n1, merge_runner k1 k2)
  | Inp (n1, k1), Inp (n2, k2) ->
      Name.unify n1 n2;
      Inp (n1, merge_runner k1 k2)
  | End, End -> End
  | _ -> assert false

type running = Running : ('h runner * 'h) -> running

let rec run : running -> unit =
 fun (Running (runner, handler)) ->
  match (runner, handler) with
  | Out (n, rk), (v, hk) ->
      Event.sync (Event.send (Name.finalise n) v);
      run (Running (rk, hk))
  | Inp (n, rk), f ->
      run (Running (rk, f (Event.sync (Event.receive (Name.finalise n)))))
  | ExtC rs, h ->
      let names =
        rs
        |> List.map (fun (ExtChoice (n, meth, _)) ->
               Event.wrap
                 (Event.receive (Name.finalise n))
                 (fun () -> meth.method_name))
      in
      let name = Event.sync (Event.choose names) in
      let next =
        List.filter_map
          (fun (ExtChoice (_, meth, rk)) ->
            if meth.method_name = name then Some (Running (rk, meth.call_obj h))
            else None)
          rs
      in
      List.iter run next
  | End, () -> ()
  | IntC _, _ -> failwith "TODO"

and run_ext_choice : type h. h ext_choice -> h -> unit =
 fun (ExtChoice (_, meth, rk)) h -> run (Running (rk, meth.call_obj h))

module Seq = struct
  type _ t =
    | SeqCons : 'hd runner * 'tl t -> [ `cons of 'hd * 'tl ] t
    | SeqNil : ([ `cons of unit * 'a ] as 'a) t

  let head : type hd tl. [ `cons of hd * tl ] t -> hd runner = function
    | SeqCons (hd, _) -> hd
    | SeqNil -> End

  let tail : type hd tl. [ `cons of hd * tl ] t -> tl t = function
    | SeqCons (_, tl) -> tl
    | SeqNil -> SeqNil

  let rec get : type a b xs ys. (a, b, xs, ys) Hlist.idx -> xs t -> a runner =
   fun ln xs -> match ln with Zero -> head xs | Succ ln' -> get ln' (tail xs)

  let rec put :
      type a b xs ys. (a, b, xs, ys) Hlist.idx -> xs t -> b runner -> ys t =
   fun ln xs b ->
    match ln with
    | Zero -> SeqCons (b, tail xs)
    | Succ ln' -> SeqCons (head xs, put ln' (tail xs) b)

  let rec merge : type x. x t -> x t -> x t =
   fun l r ->
    match (l, r) with
    | SeqCons (_, _), _ ->
        let hd = merge_runner (head l) (head r) in
        let tl = merge (tail l) (tail r) in
        SeqCons (hd, tl)
    | _, SeqCons (_, _) -> merge r l
    | SeqNil, SeqNil -> SeqNil
end
(* 
let ( --> ) a b lab g =
  let name = Name.make () in
  let bcont = Seq.get b.role_index g in
  let g = Seq.put b.role_index g (Inp (name, lab.obj, bcont)) in
  let acont = Seq.get a.role_index g in
  let g = Seq.put a.role_index g (Out (name, lab.var, acont)) in
  g

let choice_at a disj (aL, gL) (aR, gR) =
  let acontL = Seq.get aL.role_index gL in
  let acontR = Seq.get aR.role_index gR in
  let acont = disj.disj_concat acontL acontR in
  let gL = Seq.put aL.role_index gL End in
  let gR = Seq.put aL.role_index gR End in
  let g = Seq.merge gL gR in
  let g = Seq.put a.role_index acont in
  g *)

(* let receive_m ch cont = Step ((fun h -> h#m), cont) *)

(* type 'h runner = | Step : (('h -> 'k) * 'k runner) -> 'h runner | End : unit
   runner *)

(* let receive_m ch cont = Step ((fun h -> h#m), cont) *)


type 'h runner =
  | Step : (('h -> 'k Event.event) * 'k runner) -> 'h runner
  | End : unit runner

let receive_m ch cont =
  Step ((fun h -> Event.wrap (Event.receive ch) (fun x -> h#m x)), cont)

let send_m ch (f : [ `m of 't ] -> 't) cont =
  Step
    ( (fun h ->
        match f h with v, k -> Event.wrap (Event.send ch v) (fun () -> k)),
      cont )

let ( --> ) a b msg cont =
  let ch = Event.new_channel () in
  let sb : _ local = Seq.get b.role_index cont in
  let sb =
    {
      run =
        (fun h ->
          let obj = a.role_label.match_var h in
          let v = Chan.recv ch in
          ());
    }
  in
  cont

let f g1 g2 h = if true then ignore (g1 h) else ignore (g2 h)
let g x = f (fun h -> h#m1) (fun h -> h#m2) x *)
