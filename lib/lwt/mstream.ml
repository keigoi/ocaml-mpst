(* An lwt stream that is 
 * (1) optimized for unbounded buffering
 * (2) mergeable on both input and output send
 *)
type 't st = {
    stream : 't Lwt_stream_opt.t;
    mutable out_refs : 't out_refs;
    mutable inp_refs : 't inp_refs
  }
and 't out_refs = 't wrap_out list
and 't inp_refs = 't wrap_inp list
and _ wrap_out = WrapOut : 'u out * ('u -> 't) -> 't wrap_out
and _ wrap_inp = WrapInp : 'u inp * ('t -> 'u option) -> 't wrap_inp
and 't out = 't out_ ref
and 't inp = 't inp_ ref
and 't out_ =
  Out_ : 'u st * ('t -> 'u) -> 't out_
and 't inp_ =
  Inp_ : 'u st * ('u -> 't option) -> 't inp_
(* and 't out_ =
 *   Out_ : 'u st * ('t -> 'u) * ('t out * ('t -> 'u)) list -> 't out_
 * and 't inp_ =
 *   Inp_ : 'u st * ('u -> 't option) * ('t inp * ('u -> 't option)) list -> 't inp_ *)

let join_option f m0 =
  match m0 with
  | Some x -> f x
  | None -> None
let map_option f m0 =
  match m0 with
  | Some x -> Some (f x)
  | None -> None
let orelse f g x =
  match f x with
  | None -> g x
  | x-> x
let compose_opt f g x =
  match g x with
  | Some x -> f x
  | None -> None

let[@inline] debug str f x =
  (* print_endline str; *)
  f x

let create_raw () =
  let t = {
      stream = Lwt_stream_opt.create ();
      out_refs = [];
      inp_refs = [];
    }
  in
  t

let update_out_ref : type t u. u st -> (t -> u) -> t wrap_out -> u wrap_out =
  fun st f (WrapOut(r,out)) ->
    r := Out_(st, (fun x -> f (out x)));
    WrapOut(r,(fun x -> f (out x)))

let update_inp_ref : type t u. u st -> (u -> t option) -> t wrap_inp -> u wrap_inp =
  fun st g (WrapInp(r,in_)) ->
  r := Inp_(st, (fun x -> join_option in_ (g x)));
  WrapInp(r,(fun x -> join_option in_ (g x)))

(* auxiliary data types for "internal protocol" *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) prod = FstOnly of 'a | SndOnly of 'b | Both of 'a * 'b

let in_left x = Left(x)
let in_right x = Right(x)
let in_fstonly x = FstOnly(x)
let in_sndonly x = SndOnly(x)
let fst_prod = function
  | FstOnly(x) -> Some x
  | SndOnly(_) -> None
  | Both(x,_) -> Some x
let snd_prod = function
  | FstOnly(_) -> None
  | SndOnly(x) -> Some x
  | Both(_,x) -> Some x
(* these are __dangerous__, as None blocks inputs. *)
let from_left = function Left x -> Some x | _ -> None
let from_right = function Right x -> Some x | _ -> None

let merge_st : type t. t st -> t st -> t st = fun sl sr ->
  let st = create_raw () in
  st.out_refs <- List.map (update_out_ref st (fun x -> x)) (sl.out_refs @ sr.out_refs);
  st.inp_refs <- List.map (update_inp_ref st (fun x -> Some x)) (sl.inp_refs @ sr.inp_refs);
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  st

let merge_st_either : type t u. t st -> u st -> (t,u) either st = fun sl sr ->
  let st : (_,_) either st = create_raw () in
  (* merge in/out references pointing to sl/sr *)
  (* and update each reference to point to this stream  *)
  st.out_refs <-
    List.map (update_out_ref st in_left) sl.out_refs @ List.map (update_out_ref st in_right) sr.out_refs;
  st.inp_refs <-
    List.map (update_inp_ref st from_left) sl.inp_refs
    @ List.map (update_inp_ref st from_right) sr.inp_refs;
  (* below is not necessary. here, this just explicitly discards sl and sr *)
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  st

(* Merges two streams into one. Let lr = merge ll rr, then:
   (1) Writing x to the left delivers FstOnly(x). 
       It can also consumed by receiving on ll.
   (2) Similarly, writing on the right delivers FstOnly(x) and
       it disappears by receiving on rr.
   (3) Writing Both(x,y) can be consumed by one of ll, rr or lr.
   (4) Message order is preserved. 
       If the queue head is FstOnly, rr blocks.
       Similarly, SndOnly will block ll.
 *)
let merge_st_prod : type t u. t st -> u st -> (t,u) prod st = fun sl sr ->
  let st : (_,_) prod st = create_raw () in
  st.out_refs <-
    List.map (update_out_ref st in_fstonly) sl.out_refs @
    List.map (update_out_ref st in_sndonly) sr.out_refs;
  st.inp_refs <-
    List.map (update_inp_ref st fst_prod) sl.inp_refs @
    List.map (update_inp_ref st snd_prod) sr.inp_refs;
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  st
  
(** Merge two streams into one, via input endpoint  *)
let merge_inp : type t. t inp -> t inp -> t inp = fun ll rr ->
  match !ll,!rr with
  | Inp_(sl,fl),Inp_(sr,fr) ->
     let flr = (function Left x -> fl x | Right x -> fr x) in
     let st : (_,_) either st = create_raw () in
     (* merge in/out references pointing to sl/sr *)
     (* and update each reference to point to this stream  *)
     st.out_refs <-
       List.map (update_out_ref st in_left) sl.out_refs @ List.map (update_out_ref st in_right) sr.out_refs;
     st.inp_refs <-
       List.map (update_inp_ref st from_left) sl.inp_refs
       @ List.map (update_inp_ref st from_right) sr.inp_refs;
     (* below is not necessary. here, this just explicitly discards sl and sr *)
     sl.inp_refs <- [];
     sl.out_refs <- [];
     sr.inp_refs <- [];
     sr.out_refs <- [];
     let i = Inp_(st, flr) in
     ll := i; rr := i;
     ll

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out -> t out -> t out = fun ll rr ->
  let Out_(sl,fl),Out_(sr,fr) = !ll, !rr in
  (* The underlying protocol is ('a,'b) prod which conveys:
     (1) FstOnly(x) delivers x to ll only,
     (2) SndOnly(x) delivers x to rr only, or
     (3) Both(x,y) delivers to ll __or__ rr (earlier one).
   *)
  let st = merge_st_prod sl sr in
  let o = Out_(st,(fun x -> Both(fl x, fr x))) in
  ll := o;
  rr := o;
  ll

let wrap_inp {contents=i} g =
  let g = (fun x -> Some (g x)) in
  match i with
  | Inp_(st,f) ->
     let r = ref (Inp_(st, compose_opt g f)) in
     st.inp_refs <- WrapInp(r, compose_opt g f) :: st.inp_refs;
     r

let create_with ~wrap =
  let st1 = create_raw () in
  let out = ref (Out_(st1,wrap)) in
  let inp = ref (Inp_(st1,fun x -> Some x)) in
  st1.out_refs <- [WrapOut(out, wrap)];
  st1.inp_refs <- [WrapInp(inp, (fun x -> Some x))];
  out, inp

let create () =
  create_with ~wrap:(fun x -> x)

let send {contents=Out_(st,f)} v =
  Lwt_stream_opt.send st.stream (f v)

let receive {contents=i} =
  match i with
  | Inp_(st,f) ->
     Lwt_stream_opt.receive_wrap ~f st.stream
