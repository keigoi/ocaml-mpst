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
  Out_ : 'u st * ('t -> 'u) * 't out list -> 't out_
and 't inp_ =
  Inp_ : 'u st * ('u -> 't option) * 't inp list -> 't inp_

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
let compose f g x = f (g x)
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
  fun st f (WrapOut({contents=Out_(_,_,rs)} as r,out)) ->
    let o = Out_(st, (fun x -> f (out x)), rs) in
    List.iter (fun r -> r := o) rs;
    r := o;
    WrapOut(r,(fun x -> f (out x)))

let update_inp_ref : type t u. u st -> (u -> t option) -> t wrap_inp -> u wrap_inp =
  fun st g (WrapInp({contents=Inp_(_,_,rs)} as r,in_)) ->
  let i = Inp_(st, (fun x -> join_option in_ (g x)), rs) in
  List.iter (fun r -> r := i) rs;
  r := i;
  WrapInp(r,(fun x -> join_option in_ (g x)))

(* auxiliary data types for "internal protocol" *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) prod = FstOnly of 'a | SndOnly of 'b | Both of 'a * 'b

let in_left x = Left(x)
let in_right x = Right(x)
let fst_prod = function
  | FstOnly(x) -> Some x
  | SndOnly(_) -> None
  | Both(x,_) -> Some x
let snd_prod = function
  | FstOnly(_) -> None
  | SndOnly(x) -> Some x
  | Both(_,x) -> Some x
(* these functions are __dangerous__, as they result in blocking inputs (`None`). *)
let in_fstonly x = FstOnly(x)
let in_sndonly x = SndOnly(x)
let from_left = function Left x -> Some x | _ -> None
let from_right = function Right x -> Some x | _ -> None

(* let merge_st : type t. t st -> t st -> t st = fun sl sr ->
 *   let st = create_raw () in
 *   st.out_refs <- List.map (update_out_ref st (fun x -> x)) (sl.out_refs @ sr.out_refs);
 *   st.inp_refs <- List.map (update_inp_ref st (fun x -> Some x)) (sl.inp_refs @ sr.inp_refs);
 *   sl.inp_refs <- [];
 *   sl.out_refs <- [];
 *   sr.inp_refs <- [];
 *   sr.out_refs <- [];
 *   st *)

let merge_st_either : type t u. t st -> u st -> (t,u) either st = fun sl sr ->
  let st : (_,_) either st = create_raw () in
  (* merge in/out references pointing to sl/sr *)
  (* and update each reference to point to this stream  *)
  st.out_refs <-
    List.map (update_out_ref st in_left) sl.out_refs @ List.map (update_out_ref st in_right) sr.out_refs;
  (* this part is potentially dangerous, but actually safe for our purpose 
     since these are anyway overwritten later by merge_inp *)
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
  st.out_refs <- (* possibly dangerous, but actually safe (see merge_inp) *)
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
  | Inp_(sl,fl,ls),Inp_(sr,fr,rs) ->
     (* The following wrongly wires ll and rr, as Left(x) and Right(x) are 
        delivered only to ll or rr respectively. 
        (i.e. other side will block.)
        This is fixed right after this.
      *)
     let st : (_,_) either st = merge_st_either sl sr in
     let flr = function Left x -> fl x|Right x -> fr x in
     (* Gather all `t inp`s *)
     let is = ll::rr::(List.append ls rs) in
     (* and update to the desired ones *)
     let i = Inp_(st, flr, is) in
     List.iter (fun r0 -> r0 := i) is;
     ll

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out -> t out -> t out = fun ll rr ->
  match !ll,!rr with
  | Out_(sl,fl,ls),Out_(sr,fr,rs) ->
     (* The underlying protocol is ('a,'b) prod which conveys:
        (1) FstOnly(x) delivers x to ll only,
        (2) SndOnly(x) delivers x to rr only, or
        (3) Both(x,y) delivers to ll __or__ rr (earlier one).
        Here, (1) and (2) are __NOT__ what we want, as both ll and rr shares 
        the same payload type `t`, they should treated equally.
        Similar to merge_inp, this is fixed right after this:
      *)
     let st : (_,_) prod st = merge_st_prod sl sr in
     let flr = fun x -> Both(fl x, fr x) in
     (* Here, we gather all `t out`s in one place *)
     let os = ll::rr::(List.append ls rs) in
     (* and put the correct wrapper which delivers `Both`. *)
     let o = Out_(st, flr, os) in
     List.iter (fun r0 -> r0 := o) os;
     ll

let wrap_inp {contents=i} g =
  let g = (fun x -> Some (g x)) in
  match i with
  | Inp_(st,f,rs) ->
     let r = ref (Inp_(st, compose_opt g f, [])) in
     st.inp_refs <- WrapInp(r, compose_opt g f) :: st.inp_refs;
     r

let create_with ~wrap =
  let st1 = create_raw () in
  let out = ref (Out_(st1,wrap,[])) in
  let inp = ref (Inp_(st1,(fun x -> Some x),[])) in
  st1.out_refs <- [WrapOut(out, wrap)];
  st1.inp_refs <- [WrapInp(inp, (fun x -> Some x))];
  out, inp

let create () =
  create_with ~wrap:(fun x -> x)

let send {contents=Out_(st,f,_)} v =
  Lwt_stream_opt.send st.stream (f v)

let receive {contents=Inp_(st,f,_)} =
  Lwt_stream_opt.receive_wrap ~f st.stream
