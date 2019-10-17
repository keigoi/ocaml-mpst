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
and -'t out_ = Out_ : 'u st * ('t -> 'u) -> 't out_
and +'t inp_ = Inp_ : 'u st * ('u -> 't option) -> 't inp_

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

let rec assign_out : type t u. t out_refs -> (t -> u) -> u st -> unit = fun os f st ->
  match os with
  | [] ->
     ()
  | WrapOut (r,f0) :: rs ->
     r := Out_(st, (fun x -> f (f0 x)));
     assign_out rs f st;
     ()

let rec assign_inp : type t u. t inp_refs -> (u -> t option) -> u st -> unit = fun rs f st ->
  match rs with
  | [] ->
     ()
  | WrapInp (r,g0) :: rs ->
     r := Inp_(st, (fun x -> join_option g0 (f x)));
     assign_inp rs f st;
     ()

let wrap_out : type t u. (t -> u) -> t wrap_out -> u wrap_out =
  fun f (WrapOut(r,out)) ->
  WrapOut(r,(fun x -> f (out x)))

let wrap_inp : type t u. (u -> t option) -> t wrap_inp -> u wrap_inp =
  fun g (WrapInp(r,in_)) ->
  WrapInp(r,(fun x -> join_option in_ (g x)))

(* auxiliary data types for "internal protocol" *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) prod = FstOnly of 'a | SndOnly of 'b | Both of 'a * 'b

let fst_option = function Some(FstOnly(x)|Both(x,_)) -> Some x | _ -> None
let snd_option = function Some(SndOnly(x)|Both(_,x)) -> Some x | _ -> None
let in_left x = Left(x)
let in_right x = Right(x)
let from_left_opt = function Some(Left x) -> Some x | _ -> None
let from_right_opt = function Some(Right x) -> Some x | _ -> None
let from_left = function Left x -> Some x | _ -> None
let from_right = function Right x -> Some x | _ -> None
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

(** Merge two streams into one, via input endpoint  *)
let merge_inp : type t. t inp -> t inp -> t inp = fun ll rr ->
  let Inp_(sl,fl),Inp_(sr,fr) = !ll, !rr in
  (* Creating the underlying stream. 
     The "protocol" is that, the output values are transformed to
     Left(x)/Right(y) (for ll/rr respectively) then enqueued to the stream,
     and are pattern-matched and distributed on input side. *)
  let st : (_,_) either st = create_raw () in
  (* merge in/out references pointed by l/r *)
  st.out_refs <-
    List.map (wrap_out (debug "merge_inp in_left" @@ in_left)) sl.out_refs @
    List.map (wrap_out (debug "merge_inp in_right" @@ in_right)) sr.out_refs;
  st.inp_refs <-
    List.map (wrap_inp (debug "merge_inp from_left" @@ from_left)) sl.inp_refs @
    List.map (wrap_inp (debug "merge_inp from_right" @@ from_right)) sr.inp_refs;
  (* below is not necessary. here, this just explicitly discards sl and sr *)
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  (* update each reference to point to this stream  *)
  assign_inp st.inp_refs (fun x -> Some x) st;
  assign_out st.out_refs (fun x -> x) st;
  (* update l and r *)
  let i = Inp_(st, orelse (debug "merge_inp left" @@ compose_opt fl from_left) (debug "merge_inp right" @@ compose_opt fr from_right)) in
  ll := i;
  rr := i;
  ll

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out -> t out -> t out = fun ll rr ->
  let Out_(sl,fl),Out_(sr,fr) = !ll, !rr in
  (* The underlying protocol is ('a,'b) prod which conveys:
     (1) FstOnly(x) delivers x to ll only,
     (2) SndOnly(x) delivers x to rr only, or
     (3) Both(x,y) delivers to ll __or__ rr (earlier one).
   *)
  let st : (_,_) prod st = create_raw () in
  st.out_refs <-
    List.map (wrap_out in_fstonly) sl.out_refs @
    List.map (wrap_out in_sndonly) sr.out_refs;
  st.inp_refs <-
    List.map (wrap_inp fst_prod) sl.inp_refs @
    List.map (wrap_inp snd_prod) sr.inp_refs;
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  assign_out st.out_refs (fun x -> x) st;
  assign_inp st.inp_refs (fun x -> Some x) st;
  let o = Out_(st,(fun x -> Both(fl x, fr x))) in
  ll := o;
  rr := o;
  ll

let wrap_inp {contents=Inp_(st,f)} g =
  let g = (fun x -> Some (g x)) in
  let r = ref (Inp_(st, debug "wrap_inp 1" @@ compose_opt g f)) in
  st.inp_refs <- WrapInp(r,debug "wrap_inp 2" @@ compose_opt g f) :: st.inp_refs;
  r

let create_with ~wrap_inp =
  let st1 = create_raw () in
  let out = ref (Out_(st1,(fun x -> x))) in
  let inp = ref (Inp_(st1,(fun x -> Some(wrap_inp(x))))) in
  st1.out_refs <- [WrapOut(out, (fun x -> x))];
  st1.inp_refs <- [WrapInp(inp, (fun x -> Some(wrap_inp(x))))];
  out, inp

let create () =
  create_with ~wrap_inp:(fun x -> x)

let send {contents=Out_(st,f)} v =
  Lwt_stream_opt.send st.stream (f v)

let receive {contents=Inp_(st,f)} =
  Lwt_stream_opt.receive_wrap ~f st.stream
