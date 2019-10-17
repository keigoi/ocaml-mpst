(* An lwt stream that is 
 * (1) optimized for unbounded buffering
 * (2) mergeable on both input and output send
 *)
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

type 't st = {
    mutable push_signal : unit Lwt.t;
    mutable push_signal_resolver : unit Lwt.u;
    mutable push_waiting : bool;
    mutable node : 't node;
    last: 't node ref;
    (* output and input references pointing to this queue*)
    mutable out_refs : 't out_refs;
    mutable inp_refs : 't inp_refs
  }
and 't node = {
    mutable next : 't node;
    mutable data : 't option
  }
and -'t out = Out : 'u st * ('t -> 'u) -> 't out
and +'t inp = Inp : 'u st * ('u -> 't option) -> 't inp
and 't out_ref = 't out ref
and 't inp_ref = 't inp ref
and 't out_refs = 't wrap_out list
and 't inp_refs = 't wrap_inp list
and _ wrap_out = WrapOut : 'u out_ref * ('u -> 't) -> 't wrap_out
and _ wrap_inp = WrapInp : 'u inp_ref * ('t -> 'u option) -> 't wrap_inp

let new_node () =
  let rec node = { next = node; data = None } in
  node

let create_raw () =
  let push_signal, push_signal_resolver = Lwt.wait () in
  let last = new_node () in
  let rec t = {
      push_signal;
      push_signal_resolver;
      push_waiting=false;
      node = last;
      last = ref last;
      out_refs = [];
      inp_refs = [];
    }
  in
  t

let enqueue' e last =
  let node = !last
  and new_last = new_node () in
  node.data <- e;
  node.next <- new_last;
  last := new_last

let send_raw t v =
  begin match t.node.data with
  | None ->
     t.node.data <- Some v
  | _ ->
     enqueue' (Some v) t.last
  end;
  if t.push_waiting then begin
      print_endline "found waitor";
      t.push_waiting <- false;
      let old_push_signal_resolver = t.push_signal_resolver in
      let new_waiter, new_push_signal_resolver = Lwt.wait () in
      t.push_signal <- new_waiter;
      t.push_signal_resolver <- new_push_signal_resolver;
      Lwt.wakeup_later old_push_signal_resolver ()
    end;
  Lwt.return_unit

let rec next_rec ~f t =
  let open Lwt in
  match t.node.data with
  | None ->
     print_endline "None";
     t.push_waiting <- true;
     Lwt.on_cancel t.push_signal (fun _ -> print_endline "cancelled");
     Lwt.protected t.push_signal >>= fun () ->
     next_rec ~f t
     (* begin match t.node.data with
      * | None ->
      *    print_endline "fail";
      *    assert false
      * | Some x ->
      *    print_endline "ok";
      *    Lwt.return x
      * end *)
  | Some x ->
     match f x with
     | Some x -> 
        print_endline "Received";
        t.node.data <- None;
        t.node <- t.node.next;
        Lwt.return x
     | None ->
        print_endline "Retry";
        next_rec ~f t

let receive_raw ~f t =
  next_rec ~f t

let rec assign_out_ref : type t u. t out_refs -> (t -> u) -> u st -> unit = fun os f st ->
  match os with
  | [] ->
     ()
  | WrapOut (r,f0) :: rs ->
     r := Out(st, (fun x -> f (f0 x)));
     assign_out_ref rs f st;
     ()

let rec assign_inp_ref : type t u. t inp_refs -> (u -> t option) -> u st -> unit = fun rs f st ->
  match rs with
  | [] ->
     ()
  | WrapInp (r,g0) :: rs ->
     r := Inp(st, (fun x -> join_option g0 (f x)));
     assign_inp_ref rs f st;
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
let merge_inp : type t. t inp_ref -> t inp_ref -> t inp_ref = fun ll rr ->
  let Inp(sl,fl),Inp(sr,fr) = !ll, !rr in
  (* Creating the underlying stream. 
     The "protocol" is that, the output values are transformed to
     Left(x)/Right(y) (for ll/rr respectively) then enqueued to the stream,
     and are pattern-matched and distributed on input side. *)
  let st : (_,_) either st = create_raw () in
  (* merge in/out references pointed by l/r *)
  st.out_refs <-
    List.map (wrap_out in_left) sl.out_refs @
    List.map (wrap_out in_right) sr.out_refs;
  st.inp_refs <-
    List.map (wrap_inp from_left) sl.inp_refs @
    List.map (wrap_inp from_right) sr.inp_refs;
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  (* update each reference to point to this stream  *)
  assign_inp_ref st.inp_refs (fun x -> Some x) st;
  assign_out_ref st.out_refs (fun x -> x) st;
  (* update l and r *)
  let i = Inp(st, orelse (compose_opt fl from_left) (compose_opt fr from_right)) in
  ll := i;
  rr := i;
  ll

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out_ref -> t out_ref -> t out_ref = fun ll rr ->
  let Out(sl,fl),Out(sr,fr) = !ll, !rr in
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
  assign_out_ref st.out_refs (fun x -> x) st;
  assign_inp_ref st.inp_refs (fun x -> Some x) st;
  let o = Out(st,(fun x -> Both(fl x, fr x))) in
  ll := o;
  rr := o;
  ll

let get_out : 'a. 'a out_ref -> 'a out = fun o -> !o
let get_inp : 'a. 'a inp_ref -> 'a inp = fun i -> !i

let send : type t u. t out -> t -> unit Lwt.t = fun (Out(st,f)) v ->
  send_raw st (f v)

let receive : type t u. t inp -> t Lwt.t = fun (Inp(st,f)) ->
  receive_raw ~f st

let create_with ~wrap_inp =
  let st1 = create_raw () in
  let out_ref = ref (Out(st1,(fun x -> x))) in
  let inp_ref = ref (Inp(st1,(fun x -> Some(wrap_inp(x))))) in
  st1.out_refs <- [WrapOut(out_ref, (fun x -> x))];
  st1.inp_refs <- [WrapInp(inp_ref, (fun x -> Some(wrap_inp(x))))];
  out_ref, inp_ref
