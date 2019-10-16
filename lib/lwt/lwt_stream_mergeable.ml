(* An lwt stream that is 
 * (1) optimized for unbounded buffering
 * (2) mergeable on both input and output send
 *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) prod = FstOnly of 'a | SndOnly of 'b | Both of 'a * 'b

let fst_option = function Some(FstOnly(x)|Both(x,_)) -> Some x | _ -> None
let snd_option = function Some(SndOnly(x)|Both(_,x)) -> Some x | _ -> None
let from_left_opt = function Some(Left x) -> Some x | _ -> None
let from_right_opt = function Some(Right x) -> Some x | _ -> None
let is_some = function Some _ -> true | _ -> false

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
and 't out_refs =
  | NoOut : 't out_refs
  | OneOut : 't out_ref -> 't out_refs
  | BinOutEither : 'l out_refs * 'r out_refs -> ('l, 'r) either out_refs
  | BinOutProd : 'l out_refs * 'r out_refs -> ('l, 'r) prod out_refs
and _ inp_refs =
  | NoInp : 't inp_refs
  | OneInp : 't inp_ref -> 't inp_refs
  | BinInpProd : 'l inp_refs * 'r inp_refs -> ('l, 'r) prod inp_refs
  | BinInpEither : 'l inp_refs * 'r inp_refs -> ('l, 'r) either inp_refs

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
      out_refs = NoOut;
      inp_refs = NoInp;
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

let receive_raw t =
  next_rec t

let rec assign_out_ref : type t u. t out_refs -> (t -> u) -> u st -> unit = fun os f st ->
  match os with
  | NoOut -> ()
  | OneOut(r) ->
     r := Out(st,f)
  | BinOutEither(l,r) ->
     assign_out_ref l (fun x -> f (Left x)) st;
     assign_out_ref r (fun x -> f (Right x)) st
  | BinOutProd(l,r) ->
     assign_out_ref l (fun x -> f (FstOnly(x))) st;
     assign_out_ref r (fun x -> f (SndOnly(x))) st

let rec assign_inp_ref : type t u. t inp_refs -> (u -> t option) -> u st -> unit = fun os f st ->
  match os with
  | NoInp -> ()
  | OneInp (r) ->
     r := Inp(st,f)
  | BinInpProd(l,r) ->
     assign_inp_ref l (fun x -> fst_option (f x)) st;
     assign_inp_ref r (fun x -> snd_option (f x)) st;
     ()
  | BinInpEither(l,r) ->
     assign_inp_ref l (fun x -> from_left_opt (f x)) st;
     assign_inp_ref r (fun x -> from_right_opt (f x)) st;
     ()

let merge_inp : type t. t inp_ref -> t inp_ref -> t inp_ref = fun ({contents=(Inp(st1,f1))} as l) ({contents=(Inp(st2,f2))} as r) ->
  let st = create_raw () in
  st.out_refs <- BinOutEither (st1.out_refs, st2.out_refs);
  st.inp_refs <- BinInpEither (st1.inp_refs, st2.inp_refs);
  assign_inp_ref st.inp_refs (fun x -> Some x) st;
  assign_out_ref st.out_refs (fun x -> x) st;
  let i = Inp(st, (function Left x -> f1 x | Right x -> f2 x)) in
  l := i;
  r := i;
  l

let merge_out : type t. t out_ref -> t out_ref -> t out_ref = fun ll rr ->
  let Out(sl,fl),Out(sr,fr) = !ll, !rr in
  let st = create_raw () in
  st.inp_refs <- BinInpProd(sl.inp_refs, sr.inp_refs);
  st.out_refs <- BinOutProd(sl.out_refs, sr.out_refs);
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

let create () =
  let st0 = create_raw () in
  let st1 = create_raw () in
  let out_ref = ref (Out(st1,(fun x -> x))) in
  let inp_ref = ref (Inp(st1,(fun x -> x))) in
  st1.out_refs <- OneOut(out_ref);
  st0.inp_refs <- OneInp(inp_ref);
  out_ref, inp_ref
