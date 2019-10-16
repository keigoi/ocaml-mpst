(* An lwt stream that is 
 * (1) optimized for unbounded buffering
 * (2) mergeable on both input and output send
 *)
type ('a, 'b) either = Left of 'a | Right of 'b
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
and +'t inp = Inp : 'u st * ('u -> 't) -> 't inp
and 't out_ref = 't out ref
and 't inp_ref = 't inp ref
and 't out_refs =
  | NoOut : 't out_refs
  | OneOut : 't out_ref -> 't out_refs
  | BinOut : 'l out_refs * 'r out_refs -> ('l,'r) either out_refs
and _ inp_refs =
  | NoInp : 't inp_refs
  | OneInp : 't inp_ref -> 't inp_refs
  | BinInp : 'l inp_refs * 'r inp_refs -> ('l * 'r) inp_refs

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

let rec next_rec t =
  let open Lwt in
  match t.node.data with
  | None ->
     print_endline "None";
     t.push_waiting <- true;
     Lwt.on_cancel t.push_signal (fun _ -> print_endline "cancelled");
     Lwt.protected t.push_signal >>= fun () ->
     next_rec t
     (* begin match t.node.data with
      * | None ->
      *    print_endline "fail";
      *    assert false
      * | Some x ->
      *    print_endline "ok";
      *    Lwt.return x
      * end *)
  | Some x ->
     print_endline "Some";
     t.node.data <- None;
     t.node <- t.node.next;
     Lwt.return x

let receive_raw t =
  next_rec t

let rec assign_out_ref : type t u. t out_refs -> (t -> u) -> u st -> unit = fun os c st ->
  match os with
  | NoOut -> ()
  | OneOut({contents=Out(_,_)} as r) ->
     r := Out(st,c)
  | BinOut(l,r) ->
     assign_out_ref l (fun x -> c (Left x)) st;
     assign_out_ref r (fun x -> c (Right x)) st

let merge_inp : 'a. 'a inp_ref -> 'a inp_ref -> 'a inp_ref = fun l r ->
  let Inp(st1,f1), Inp(st2,f2) = !l, !r in
  let st = create_raw () in
  assign_out_ref st1.out_refs (fun x -> Left x) st;
  assign_out_ref st2.out_refs (fun x -> Right x) st;
  st.out_refs <- BinOut (st1.out_refs,st2.out_refs);
  let i = Inp(st, (function Left x -> f1 x | Right x -> f2 x)) in
  l := i;
  r := i;
  l

let rec assign_inp_ref : type t u. t inp_refs -> (u -> t) -> u st -> unit = fun os c st ->
  match os with
  | NoInp -> ()
  | OneInp ({contents=Inp(_,_)} as r) ->
     r := Inp(st,c)
  | BinInp(l,r) ->
     assign_inp_ref l (fun x -> fst (c x)) st;
     assign_inp_ref r (fun x -> snd (c x)) st;
     ()

let merge_out : type t. t out_ref -> t out_ref -> t out_ref = fun ll rr ->
  let Out(sl,fl),Out(sr,fr) = !ll, !rr in
  let st = create_raw () in
  assign_inp_ref sl.inp_refs (fun (x,_) -> x) st;
  assign_inp_ref sr.inp_refs (fun (_,x) -> x) st;
  st.inp_refs <- BinInp(sl.inp_refs, sr.inp_refs);
  let o = Out(st,(fun x -> (fl x, fr x))) in
  ll := o;
  rr := o;
  ll

let get_out : 'a. 'a out_ref -> 'a out = fun o -> !o
let get_inp : 'a. 'a inp_ref -> 'a inp = fun i -> !i

let send : type t u. t out -> t -> unit Lwt.t = fun (Out(st,f)) v ->
  send_raw st (f v)

let receive : type t u. t inp -> t Lwt.t = fun (Inp(st,f)) ->
  Lwt.map f (receive_raw st)

let create () =
  let st0 = create_raw () in
  let st1 = create_raw () in
  let out_ref = ref (Out(st1,(fun x -> x))) in
  let inp_ref = ref (Inp(st1,(fun x -> x))) in
  st1.out_refs <- OneOut(out_ref);
  st0.inp_refs <- OneInp(inp_ref);
  out_ref, inp_ref
