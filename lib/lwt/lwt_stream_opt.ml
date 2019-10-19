(* lwt stream optimized for unbounded buffering *)
type 'a t = {
    mutable push_signal : unit Lwt.t;
    mutable push_signal_resolver : unit Lwt.u;
    mutable push_waiting : bool;
    mutable node : 'a node;
    last: 'a node ref
  }
and 'a node = {
    mutable next : 'a node;
    mutable data : 'a option
  }

let new_node () =
  let rec node = { next = node; data = None } in
  node

let create () =
  let push_signal, push_signal_resolver = Lwt.wait () in
  let last = new_node () in
  let t = {
      push_signal;
      push_signal_resolver;
      push_waiting=false;
      node = last;
      last = ref last;
    }
  in
  t

let enqueue' e last =
  let node = !last
  and new_last = new_node () in
  assert (node.data <> None);
  new_last.data <- e;
  node.next <- new_last;
  last := new_last

let send t v =
  begin match t.node.data with
  | None ->
     (* print_endline "stream: None"; *)
     t.node.data <- Some v
  | _ ->
     (* print_endline "stream: enq"; *)
     enqueue' (Some v) t.last
  end;
  if t.push_waiting then begin
      (* print_endline "found waitor"; *)
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
     (* print_endline "None"; *)
     t.push_waiting <- true;
     (* Lwt.on_cancel t.push_signal (fun _ -> print_endline "cancelled"); *)
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
        (* print_endline "Received"; *)
        t.node.data <- None;
        t.node <- t.node.next;
        Lwt.return x
     | None ->
        failwith "lwt_stream_opt: TODO"
        (*TODO*)
        (* next_rec ~f t *)

let receive t =
  next_rec ~f:(fun x -> Some x) t

let receive_wrap ~f t =
  next_rec ~f t
