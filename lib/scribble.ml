(* WIP *)

type ('a,'b) case2 = L1 of 'a | L2 of 'b
module M = struct
  type 'a send = Send of 'a
  type 'a recv = Recv of 'a
  type 'a select = Select of 'a
  type 'a branch = Branch of 'a Lwt.t
  type close = Close

  type 'a mpst = MPST : ('a Lwt.t * 'a Lwt.u -> 'a) -> 'a mpst

  let send : 'r 'l 'v 's. ((< .. > as 'r) -> (< .. > as 'l)) -> ('l -> 'v -> 's) -> 'v -> 'r select -> 's =
    fun f g v (Select r) ->
    g (f r) v

  let receive : 'r 'l. (([>] as 'r) -> ([>] as 'l)) -> 'r branch -> 'l Lwt.t =
    fun f (Branch l) ->
    Lwt.map f l

  let close : close -> unit = fun _ -> ()

  let f2 obj1 obj2 =
    let t1, u1 = Lwt.wait () in
    let t2, u2 = Lwt.wait () in
    object
      method a =
        Select (object method b=
                    object
                      method left=(fun v -> Lwt.wakeup_later u1 v; obj1#a);
                    end
                  method c =
                    object
                      method right=(fun v -> Lwt.wakeup_later u2 v; obj2#a)
                    end
                end)
      method b =
        Branch (Lwt.map (fun v -> `A(`Left(v, obj1#b))) t1)
      method c =
        Branch (Lwt.map (fun v -> `A(`Right(v, obj2#c))) t2)
    end

  let finish =
    object
      method a = Close
      method b = Close
    end

  (* let p = f finish finish
   * let pa = p#a *)

  let g s =
    let s = send (fun x -> x#b) (fun x -> x#left) 100 s in
    Lwt.bind (receive (fun (`B(x)) -> x) s) @@ function
                                              | `Left(s) ->
                                                 let s = send (fun x -> x#b) (fun x -> x#right) "abc" s in
                                                 close s;
                                                 Lwt.return ()
                                              | `Right(s) ->
                                                 close s;
                                                 Lwt.return ()

  let f1 () =
    let t, u = Lwt.wait () in
    object
      method a =
        Select (object method b=
                    object
                      method left=(fun v ->
                        let t', u' = Lwt.wait () in
                        Lwt.wakeup_later u (L1 (v, u'));
                        Branch (Lwt.map (fun v -> `B(`Msg(v, Close))) t'))
                      method right=(fun v ->
                        Lwt.wakeup_later u (L2 v); Close)
                    end
                end)
      method b =
        Branch (Lwt.map
                  (function
                   | L1 (v, u) -> `A(`Left(v,
                                           Select(object method b = object
                                                        method msg = (fun v ->
                                                          Lwt.wakeup_later u v; Close) end end)))
                   | L2 v -> `A(`Right(v, Close))) t)
    end

  let f : unit -> unit Lwt.t = fun () ->
    let open Lwt in
    let s = (f1 ())#a in
    let s = send (fun x -> x#b) (fun x -> x#left) 100 s in
    receive (fun (`B(x)) -> x) s >>= (fun (`Msg(y, s)) ->
      close s;
      Lwt.return ())

end
