(* WIP *)

type ('a,'b) case2 = L1 of 'a | L2 of 'b
module M = struct
  type 'a send = Send of 'a
  type 'a recv = Recv of 'a
  type 'a select = Select of 'a
  type 'a branch = Branch of 'a Lwt.t
  type close = Close

  let send : 'r 'l 'v 's. ((< .. > as 'r) -> (< .. > as 'l)) -> ('l -> 'v -> 's) -> 'v -> 'r select -> 's =
    fun f g v (Select r) ->
    g (f r) v

  let receive : 'r 'l. (([>] as 'r) -> ([>] as 'l)) -> 'r branch -> 'l Lwt.t =
    fun f (Branch l) ->
    Lwt.map f l

  let close : close -> unit = fun _ -> ()


  let f obj1 obj2 =
    let t, u = Lwt.wait () in
    object
      method a =
        Select (object method b=
                    object
                      method left=(fun v -> Lwt.wakeup_later u (L1 v); obj1#a);
                      method right=(fun v -> Lwt.wakeup_later u (L2 v); obj2#a)
                    end
                end)
      method b =
        Branch (Lwt.map (function
                | L1 v -> `A(`Left(v, obj1#b))
                | L2 v -> `A(`Right(v, obj2#b))) t)
    end

  (* let p1 obj1 obj2 =
   *   let u1' = ref @@ Lwt.wait () in
   *   let u2' = ref @@ Lwt.wait () in
   *   object
   *     method a = Select [`B(`Left(u1', obj1#a)); `C(`Right(u2', obj2#a))]
   *     method b = Branch [`A(`Left(u1', obj1#b))]
   *     method c = Branch [`A(`Left(u2', obj1#b))]
   *   end *)

  let finish =
    object
      method a = Close
      method b = Close
    end

  let p = f finish finish
  let pa = p#a

  let f s =
    let s = send (fun x -> x#b) (fun x -> x#left) 100 s in
    Lwt.bind (receive (fun (`B(x)) -> x) s) @@ function
    | `Left(s) ->
       let s = send (fun x -> x#b) (fun x -> x#right) "abc" s in
       close s;
       Lwt.return ()
    | `Right(s) ->
       close s;
       Lwt.return ()

  let g s =
    let s = send (fun x -> x#b) (fun x -> x#left) 100 s in
    close s

  let () =
    g pa
end
