open Session
open Monad

type empty = Empty

let send : 'r 'k 'ks 'ls 'v 's.
  ('r, 'k conn, _, 'ks, _) role ->
  ((< .. > as 'ls) -> 'v -> ('ks,'s) sess) ->
  'v ->
  (('ks, ('r, 'k, 'ls) send) sess, empty, ('ks,'s) sess) monad =
  fun r l v ->
  Monad.__in (fun s ->
      Lwt.return (Empty, Session.send r l v s)
    )

  (*   (\* %lin がないと *\)
   *   send r (fun x->x#left) 100 >>- fun s -> (\* s が複製できてしまうのでNG*\)
   *
   * (\* %lin があると *\)
   * send r (fun x->x#left) 100 >>= fun%lin #_0 -> ...
   *
   * ==>
   *
   * send r (fun x->x#left) 100 >>= fun __tmp -> put _0 __tmp >> .. *)


  (* receive r >>=
   *   function%lin
   *         | `left(v, #_0) -> ...
   *         | `right(w, #_0) -> ... *)


(* ls は たとえば [< `left of int * () sess | `right of ...] のような型．
   global type のほうで縛る. *)
let receive : 'r 'k 'ks. ('r, 'k conn, _, 'ks, _) role ->
  (('ks, ('r, 'k, ([>] as 'ls)) receive) sess, empty, 'ls) monad =
  fun r ->
  Monad.__in (fun s ->
      Lwt.bind (receive r s) (fun ls ->
      Lwt.return (Empty, ls))
    )

let close : 'ks. unit -> (('ks, close) sess, empty, unit) monad = fun () ->
  Monad.__in (fun s ->
      close s;
      Lwt.return (Empty, ())
    )

let accept : 'r 'k 'ks 'ks2 'ls.
  ('r, unit, 'k conn, 'ks, 'ks2) role ->
  'k ->
  (('ks, ('ks2, 'r, 'k, 'ls) accept) sess, empty, 'ls) monad =
  fun r k ->
  Monad.__in (fun s ->
      Lwt.bind (accept r k s) (fun ls ->
      Lwt.return (Empty, ls)))

let request : 'r 'k 'ks 'ks2 'ls 'v 's.
  ('r, unit, 'k conn, 'ks, 'ks2) role ->
  ('ls -> 'v -> ('ks2, 's) sess) ->
  'v ->
  'k ->
  (('ks, ('ks2, 'r, 'k, 'ls) request) sess, empty, ('ks2, 's) sess) monad =
  fun r l v k ->
  Monad.__in (fun s ->
      Lwt.return (Empty, (request r l v k s)))


let disconnect : 'r 'k 'ks 'ks2 's.
  ('r, 'k conn, unit, 'ks, 'ks2) role ->
  (('ks, ('ks2, 'r, 'k, 's) disconnect) sess, empty, ('ks2, 's) sess) monad =
  fun r ->
  Monad.__in (fun s ->
      Lwt.return (Empty, disconnect r s))
