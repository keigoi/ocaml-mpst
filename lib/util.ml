open Base
open Session
open Global

module Labels = struct

  let msg_ write read (k1, k2) =
    Labels.mklabel (fun g -> object method msg=g end) (fun x -> `msg x) write read (k1, k2)

  let left_ write read (k1, k2) =
    Labels.mklabel (fun g -> object method left=g end) (fun x -> `left x) write read (k1, k2)

  let middle_ write read (k1, k2) =
    Labels.mklabel (fun g -> object method middle=g end) (fun x -> `middle x) write read (k1, k2)

  let right_ write read (k1, k2) =
    Labels.mklabel (fun g -> object method right=g end) (fun x -> `right x) write read (k1, k2)

  let leftright_ write read (k1, k2) =
    Labels.mklabel2 (fun f g -> object method left=f method right=g end) (fun x -> `left x) (fun x -> `right x) write read  (k1,k2)

  module Shmem = struct
    let dummyconn = ({conn=Some (); origin=None}, {conn=Some (); origin=None})

    let msg () =
      let st, push = Lwt_stream.create () in
      msg_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

    let left () =
      let st, push = Lwt_stream.create () in
      left_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

    let middle () =
      let st, push = Lwt_stream.create () in
      middle_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

    let right () =
      let st, push = Lwt_stream.create () in
      right_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn

    let leftright () =
      let st, push = Lwt_stream.create () in
      leftright_ (fun _ v -> push (Some v)) (fun _ -> Lwt_stream.next st) dummyconn
  end
end

module Roles = struct

  module LensUtil = struct
    let a : 'a1 'a2 'b 'c. ('a1, 'a2, <a:'a1; b:'b; c:'c>, <a:'a2; b:'b; c:'c>) lens =
      {get=(fun s -> s#a); put=(fun s v -> object method a=v method b=s#b method c=s#c end)}
    let b : 'a 'b1 'b2 'c. ('b1, 'b2, <a:'a; b:'b1; c:'c>, <a:'a; b:'b2; c:'c>) lens =
      {get=(fun s -> s#b); put=(fun s v -> object method a=s#a method b=v method c=s#c end)}
    let c : 'a 'b 'c1 'c2. ('c1, 'c2, <a:'a; b:'b; c:'c1>, <a:'a; b:'b; c:'c2>) lens =
      {get=(fun s -> s#c); put=(fun s v -> object method a=s#a method b=s#b method c=v end)}
  end

  type a = A
  type b = B
  type c = C

  let a : 'a1 'a2 'b 'c. ('a1 sess, 'a2 sess, <a:'a1 sess; b:'b sess; c:'c sess> mpst, <a:'a2 sess; b:'b sess; c:'c sess> mpst, a) role =
    {get=(fun s -> uget LensUtil.a s); put=(fun s v -> MPST (lazy [object method a=v method b=uget LensUtil.b s method c=uget LensUtil.c s end]))}, A
  let b =
    {get=(fun s -> uget LensUtil.b s); put=(fun s v -> MPST (lazy [object method a=uget LensUtil.a s method b=v method c=uget LensUtil.c s end]))}, B
  let c =
    {get=(fun s -> uget LensUtil.c s); put=(fun s v -> MPST (lazy [object method a=uget LensUtil.a s method b=uget LensUtil.b s method c=v end]))}, C

  let finish =
    MPST (Lazy.from_val [object method a=Close method b=Close method c=Close end])

end
