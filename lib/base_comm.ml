

module LwtEvent : S.EVENT = struct
  type 'a monad = 'a Lwt.t
  type 'a event = 'a Lwt.t
  type 'a st = {write: 'a option -> unit; read:'a Lwt_stream.t}
  type 'a channel = {me:'a st; othr:'a st}

  let new_channel () =
    let r1, w2 = Lwt_stream.create () 
    and r2, w1 = Lwt_stream.create ()
    in
    {me={write=w1;read=r1}; othr={write=w2;read=r2}}
  let receive {me={read}; _} = Lwt_stream.next read
  let flip {me=othr; othr=me} = {me; othr}
  let send {me={write; _}; _} v = write (Some v); Lwt.return_unit
  let sync = fun x -> x
  let guard f = f () (* XXX *)
  let choose = Lwt.choose
  let wrap e f = Lwt.map f e
  let always = Lwt.return
  let receive_list chs =
    Lwt_list.map_s (fun {me={read;_};_} -> Lwt_stream.next read) chs
end
