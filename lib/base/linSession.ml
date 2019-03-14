module Make(Sess:S.SESSION)
       : sig
  module Session : S.SESSION
  module LinMonad : S.LIN_MONAD

  open Session
  open LinMonad

  val send :
    ([>  ] as 'a) ->
    ((< .. > as 'b) -> 'v -> 's sess) ->
    'v ->
    (('a, 'b) send sess lin, 's sess lin, unit data) monad
  val receive :
    ([>  ] as 'a) ->
    (('a, 'ls) receive sess lin, unit, 'ls lin) monad

  val close :
    (close sess lin, unit, unit data) monad
       
end with module Session = Sess
  = struct
  module Session = Sess
  module LinMonad = LinMonad

  open LinMonad

  let send r sel v =
    {__run=
       fun {__lindata=s} ->
       let s = Session.send r sel v s in
       Lwt.return ({__lindata=s}, {data=()})
    }

  let receive r =
    {__run=
       fun {__lindata=s} ->
       Lwt.bind (Session.receive r s) @@ fun ls ->
       Lwt.return ((), {__lindata=ls})
    }

  let close =
    {__run=
       fun {__lindata=s} ->
       let () = Session.close s in
       Lwt.return ((), {data=()})
    }
end
