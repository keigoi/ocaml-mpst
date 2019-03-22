module Make(Sess:S.SESSION)(Glob:S.GLOBAL)
       : sig
  module Session : S.SESSION
  module Global : S.GLOBAL
  module LinMonad : S.LIN_MONAD
  open Session
  open LinMonad

  type 'g global

  val create_global : (unit -> 'g Global.slots lazy_t) -> [>] list -> 'g global 

  val connect :
    'g global ->
    (_, 's, _, 'g Global.slots, _) Global.role ->
    ('pre, 'pre, 's Global.e lin) monad
    
  val send :
    ([>  ] as 'r) ->
    ((< .. > as 'ls) -> 'v data -> 's sess lin) ->
    'v ->
    (('r, 'ls) send sess lin, empty, 'pre, 'post) lens ->
    ('pre lazy_t, 'post lazy_t, 's sess lin) monad

  val receive :
    ([>  ] as 'r) ->
    (('r, 'ls) receive sess lin, empty, 'pre, 'post) lens ->
    ('pre lazy_t, 'post lazy_t, 'ls lin) monad

  val close :
    (close sess lin, empty, 'pre, 'post) lens ->
    ('pre lazy_t, 'post lazy_t, unit data) monad

end with module Session = Sess and module Global = Glob and module LinMonad = LinMonad
  = struct
  module Session = Sess
  module Global = Glob
  module LinMonad = LinMonad

  open LinMonad

  type 'g global =
    {locals:(Obj.t * 'g Global.slots lazy_t Lwt_stream.t) list}

  let create_global f rs =
    let st = Lwt_stream.from_direct (fun () -> Some (f ())) in
    {locals=List.map (fun r -> Obj.repr r, Lwt_stream.clone st) rs}

  let connect {locals} {Global.role;lens} =
    {__run=
       fun pre->
       let st = List.assoc (Obj.repr role) locals in
       Lwt_stream.next st |> Lwt.map (fun g ->
       (pre, {__lindata=Global.lens_get_ lens g}))
    }

  let send r sel v lens =
    {__run=
       fun pre ->
       let {__lindata=s} = LinMonad.lens_get_ lens pre in
       let s = Session.send r (fun o v-> (sel o {data=v}).__lindata) v s in
       Lwt.return (LinMonad.lens_put_ lens pre Empty, {__lindata=s})
    }

  let receive r lens =
    {__run=
       fun pre ->
       let {__lindata=s} = LinMonad.lens_get_ lens pre in
       Lwt.bind (Session.receive r s) @@ fun ls ->
       Lwt.return (LinMonad.lens_put_ lens pre Empty, {__lindata=ls})
    }

  let close lens =
    {__run=
       fun pre ->
       let {__lindata=s} = LinMonad.lens_get_ lens pre in
       let () = Session.close s in
       Lwt.return (LinMonad.lens_put_ lens pre Empty, {data=()})
    }
end
