module Make(BareSession:S.SESSION)(Global:S.GLOBAL)
       : sig
  open BareSession
  open LinMonad

  type 's sess = 's BareSession.sess
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
    ('pre, 'post, 's sess lin) monad

  val receive :
    ([>  ] as 'r) ->
    (('r, 'ls) receive sess lin, empty, 'pre, 'post) lens ->
    ('pre, 'post, 'ls lin) monad

  val close :
    (close sess lin, empty, 'pre, 'post) lens ->
    ('pre, 'post, unit data) monad

end
  = struct
  module S = BareSession
  module G = Global

  open LinMonad

  type 's sess = 's BareSession.sess

  type 'g global =
    {locals:(Obj.t * 'g Global.slots lazy_t Lwt_stream.t) list}

  let create_global f rs =
    (* let st = Lwt_stream.from_direct (fun () -> Some (f ())) in
     * {locals=List.map (fun r -> Obj.repr r, Lwt_stream.clone st) rs} *)
    match rs with
    | [] -> failwith "empty"
    | r::rs ->
       let st = Lwt_stream.from_direct (fun () -> Some (f ())) in
       {locals=(Obj.repr r,st) :: List.map (fun r -> Obj.repr r, Lwt_stream.clone st) rs}

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
       let {__lindata=s} = LinMonad.lens_get lens pre in
       let s = BareSession.send r (fun o v-> (sel o {data=v}).__lindata) v s in
       Lwt.return (LinMonad.lens_put lens pre Empty, {__lindata=s})
    }

  let receive r lens =
    {__run=
       fun pre ->
       let {__lindata=s} = LinMonad.lens_get lens pre in
       Lwt.bind (BareSession.receive r s) @@ fun ls ->
       Lwt.return (LinMonad.lens_put lens pre Empty, {__lindata=ls})
    }

  let close lens =
    {__run=
       fun pre ->
       let {__lindata=s} = LinMonad.lens_get lens pre in
       let () = BareSession.close s in
       Lwt.return (LinMonad.lens_put lens pre Empty, {data=()})
    }
end
