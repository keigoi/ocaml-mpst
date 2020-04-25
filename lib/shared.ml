
module Make(DynLin:Dyn_lin.S)(Lin:Combinators.LIN)
       : S.SHARED
       with type 'a lin = 'a Lin.lin
        and type ('v,'t) out = ('v,'t) Combinators.Make(DynLin)(Lin).out
        and type 'var inp = 'var Combinators.Make(DynLin)(Lin).inp
        and type close = Combinators.Make(DynLin)(Lin).close
        and type ('v,'t) scatter = ('v,'t) Combinators.Make(DynLin)(Lin).scatter
        and type 'var gather = 'var Combinators.Make(DynLin)(Lin).gather
        and type 't global = 't Combinators.Make(DynLin)(Lin).global
        and type 't tup = 't Combinators.Make(DynLin)(Lin).tup
        and type 't ty = 't Combinators.Make(DynLin)(Lin).ty
  = struct

  open Concur_shims
  include Combinators.Make(DynLin)(Lin)

  type kind = [`Local | `IPCProcess | `Untyped]
  (** kind *)

  let ipc cnt =
    Env.EpDpipe (List.init cnt (fun _ -> Table.create ()))

  let untyped cnt =
    Env.EpUntyped (List.init cnt (fun _ -> Table.create ()))

  let rm_kind_of_kind ~rm_index ~rm_size = function
    | `Local -> {Env.rm_kind=EpLocal; rm_size; rm_index}
    | `IPCProcess -> {Env.rm_kind=ipc rm_size; rm_size; rm_index}
    | `Untyped -> {Env.rm_kind=untyped rm_size; rm_size; rm_index}
      
  type _ shared =
      Shared :
        {global: [`cons of 'ep * 'tl] global;
         kinds: (kind*int) list option;
         accept_lock: Mutex.t;
         connect_sync: ([`cons of 'ep * 'tl] tup) Event.channel list;
        } -> [`cons of 'ep * 'tl] shared
  
  let local _ = Env.EpLocal
  
  let mkenv ps =
    match ps with
    | Some ps -> 
      {Env.metainfo =
         Table.create_from
           (List.mapi (fun i (kind,size) -> rm_kind_of_kind ~rm_index:i ~rm_size:size kind) ps);
       default=local}
    | None -> {Env.metainfo=Table.create (); default=local}
  
  let init_seq_ (Shared m) =
    let env = mkenv m.kinds in
    let tup = gen_with_env env m.global in
    tup
  
  let sync_all_ myroleid connect_sync tup =
    IO_list.iteri (fun i ch ->
        if i=myroleid then begin
          IO.return ()
        end else begin
          Event.sync (Event.send ch tup)
        end
      )
      connect_sync
  
  let create_shared ?kinds global =
    let accept_lock = Mutex.create () in
    let env = mkenv kinds in
    let seq = gen_with_env env global in
    let len = effective_length seq in
    let connect_sync = List.init len (fun _ -> Event.new_channel ()) in
    Shared
      {global;
       kinds;
       accept_lock;
       connect_sync;
      }
  
  let accept_ (Shared m) r =
    let (let*) = IO.bind in
    let* () = Mutex.lock m.accept_lock in
    let tup = init_seq_ (Shared m) in
    (* sync with all threads *)
    let me = Base.int_of_idx r.role_index in
    let* () = sync_all_ me m.connect_sync tup in
    (* get my ep *)
    let ep = get_ch r tup in
    Mutex.unlock m.accept_lock;
    IO.return ep
  
  let connect_ (Shared m) r =
    let (let*) = IO.bind in
    let roleid = Base.int_of_idx r.role_index in
    let c = List.nth m.connect_sync roleid in
    let* tup = Event.sync (Event.receive c) in
    IO.return (get_ch r tup)
  
  let accept sh r =
    accept_ sh r
  
  let connect sh r =
    connect_ sh r
end
