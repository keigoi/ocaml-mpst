open Concur_shims
open Base

let (let*) = IO.bind

module Make(DynLin:DynLin.S)(Lin:Comm.LIN) = struct
  module Comm = Comm.Make(DynLin)(Lin)
  open Comm

  type kind = [`Local | `IPCProcess | `Untyped]
  
  let ipc _ = failwith ""
  let untyped _ = failwith ""
  
  let rm_kind_of_kind ~rm_index ~rm_size = function
    | `Local -> {Env.rm_kind=EpLocal; rm_size; rm_index}
    | `IPCProcess -> {Env.rm_kind=ipc rm_size; rm_size; rm_index}
    | `Untyped -> {Env.rm_kind=untyped rm_size; rm_size; rm_index}
      
  type _ shared =
      Shared :
        {global: [`cons of 'ep * 'tl] global;
         kinds: (kind*int) list option;
         accept_lock: Mutex.t;
         connect_sync: ([`cons of 'ep * 'tl] Comm.tup) Event.channel list;
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
    let len = Comm.effective_length seq in
    let connect_sync = List.init len (fun _ -> Event.new_channel ()) in
    Shared
      {global;
       kinds;
       accept_lock;
       connect_sync;
      }
  
  let accept_ (Shared m) r =
    let* () = Mutex.lock m.accept_lock in
    let tup = init_seq_ (Shared m) in
    (* sync with all threads *)
    let me = int_of_idx r.role_index in
    let* () = sync_all_ me m.connect_sync tup in
    (* get my ep *)
    let ep = get_ch r tup in
    Mutex.unlock m.accept_lock;
    IO.return ep
  
  let connect_ (Shared m) r =
    let roleid = int_of_idx r.role_index in
    let c = List.nth m.connect_sync roleid in
    let* tup = Event.sync (Event.receive c) in
    IO.return (get_ch r tup)
  
  let accept sh r =
    accept_ sh r
  
  let connect sh r =
    connect_ sh r
  
  let accept_and_start (Shared{kinds;_} as sh) r f =
    let* ep = accept_ sh r in
    let env = mkenv kinds in
    let prop = Table.get env.metainfo (int_of_idx r.role_index) in
    match prop.rm_kind with
    | Env.EpLocal (* | EpUntyped _ *)
      ->
      ignore (Thread.create (fun () -> (f ep : unit IO.io)) ());
      IO.return ()
    (* | EpDpipe _ ->
     *   ignore (C.fork_child (fun () -> f ep));
     *   IO.return () *)
  
  let connect_and_start (Shared{kinds;_} as sh) r f =
    let* ep = connect_ sh r in
    let env = mkenv kinds in
    let prop = Table.get env.metainfo (int_of_idx r.role_index) in
    match prop.rm_kind with
    | EpLocal (* | EpUntyped _ *)
      ->
      ignore (Thread.create (fun () -> (f ep : unit IO.io)) ());
      IO.return ()
    (* | EpDpipe _ ->
     *   ignore (C.fork_child (fun () -> f ep));
     *   IO.return () *)
end

module Dyn = Make(DynLin.Check)(Comm.NoStatic)
