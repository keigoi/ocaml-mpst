open Base
module Pure = struct
  type 'a t = 'a
  external return : 'a -> 'a = "%identity"
  external bind : 'a -> ('a -> 'b) -> 'b = "%revapply"
  external map : ('a -> 'b) -> 'a -> 'b = "%apply"
  let return_unit = ()
  let iteriM = List.iteri
  let mapM = List.map
  let yield () = ()
  let async f = ignore (Thread.create f ())

  type mutex = Mutex.t
  let create_mutex = Mutex.create
  let lock = Mutex.lock
  let unlock = Mutex.unlock
end
module Event : S.EVENT
       with type 'a event = 'a Event.event
       with type 'a monad = 'a
  = struct
  type 'a event = 'a Event.event
  type 'a channel = 'a Event.channel
  let new_channel = Event.new_channel
  let receive = Event.receive
  let send = Event.send
  let sync = Event.sync

  type 'a monad = 'a
  let flip_channel x = x

  type 'a st = 'a Event.channel list
  type 'a inp =
    | InpOne : 'a Event.event -> 'a one inp
    | InpMany : 'a Event.event -> 'a list inp 
  type 'a out =
    | OutOne : 'a Event.channel ref -> 'a one out
    | OutMany : 'a Event.channel list ref -> 'a list out

  let create_st ~num =
    List.init num (fun _ -> Event.new_channel ())
                     
  let wrap chs f =
    let r = ref @@ List.hd chs in
    (OutOne r, InpOne (Event.wrap (Event.guard (fun () -> Event.receive (!r))) f))

  let wrap_scatter chs f =
    let r = ref chs in
    (OutMany r,
     List.init
       (List.length chs)
       (fun i ->
         InpOne
           (Event.wrap (Event.guard (fun () -> Event.receive (List.nth (!r) i))) (f i))))

  let receive_list = function
    | [] ->
       Event.always []
    | ch::chs ->
       Event.wrap (Event.receive ch)
         (fun v ->
           v :: List.map (fun ch -> Event.sync @@ Event.receive ch) chs)

  let wrap_gather chs f =
    let rs = List.map (fun ch -> ref ch) chs in
    let g =
      Event.guard (fun () ->
          let chs = List.map (fun r -> !r) rs in
          receive_list chs)
    in
    (List.map (fun r -> OutOne r) rs,
     InpMany (Event.wrap g f))

  let merge_inp : type t. t inp -> t inp -> t inp = fun i1 i2 ->
    match i1,i2 with
    | InpOne(ev1),InpOne(ev2) ->
       InpOne(Event.choose [ev1; ev2])
    | InpMany(ev1),InpMany(ev2) ->
       InpMany(Event.choose [ev1; ev2])

  let merge_out : type t. t out -> t out -> t out = fun r1 r2 ->
    match r1,r2 with
    | OutOne(r1),OutOne(r2) ->
       r1 := !r2;
       OutOne(r1)
    | OutMany(r1),OutMany(r2) ->
       r1 := !r2;
       OutMany(r1)

  let send_st (OutOne r) v = Event.send !r v

  let sendmany_st (OutMany {contents=cs}) vf =
    Event.wrap
      (Event.send (List.hd cs) (vf 0))
      (fun () ->
        List.iteri
          (fun i c -> Event.sync (Event.send c (vf (i+1))))
          (List.tl cs))

  let receive_st (InpOne ev) = ev

  let receivemany_st (InpMany ev) = ev
end
module Serial : S.SERIAL
       with type 'a monad = 'a and type in_channel = Stdlib.in_channel and type out_channel=Stdlib.out_channel
  = struct
  type 'a monad = 'a
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
  let pipe () =
    let inp,out = Unix.pipe () in
    Unix.in_channel_of_descr inp, Unix.out_channel_of_descr out
  let input_value ch =
    Stdlib.input_value ch
  let input_tagged =
    input_value
  let output_value =
    Stdlib.output_value
  let output_tagged =
    output_value
  let flush ch =
    Stdlib.flush ch
  let input_value_list chs =
    let rec loop = function
      | [] -> []
      | ch::chs -> Stdlib.input_value ch::loop chs
    in
    loop chs

  let close_in = close_in
  let close_out = close_out

  let fork_child f =
    let pid = Unix.fork () in
    if pid = 0 then begin
        (f ():unit);
        exit 0;
      end
    else
      pid
end
