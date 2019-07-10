module Pure = struct
  type 'a t = 'a
  let return a = a
  let return_unit = ()
  let bind x f = f x
  let map f x = f x
  let iteriM = List.iteri
  let mapM = List.map
  let async f = ignore (Thread.create f ())
end
module Event : S.EVENT
       with type 'a event = 'a Event.event
       with type 'a monad = 'a
  = struct
  include Event
  type 'a monad = 'a
  let flip_channel x = x
  (* XXX a dumb implementation of receiving from multiple channels  *)
  let receive_list = function
    | [] ->
       Event.always []
    | ch::chs ->
       Event.wrap (Event.receive ch)
         (fun v ->
           v :: List.map (fun ch -> Event.sync @@ Event.receive ch) chs)
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

  let fork_child f =
    let pid = Unix.fork () in
    if pid = 0 then begin
        (f ():unit);
        exit 0;
      end
    else
      pid
end
