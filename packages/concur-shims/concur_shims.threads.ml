module IO = struct
  type +'a io = 'a
  let bind m f = f m
  let both a b = (a, b)
  let map f x = f x
  let return x = x
  let printl = print_endline
  let main_run x = x
  let read_line = input_line
  let stdin = stdin
  let stdout = stdout
  let stderr = stderr
  let catch f g =
    try
      f ()
    with
      exn ->
      g exn
  let yield _ = ()
  type in_channel = Stdlib.in_channel
  type out_channel = Stdlib.out_channel
  let pipe () =
    let inp,out = Unix.pipe () in
    Unix.in_channel_of_descr inp, Unix.out_channel_of_descr out
  let close_in = close_in
  let close_out = close_out
  let output_value = output_value
  let input_value = input_value
  let flush = flush
  let is_direct = true
end

module IO_list = struct
  let iter = List.iter
  let iteri = List.iteri
  let map = List.map
  let mapi = List.mapi
end

module Thread = Thread
module Event = Event
module Mutex = Mutex
module Unix = Unix
