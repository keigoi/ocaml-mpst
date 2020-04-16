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
end

module IO_list = struct
  let iter = List.iter
  let iteri = List.iteri
end

module Thread = Thread
module Event = Event
module Mutex = Mutex
module Unix = Unix
