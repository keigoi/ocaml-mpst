module IO = struct
  type +'a io = 'a
  let bind m f = f m
  let both a b = (a, b)
  let map f x = f x
  let return x = x
end

module IO_list = struct
  let iter = List.iter
  let iteri = List.iteri
end

module Thread = Thread
module Event = Event
module Mutex = Mutex
module Unix = Unix
