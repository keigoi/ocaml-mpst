module M = Mutex
module C = Condition

type 'a t = ('a * M.t * C.t)

type 'a wait = WaitMore | Return of 'a

let create v = (v, M.create (), C.create ())

let wait (cell,m,c) (func: 'a -> 'b wait) : 'b = 
  Mutex.lock m;
  let rec loop () =
    begin try
      match func cell with
      | Return v -> v
      | WaitMore -> begin
          Condition.wait c m; 
          loop ()
        end
    with e ->
      Mutex.unlock m;
      raise e
    end
  in
  let v = loop () in
  Mutex.unlock m;
  v

let signal (_,_,c) = Condition.signal c

let lock (cell,m,_) (func: 'a -> 'b) : 'b =
  Mutex.lock m;
  begin try
    let v = func cell in
    Mutex.unlock m;
    v
  with e ->
    Mutex.unlock m;
    raise e
  end

let try_lock (cell,m,_) (func: 'a -> 'b) (iffail:'b) : 'b =
  if Mutex.try_lock m then begin
    begin try
      let v = func cell in
      Mutex.unlock m;
      v
    with e ->
      Mutex.unlock m;
      raise e
    end
  end else iffail

let get (cell,_,_) = cell
