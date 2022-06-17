module Q = Queue

module M = Monitor

type 'a t = ('a Q.t ref) M.t

let create () : 'a t = M.create (ref (Q.create ()))

let send (t : 'a t) (v:'a) : unit = 
  M.lock t (fun q -> Q.add v !q; M.signal t)

let receive (t:'a t) : 'a = 
  M.wait t (fun q ->
    if Q.is_empty !q then
      M.WaitMore
    else
      M.Return (Q.take !q))

let clear_queue_ t = 
  M.lock t (fun q -> 
    let old = !q in 
    q := Q.create (); 
    old)

let peek (t:'a t) : 'a = 
  M.wait t (fun q ->
    if Q.is_empty !q then
      M.WaitMore
    else
      M.Return (Q.peek !q))

let clear (t:'a t) : unit =
  ignore (clear_queue_ t)

let is_empty (t:'a t) : bool =
  M.lock t (fun q -> Q.is_empty !q)

let length (t:'a t) : int =
  M.lock t (fun q -> Q.length !q)
