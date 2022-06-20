module U = Unification

type 'a ops = { send : 'a -> unit; receive : unit -> 'a }
type 'a endpoint = 'a ops U.t

module type S = sig
  val new_endpoint : unit -> 'a endpoint
end

type chan = (module S)

let finalise = U.path_compression
let unify = U.unify
let send ep v = (U.get ep).send v
let receive ep = (U.get ep).receive ()

module Make () : S = struct
  type payload = ..

  let channel : payload Chan.t = Chan.create ()

  module Add (ElemType : sig
    type t
  end) =
  struct
    type payload += Payload of ElemType.t

    let send v = Chan.send channel (Payload v)

    let receive () =
      match Chan.receive channel with
      | Payload v -> v
      | _ -> failwith "impossible: queue protocol mismatch"
  end

  let new_endpoint (type a) () : a endpoint =
    let module M = Add (struct
      type t = a
    end) in
    U.make { send = M.send; receive = M.receive }
end

let make () : (module S) =
  let module M = Make () in
  (module M)

let new_endpoint (module M : S) = M.new_endpoint ()
