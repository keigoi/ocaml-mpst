type 'a name = 'a link ref
and 'a link = Link of 'a name | Chan of 'a endpoint
and 'a endpoint = { send : 'a -> unit; receive : unit -> 'a }

module type S = sig
  val new_name : unit -> 'a name
end

type chan = (module S)

let rec path_compression n =
  match !n with
  | Chan _ -> n
  | Link n1 ->
      let n1' = path_compression n1 in
      n := Link n1';
      n1

let rec get n = match !n with Chan ch -> ch | Link n -> get n

let finalise n =
  let n = path_compression n in
  get n

let send ep v = ep.send v
let receive ep = ep.receive ()

let rec unify0 : type a. a name -> a name -> unit =
 fun c1 c2 ->
  match (c1, c2) with
  | _, { contents = Link c2' } -> unify0 c1 c2'
  | { contents = Link c1' }, _ -> unify0 c1' c2
  | _ -> if c1 == c2 then () else c2 := Link c1

let unify c1 c2 =
  unify0 c1 c2;
  ignore @@ path_compression c1;
  ignore @@ path_compression c2

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

  let new_name (type a) () : a name =
    let module M = Add (struct
      type t = a
    end) in
    ref (Chan { send = M.send; receive = M.receive })
end

let make () : (module S) =
  let module M = Make () in
  (module M)

let new_name (module M : S) = M.new_name ()
