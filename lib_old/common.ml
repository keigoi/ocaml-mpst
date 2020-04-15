let marshal_flags = [Marshal.Closures]

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let of_option ~dflt = function
  | Some x -> x
  | None -> dflt

let option ~dflt ~f = function
  | Some x -> f x
  | None -> dflt

let rec transpose : 'a list list -> 'a list list = fun xss ->
  match xss with
  | [] -> []
  | []::_ -> []
  | xss ->
     let hds, tls =
       List.map (fun xs -> List.hd xs, List.tl xs) xss |> List.split
     in
     hds :: transpose tls

let rec find_physeq : 'a. 'a list -> 'a -> bool = fun xs y ->
  match xs with
  | x::xs -> if x==y then true else find_physeq xs y
  | [] -> false

let atomic =
  let m = Mutex.create () in
  fun f ->
  Mutex.lock m;
  try
    (f () : unit);
    Mutex.unlock m
  with e ->
    Mutex.unlock m;
    raise e

module Make_dpipe(M:S.MONAD)(C:S.SERIAL with type 'a monad = 'a M.t) = struct

  type pipe = {inp: C.in_channel; out: C.out_channel}
  type dpipe = {me:pipe; othr:pipe}

  let new_dpipe () =
    let my_inp, otr_out = C.pipe () in
    let otr_inp, my_out = C.pipe () in
    {me={inp=my_inp; out=my_out};
     othr={inp=otr_inp; out=otr_out}}

  let flip_dpipe {me=othr;othr=me} =
    {me;othr}

  let close_dpipe dp =
    M.bind (C.close_in dp.me.inp) (fun () ->
    M.bind (C.close_out dp.me.out) (fun () ->
    M.return_unit))
end
