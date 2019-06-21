let marshal_flags = [Marshal.Closures]

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let of_option ~dflt = function
  | Some x -> x
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

type tag = {tag:Obj.t}
let make_tag : 'v. ('v -> [>]) -> tag = fun f ->
  {tag=Obj.repr (f (Obj.magic ()))}

type 'a ep = LinFlag.t -> 'a

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
