(* from https://alan.petitepomme.net/cwn/2015.03.24.html#1 *)

module Key = struct
  type _ t = ..
end

module type W = sig
  type t
  type _ Key.t += Key : t Key.t
end

type 'a key = (module W with type t = 'a)
type key_ex = KeyEx : 'a key -> key_ex

type 'a keyset = 'a key * key_ex list

let str_of_key (k:'a key) = string_of_int @@ Obj.magic k
let str_of_keyset ((k,ks):'a keyset) = 
  let keyex (KeyEx(k)) = str_of_key k in
  str_of_key k ^ ";" ^ String.concat "," @@ List.map keyex ks

let newkey () (type s) =
  let module M = struct
    type t = s
    type _ Key.t += Key : t Key.t
  end
  in
  (module M : W with type t = s)

type ('a, 'b) eq = Eq : ('a, 'a) eq

let eq (type r) (type s) (r : r key) (s : s key)
  : (r, s) eq option
  =
  let module R = (val r : W with type t = r) in
  let module S = (val s : W with type t = s) in
  match R.Key with
  | S.Key -> Some Eq
  | _ -> None

let union_sorted_lists (xs:'a list) (ys:'a list) =
  let rec loop aux xs ys =
    match xs, ys with
    | x::xs, y::ys ->
      if x=y then
        loop (x::aux) xs ys
      else if x<y then 
        loop (x::aux) xs (y::ys)
      else
        loop (y::aux) (x::xs) ys
    | [], ys -> List.rev aux @ ys
    | xs, [] -> List.rev aux @ xs
  in loop [] xs ys

let union_keys ((k1,ws1):'a keyset) ((k2,ws2):'a keyset) : 'a keyset =
  ((if k1 < k2 then k1 else k2), union_sorted_lists ws1 ws2)

let union_keys_generalised ((k1,ws1):'a keyset) ((k2,ws2):'b keyset) : ('a keyset, 'b keyset) Either.t =
  let all = union_sorted_lists ws1 ws2 in
  if KeyEx k1 < KeyEx k2 then 
    Left(k1, all) 
  else
    Right (k2, all)
    
  
type 'a state_id = 'a keyset
type visited = V : 'a state_id -> visited

type 'a head = {
  head: 'a; 
  merge: 'a -> 'a -> 'a; 
  determinise: dict -> 'a -> unit;
}
(* determinisation context *)
and binding = B : 'a keyset * 'a head -> binding
and dict = binding list

let mkbind  k v = [B(k,v)]

let union x y = x @ y

let empty = []

let lookup : type a. dict -> a keyset -> a head option = fun d (k,ws) ->
  let rec find : dict -> a head option = function
  | [] -> None
  | B ((k',ws'), v) :: bs ->
      match eq k k' with
      | Some Eq when ws=ws' -> Some v
      | _ -> find bs
  in
  find d
