module type Type = sig
  type 'a value
end

module type S = sig
  type t
  type 'a key
  type 'a value

  val make : unit -> t
  val new_key : unit -> 'a key
  val add_binding : t -> 'a key -> 'a value -> unit
  val lookup : t -> 'a key -> 'a value option
  val union_keys : 'a key -> 'a key -> 'a key
  val union_keys_general : 'a key -> 'b key -> ('a key, 'b key) Either.t
end

module Make (X : Type) : S with type 'a value = 'a X.value = struct
  module Key = struct
    type _ t = ..
  end

  module type W = sig
    type t
    type _ Key.t += Key : t Key.t
  end

  type 'a raw_key = (module W with type t = 'a)

  type t = binding list ref
  and binding = B : 'a key * 'a value -> binding
  and 'a value = 'a X.value
  and 'a key = { id_head : 'a raw_key; id_tail : key_ex list }
  and key_ex = KeyEx : 'a raw_key -> key_ex

  type ('a, 'b) eq = Eq : ('a, 'a) eq

  let newkey () (type s) =
    let module M = struct
      type t = s
      type _ Key.t += Key : t Key.t
    end in
    (module M : W with type t = s)

  let eq (type r s) (r : r raw_key) (s : s raw_key) : (r, s) eq option =
    let module R = (val r : W with type t = r) in
    let module S = (val s : W with type t = s) in
    match R.Key with S.Key -> Some Eq | _ -> None

  let make () = ref []
  let add_binding d k v = d := B (k, v) :: !d

  let lookup : type a. t -> a key -> a value option =
   fun d k ->
    let rec find : binding list -> a value option = function
      | [] -> None
      | B (k', v) :: bs -> (
          match eq k.id_head k'.id_head with
          | Some Eq when k.id_tail = k'.id_tail -> Some v
          | _ -> find bs)
    in
    find !d

  let union_sorted_lists (xs : 'a list) (ys : 'a list) =
    let rec loop aux xs ys =
      match (xs, ys) with
      | x :: xs, y :: ys ->
          if x = y then loop (x :: aux) xs ys
          else if x < y then loop (x :: aux) xs (y :: ys)
          else loop (y :: aux) (x :: xs) ys
      | [], ys -> List.rev aux @ ys
      | xs, [] -> List.rev aux @ xs
    in
    loop [] xs ys

  let union_keys_general (type a b) (ks1 : a key) (ks2 : b key) :
      (a key, b key) Either.t =
    if KeyEx ks1.id_head < KeyEx ks2.id_head then
      Left
        {
          id_head = ks1.id_head;
          id_tail =
            union_sorted_lists ks1.id_tail (KeyEx ks2.id_head :: ks2.id_tail);
        }
    else if KeyEx ks2.id_head < KeyEx ks1.id_head then
      Right
        {
          id_head = ks2.id_head;
          id_tail =
            union_sorted_lists (KeyEx ks1.id_head :: ks1.id_tail) ks2.id_tail;
        }
    else
      Left
        {
          id_head = ks1.id_head;
          id_tail = union_sorted_lists ks1.id_tail ks2.id_tail;
        }

  let union_keys (ks1 : 'a key) (ks2 : 'a key) : 'a key =
    match union_keys_general ks1 ks2 with Left ks | Right ks -> ks

  let new_key () = { id_head = newkey (); id_tail = [] }
end