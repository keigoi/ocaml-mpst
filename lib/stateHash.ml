module type Type = sig
  type 'a head
end

module type S = sig
  type t
  type 'a state_id
  type 'a head

  val make : unit -> t
  val add_binding : t -> 'a state_id -> 'a head lazy_t -> unit
  val lookup : t -> 'a state_id -> 'a head lazy_t option
  val make_key : unit -> 'a state_id
  val union_keys : 'a state_id -> 'a state_id -> 'a state_id

  val general_union_keys :
    'a state_id -> 'b state_id -> ('a state_id, 'b state_id) Either.t
end

module Make (X : Type) = struct
  module Key = struct
    type _ t = ..
  end

  module type W = sig
    type t
    type _ Key.t += Key : t Key.t
  end

  type 'a key = (module W with type t = 'a)

  type 'a head = 'a X.head
  and binding = B : 'a state_id * 'a head lazy_t -> binding
  and t = binding list ref
  and key_ex = KeyEx : 'a key -> key_ex
  and 'a state_id = { id_head : 'a key; id_tail : key_ex list }

  type ('a, 'b) eq = Eq : ('a, 'a) eq

  let newkey () (type s) =
    let module M = struct
      type t = s
      type _ Key.t += Key : t Key.t
    end in
    (module M : W with type t = s)

  let eq (type r s) (r : r key) (s : s key) : (r, s) eq option =
    let module R = (val r : W with type t = r) in
    let module S = (val s : W with type t = s) in
    match R.Key with S.Key -> Some Eq | _ -> None

  let make () = ref []
  let add_binding d k v = d := B (k, v) :: !d

  let lookup : type a. t -> a state_id -> a head lazy_t option =
   fun d k ->
    let rec find : binding list -> a head lazy_t option = function
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

  let general_union_keys (type a b) (ks1 : a state_id) (ks2 : b state_id) :
      (a state_id, b state_id) Either.t =
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

  let union_keys (ks1 : 'a state_id) (ks2 : 'a state_id) : 'a state_id =
    match general_union_keys ks1 ks2 with Left ks | Right ks -> ks

  let make_key () = { id_head = newkey (); id_tail = [] }
end