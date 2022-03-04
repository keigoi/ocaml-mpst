module rec Self : sig
  type context

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_traverse : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }

  type 'a value = 'a head lazy_t

  include PolyHash.S with type t := context and type 'a value := 'a value
end = struct
  module Hash = PolyHash.Make (Self)
  include Hash

  type context = Hash.t

  type 'a head = {
    head : 'a;
    determinise_list : context -> 'a list -> 'a;
    force_traverse : context -> 'a -> unit;
    to_string : context -> 'a -> string;
  }

  type 'a value = 'a head lazy_t
end

include Self

let determinise_head_list ctx id hds =
  match Self.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let hd = Lazy.force (List.hd hds) in
           let body =
             hd.determinise_list ctx
               (List.map (fun hd -> (Lazy.force hd).head) hds)
           in
           {
             head = body;
             determinise_list = hd.determinise_list;
             force_traverse = hd.force_traverse;
             to_string = hd.to_string;
           })
      in
      Self.add_binding ctx id hd;
      hd

let try_cast_then_merge_heads ctx id constrA constrB headA headB =
  let headA = Lazy.force headA and headB = Lazy.force headB in
  match Rows.cast_if_constrs_are_same constrA constrB headB.head with
  | Some contB_body -> begin
      match Self.lookup ctx id with
      | Some v -> Some v
      | None ->
          let head = headA.determinise_list ctx [ headA.head; contB_body ] in
          let head = Lazy.from_val { headA with head } in
          Self.add_binding ctx id head;
          Some head
    end
  | None -> None
