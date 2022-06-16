module type Op0 = sig
  type a
  type context

  val determinise : context -> a -> a
  val merge : context -> a -> a -> a
  val force : context -> a -> unit
  val to_string : context -> a -> string
end

module rec Context : sig
  type t

  module type Op1 = Op0 with type context := t

  type 'a state = { st : 'a; st_op : (module Op1 with type a = 'a) }
  type 'a value = 'a state lazy_t

  include ContextF.S with type t := t and type 'a value := 'a Context.value
end = struct
  module Ctx = ContextF.Make (Context)
  include Ctx

  module type Op1 = Op0 with type context := t

  type 'a state = { st : 'a; st_op : (module Op1 with type a = 'a) }
  type 'a value = 'a state lazy_t
end

module type Op = Context.Op1

type context = Context.t
type 't id = 't Context.key
type 'a op = (module Op with type a = 'a)
type 'a t = 'a Context.state = { st : 'a; st_op : 'a op }

let map_ops (type x y) (f : x -> y) (g : y -> x) (name : string -> string)
    (module D : Op with type a = x) : y op =
  let module M = struct
    type nonrec a = y

    let determinise ctx s = f @@ D.determinise ctx @@ g s

    let merge ctx s1 s2 =
      let inp1 = g s1 and inp2 = g s2 in
      f @@ D.merge ctx inp1 inp2

    let force ctx s = D.force ctx @@ g s
    let to_string ctx s = name @@ D.to_string ctx (g s)
  end in
  (module M)

open Rows

let obj_op meth =
  map_ops meth.make_obj meth.call_obj (fun s -> meth.method_name ^ s)

module Unit : Op with type a = unit = struct
  type a = unit

  let determinise _ _ = ()
  let merge _ _ _ = ()
  let force _ _ = ()
  let to_string _ _ = "end"
end

let determinise_list (type a) ctx (id : a id) (hds : a t lazy_t list) :
    a t lazy_t =
  match Context.lookup ctx id with
  | Some v -> v
  | None ->
      let hd =
        lazy
          (let { st_op = (module D : Op with type a = a); _ } =
             Lazy.force (List.hd hds)
           in
           let hds = List.map (fun hd -> (Lazy.force hd).st) hds in
           let st =
             match hds with
             | [ hd ] -> D.determinise ctx hd
             | hd :: hds ->
                 D.determinise ctx @@ List.fold_left (D.merge ctx) hd hds
             | [] -> failwith "impossible: empty_merge"
           in
           { st; st_op = (module D : Op with type a = a) })
      in
      Context.add_binding ctx id hd;
      hd
