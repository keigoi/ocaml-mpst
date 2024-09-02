module type Op0 = sig
  type a
  type context

  val determinise : context -> a -> a
  val flatten : context -> a -> a
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

let map_op (type x y z) (f : x * z -> y) (g : y -> x * z) (merge : z -> z -> z)
    (module D : Op with type a = x) : y op =
  let module M = struct
    type nonrec a = y

    let determinise ctx s =
      let x, z = g s in
      f @@ (D.determinise ctx x, z)

    let flatten ctx s =
      let x, z = g s in
      f @@ (D.flatten ctx x, z)

    let merge ctx s1 s2 =
      let inp1, z1 = g s1 and inp2, z2 = g s2 in
      let z12 = merge z1 z2 in
      f @@ (D.merge ctx inp1 inp2, z12)

    let force ctx s = D.force ctx @@ fst @@ g s
    let to_string ctx s = D.to_string ctx @@ fst (g s)
  end in
  (module M)

open Rows

let obj_op meth =
  map_op
    (fun (s, ()) -> meth.make_obj s)
    (fun s -> (meth.call_obj s, ()))
    (fun _ _ -> ())

module Unit : Op with type a = unit = struct
  type a = unit

  let determinise _ _ = ()
  let flatten _ _ = ()
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
