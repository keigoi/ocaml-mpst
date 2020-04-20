open Concur_shims
open Base

exception InvalidEndpoint = Mutex_flag.InvalidEndpoint

module type S = sig
  type +'a lin
  (** Linear type constructor *)

  type 's gen

  val declare : 'a -> 'a lin gen
  (** Declare a linear resource *)

  val fresh : 'a gen -> 'a
  (** Generate a fresh linear resource  *)

  val use : 'a lin -> 'a IO.io
  (** Extract the value. Raises InvalidEndpoint if the endpoint is already consumed *)

  val wrap : ('a -> 'b) -> 'a gen -> 'b gen
  (** Wrap a linear resource *)

  val merge : ('b -> 'b -> 'b) -> ('a, 'b lin) method_ -> 'a gen -> 'a gen -> 'a gen
  (** Merge two linear values (having the same flag) *)

  val lift_disj : ('lr,'l,'r) disj -> ('lr gen, 'l gen, 'r gen) disj
  (** Lift given disjoint concatenation *)

  val declare_unlimited : 'a -> 'a gen
end

module Check : S
(* Closure-less dynamic linearity checking (more efficient) *)
  = struct
  module Flag = Mutex_flag
  type once_store = Flag.t ref
  type 'a lin = {value:'a; store_ref:once_store ref}
  type 'a gen =
    {gen_value:'a;
     gen_store_ref:once_store ref;
     gen_store_assign:once_store -> unit}

  let[@inline] use t =
    IO.bind (Flag.use !(!(t.store_ref))) (fun[@inline] () ->
    IO.return t.value)

  let declare v =
    let store = ref (Flag.create ()) in
    let store_ref = ref store in
    let rec t =
    {gen_value={value=v; store_ref};
     gen_store_ref=store_ref;
     gen_store_assign=(fun store -> t.gen_store_ref := store)}
    in
    t

  let declare_unlimited v =
    {gen_value=v;
     gen_store_ref=ref @@ ref @@ Flag.create ();
     gen_store_assign=(fun _ -> ())}

  let wrap f x =
    {x with gen_value=f x.gen_value}

  let merge f meth ll rr =
    let l = meth.call_obj ll.gen_value
    and r = meth.call_obj rr.gen_value
    in
    let v = meth.make_obj {store_ref=l.store_ref; value = f l.value r.value}
    in
    {gen_value=v;
     gen_store_ref=ll.gen_store_ref;
     gen_store_assign=ll.gen_store_assign;
    }

  let lift_disj mrg =
    {disj_concat=(fun l r ->
       r.gen_store_assign !(l.gen_store_ref); (* track and use the same linearity flag *)
       {l with
         gen_value=mrg.disj_concat l.gen_value r.gen_value;
         gen_store_assign=
           (fun store -> l.gen_store_assign store; r.gen_store_assign store)
       }
     );
     disj_splitL=(fun lr -> wrap mrg.disj_splitL lr);
     disj_splitR=(fun lr -> wrap mrg.disj_splitR lr)}

  let[@inline] refresh t =
    !(t.gen_store_ref) := Flag.create ()

  let[@inline] fresh t =
    refresh t;
    t.gen_value
end

module Check_closure : S = struct
  module Flag = Mutex_flag

  type +'a lin = {once:Flag.t; value: 'a}
  type flag = Flag.t
  type 's gen = flag -> 's
  let use t =
    IO.bind (Flag.use t.once) (fun[@inline] () ->
    IO.return t.value)
  let declare v = fun once -> {once; value=v}
  let fresh f = f (Flag.create ())
  let merge f meth x y flag =
    let x = meth.call_obj (x flag) and y = meth.call_obj (y flag) in
    assert (x.once==y.once);
    meth.make_obj {value=f x.value y.value; once=x.once}
  let lift_disj mrg =
    {disj_concat=(fun ls rs once -> mrg.disj_concat (ls once) (rs once));
     disj_splitL=(fun lr once -> mrg.disj_splitL (lr once));
     disj_splitR=(fun lr once -> mrg.disj_splitR (lr once))}
  let wrap f x flag = f (x flag)
  let declare_unlimited v = fun _ -> v
end

module NoCheck : S with type 'a lin = 'a with type 's gen = 's = struct
  type +'a lin = 'a
  type 's gen = 's
  let[@inline] use t = IO.return t
  let[@inline] merge f meth x y =
    let x = meth.call_obj x and y = meth.call_obj y in
    meth.make_obj (f x y)
  external declare : 'a -> 'a = "%identity"
  external declare_unlimited : 'a -> 'a = "%identity"
  external fresh : 'a -> 'a = "%identity"
  external lift_disj : 'a -> 'a = "%identity"
  external wrap : ('a -> 'b) -> 'a -> 'b = "%apply"
end
