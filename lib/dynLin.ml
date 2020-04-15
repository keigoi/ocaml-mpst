open Concur_shims
open Base

exception InvalidEndpoint

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

module Check : S = struct
  let (let*) = IO.bind
                 
  module Flag = struct
    type t         = Mutex.t
    let create ()  = Mutex.create ()
    let use f      =
      let* b = Mutex.try_lock f in
      if b then
        IO.return ()
      else
        raise InvalidEndpoint
  end
  
  type +'a lin = {once:Flag.t; value: 'a}
  type flag = Flag.t
  type 's gen = flag -> 's
  let use t =
    let* () = Flag.use t.once in
    IO.return t.value
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
  let use t = IO.return t
  let declare v = v
  let fresh v = v
  let merge f meth x y =
    let x = meth.call_obj x and y = meth.call_obj y in
    meth.make_obj (f x y)
  let lift_disj disj = disj
  let wrap f x = f x
  let declare_unlimited v = v
end
