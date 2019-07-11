open Base

module MakeDynCheck(Flag:S.FLAG)
       : S.LIN
(* Closure-less implementation (more efficient?) *)
  = struct
  type once_store = Flag.t ref
  type 'a lin = {value:'a; store_ref:once_store ref}
  type 'a gen =
    {gen_value:'a;
     gen_store_ref:once_store ref;
     gen_store_assign:once_store -> unit}

  let use t =
    Flag.use !(!(t.store_ref));
    t.value

  let create v =
    let store = ref (Flag.create ()) in
    let store_ref = ref store in
    let rec t =
    {gen_value={value=v; store_ref};
     gen_store_ref=store_ref;
     gen_store_assign=(fun store -> t.gen_store_ref := store)}
    in
    t

  let create_nolin v = 
    {gen_value=v;
     gen_store_ref=ref @@ ref @@ Flag.create ();
     gen_store_assign=(fun _ -> ())}

  let create_dummy v = failwith "MakeDynCheck.create_dummy"

  let extract t = t.value

  let map_gen f x =
    {x with gen_value=f x.gen_value}

  let merge_gen f l r =
    {gen_value={l.gen_value with value=f l.gen_value.value r.gen_value.value};
     gen_store_ref=l.gen_store_ref;
     gen_store_assign=l.gen_store_assign;
    }

  let lift_disj_merge mrg =
    {disj_merge=(fun l r ->
       r.gen_store_assign !(l.gen_store_ref); (* track and use the same linearity flag *)
       {l with
         gen_value=mrg.disj_merge l.gen_value r.gen_value;
         gen_store_assign=
           (fun store -> l.gen_store_assign store; r.gen_store_assign store)
       }
     );
     disj_splitL=(fun lr -> map_gen mrg.disj_splitL lr);
     disj_splitR=(fun lr -> map_gen mrg.disj_splitR lr)}

  let refresh t =
    !(t.gen_store_ref) := Flag.create ()

  let fresh t =
    refresh t;
    t.gen_value
end

module NoCheck : S.LIN with type 'a lin = 'a with type 'a gen = 'a
  = struct
  type 'a lin = 'a
  type 'a gen = 'a

  let use t = t

  let create v = v
  let create_nolin v = v
  let create_dummy v = v

  let extract v = v
  let map_gen f x = f x

  let merge_gen f l r = f l r

  let lift_disj_merge mrg = mrg
  let refresh t = t

  let fresh t = t
end

module MakeDynCheckClosure(Flag:S.FLAG) : S.LIN
  = struct
  type 'a lin = {once:Flag.t; value: 'a}
  type 'a gen = Flag.t -> 'a
  let use t =
    Flag.use t.once;
    t.value
  let create v = fun once -> {once; value=v}
  let create_nolin v = fun _ -> v
  let create_dummy _ = failwith "MakeDynCheckClosure.create_dummy"
  let fresh f = f (Flag.create ())
  let map_gen f x = fun once -> f (x once)
  let merge_gen f x y = fun once -> {once; value=f (x once).value (y once).value}

  let lift_disj_merge : 'lr 'l 'r. ('lr,'l,'r) disj_merge -> ('lr gen, 'l gen, 'r gen) disj_merge = fun mrg ->
    {disj_merge=(fun l r once -> mrg.disj_merge (l once) (r once));
     disj_splitL=(fun lr -> map_gen mrg.disj_splitL lr);
     disj_splitR=(fun lr -> map_gen mrg.disj_splitR lr)}
end

module DynCheck = MakeDynCheck(LinFlag.PosixMutexFlag)
module DynCheckClosure = MakeDynCheckClosure(LinFlag.PosixMutexFlag)
