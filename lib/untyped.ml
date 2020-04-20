open Concur_shims

type tag = {tag:Obj.t}[@@ocaml.unboxed]

let make_tag : 'v. ('v -> [>]) -> tag = fun f ->
  {tag=Obj.repr (f (Obj.magic ()))}

module Make(X:sig type 'a t and 'a u val fresh : 'a t -> 'a u end) = struct

  type 'v out =
    'v -> unit IO.io

  (* untyped operation; not for channel vectors *)
  let ep_magic (cont:'t Mergeable.t) : 'u Mergeable.t =
    Obj.magic cont

  type 'v raw_input_fun =
    unit -> (tag * 'v) IO.io

  type ('v,'var) wrapper =
    Wrapper :
      'cont X.t Mergeable.t * ('v * 'cont X.u -> 'var) -> ('v,'var) wrapper

  type ('v,'var) wrappers =
    (tag * ('v,'var) wrapper) list

  type 'var inp =
    Obj.t raw_input_fun * (Obj.t,'var) wrappers

  type 'var inplist =
    Obj.t raw_input_fun list * (Obj.t list,'var) wrappers

  let merge_wrapper (tagf,Wrapper(contf,wf)) (tagg,Wrapper(contg, _wg)) =
    assert (tagf=tagg);
    (* if polyvar tags are same, we can safely merge them *)
    let cont = Mergeable.merge contf (ep_magic contg) in
    (tagf,Wrapper(cont,wf))

  let merge_wrappers fs gs =
    let fdup,fonly =
      List.partition (fun (tagf,Wrapper(_,_)) ->
          List.exists (fun (tagg,Wrapper(_,_)) -> tagf=tagg) gs
        ) fs
    in
    let gdup,gonly =
      List.partition (fun (tagg,_) ->
          List.exists (fun (tagf,_) -> tagf=tagg) fs
        ) gs
    in
    fonly @ gonly @ List.map2 merge_wrapper fdup gdup

  let apply_wrapper wrappers tag v =
    let (Wrapper(cont,f)) =
      match wrappers with
      | [wrapper] -> snd wrapper
      | _ -> List.assoc tag wrappers
    in
    f (v, X.fresh @@ Mergeable.resolve cont)
end[@@inline]
