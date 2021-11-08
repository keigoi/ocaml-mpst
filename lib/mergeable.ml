open Types

type hook = unit lazy_t
type 'a mvalue = { value : 'a; mergefun : 'a -> 'a -> 'a; hook : hook }
type 'a cache = 'a mvalue lazy_t

type 'a t =
  | Value : 'a mvalue -> 'a t  (** fully resolved merge *)
  | RecVar : 'a t lazy_t * 'a cache -> 'a t  (** (A) a recursion variable *)
  | Merge : 'a t * 'a t * 'a cache -> 'a t
      (** (B) delayed merge involving recvars *)
  | Disj : 'l t * 'r t * ('lr, 'l, 'r) disj * 'lr cache -> 'lr t
      (** (C) disjoint merge involving recvars (output) *)

exception UnguardedLoop

let seq_hook l r =
  lazy
    (Lazy.force l.hook;
     Lazy.force r.hook)

let merge_value l r =
  {
    value = l.mergefun l.value r.value;
    mergefun = l.mergefun;
    hook = seq_hook l r;
  }

let make_value_disj :
      'lr 'l 'r. ('lr, 'l, 'r) disj -> 'l mvalue -> 'r mvalue -> 'lr mvalue =
 fun disj bl br ->
  let mergefun lr1 lr2 =
    disj.disj_concat
      (bl.mergefun (disj.disj_splitL lr1) (disj.disj_splitL lr2))
      (br.mergefun (disj.disj_splitR lr1) (disj.disj_splitR lr2))
  in
  let value = disj.disj_concat bl.value br.value in
  { value; mergefun; hook = seq_hook bl br }

let rec find_phys_eq : 'a. 'a list -> 'a -> bool =
 fun xs y ->
  match xs with
  | x :: xs -> if x == y then true else find_phys_eq xs y
  | [] -> false

(** * Resolve delayed merges. * It carries list of recursion variables forced in
    the first arg, so that * *)
let rec real_resolve : type x. x t lazy_t list -> x t -> x mvalue option =
 fun hist -> function
  | Value v -> Some v (* already resolved *)
  | RecVar (t, _) ->
      (* (A) a recursion variable -- check occurrence of it *)
      if find_phys_eq hist t then None
        (* we found a cycle of form (μt. .. ⊔ t ⊔ ..) -- strip t *)
      else (* force and resolve it *)
        real_resolve (t :: hist) (Lazy.force t)
  | Merge (s, t, _) -> (
      (* (B) merge recursion variables *)
      match (real_resolve hist s, real_resolve hist t) with
      | Some s, Some t -> Some (merge_value s t)
      | Some s, None | None, Some s -> Some s
      | None, None -> None)
  | Disj (l, r, mrg, _) ->
      (* (C) disjoint merge involves recursion variables *)
      (* we can safely reset the history; as the two types are
         different from the original one, the same type variable will not occur. *)
      let l = do_real_resolve l in
      let r = do_real_resolve r in
      Some (make_value_disj mrg l r)

and do_real_resolve : 'a. 'a t -> 'a mvalue =
 fun t ->
  match real_resolve [] t with Some v -> v | None -> raise UnguardedLoop

let make_recvar t =
  let rec d = RecVar (t, lazy (do_real_resolve d)) in
  d

let make_merge_delayed : 'a. 'a t -> 'a t -> 'a t =
 fun s t ->
  let rec d = Merge (s, t, lazy (do_real_resolve d)) in
  d

let merge : 'a. 'a t -> 'a t -> 'a t =
 fun l r ->
  match (l, r) with
  | Value ll, Value rr ->
      let blr = merge_value ll rr in
      Value blr
  | _ -> make_merge_delayed l r

let make_disj : 'lr 'l 'r. ('lr, 'l, 'r) disj -> 'l t -> 'r t -> 'lr t =
 fun mrg l r ->
  match (l, r) with
  | Value bl, Value br ->
      let blr = make_value_disj mrg bl br in
      Value blr
  | _ ->
      let rec d =
        Disj (l, r, mrg, lazy (do_real_resolve d))
        (* prerr_endline "WARNING: internal choice involves recursion variable"; *)
      in
      d

let resolve : type x. x t -> x =
 fun t ->
  let b =
    match t with
    | Value b ->
        Lazy.force b.hook;
        b
    | RecVar (_, d) -> Lazy.force d
    | Disj (_, _, _, d) -> Lazy.force d
    | Merge (_, _, d) -> Lazy.force d
  in
  b.value

let make ~value ~mergefun ?cont () =
  let hook =
    match cont with
    | None -> Lazy.from_val ()
    | Some cont -> lazy (ignore (resolve cont))
  in
  Value { mergefun; value; hook }
