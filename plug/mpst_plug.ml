type tag = { tag : Obj.t }
(** ocaml-mpst w/ explicit connection handling *)

let make_tag : 'v. ('v -> [> ]) -> tag =
 fun f -> { tag = Obj.repr (f (Obj.magic ())) }

type ('lr, 'l, 'r) disj_merge = {
  disj_merge : 'l -> 'r -> 'lr;
  disj_splitL : 'lr -> 'l;
  disj_splitR : 'lr -> 'r;
}

type ('la, 'va) method_ = { make_obj : 'va -> 'la; call_obj : 'la -> 'va }
type ('la, 'lb, 'va, 'vb) label = { obj : ('la, 'va) method_; var : 'vb -> 'lb }
type ('kb, 'vb) input_handler = 'kb -> 'vb option Lwt.t
type ('ka, 'va) output_handler = 'ka -> 'va -> unit Lwt.t

type ('ka, 'kb, 'v, 'buf) handler = {
  write : 'ka -> 'v -> unit Lwt.t;
  read : 'kb -> 'buf option -> ('buf option * 'v option) Lwt.t;
}

type (_, _, _, _, _, _, _) slabel =
  | Slabel : {
      handler : ('ka, 'kb, 'v, 'buf) handler;
      label : ('la, 'lb, 'v -> 'ca Lwt.t, 'v * 'cb) label;
    }
      -> ('la, 'lb, 'v -> 'ca, 'v * 'cb, 'ka, 'kb, 'buf) slabel

let ( %% ) label handler = Slabel { handler; label }

type (_, _, _, _) lens =
  | Zero : ('a, 'b, < c : 'a * 'tl >, < c : 'b * 'tl >) lens
  | Succ :
      ('a, 'b, 'aa, 'bb) lens
      -> ('a, 'b, < c : 'hd * 'aa >, < c : 'hd * 'bb >) lens

type (_, _, _, _) role =
  | Role : {
      role_label : ('r, 'v) method_;
      role_index : ('a, 'b, 'aa, 'bb) lens;
      role_index0 : ('a0, 'b0, 'aa0, 'bb0) lens;
    }
      -> ('r, 'v, 'a * 'b * 'aa * 'bb, 'a0 * 'b0 * 'aa0 * 'bb0) role

let map_option ~f = function Some v -> Some (f v) | None -> None

let rec find_physeq : 'a. 'a list -> 'a -> bool =
 fun xs y ->
  match xs with
  | x :: xs -> if x == y then true else find_physeq xs y
  | [] -> false

(** a flag for dynamic linearity checking *)
module Flag : sig
  type t

  exception InvalidEndpoint

  val use : t -> unit
  val create : unit -> t
end = struct
  type t = Mutex.t

  exception InvalidEndpoint

  let create () = Mutex.create ()
  let use f = if not (Mutex.try_lock f) then raise InvalidEndpoint
end

module Mergeable
    (*        : sig
     *   type 'a t
     *   val make : hook:unit lazy_t -> mergefun:('a -> 'a -> 'a) -> valuefun:(Flag.t -> 'a) -> 'a t
     *   val make_recvar : 'a t lazy_t -> 'a t
     *   val make_disj_merge : ('lr,'l,'r) disj_merge -> 'l t -> 'r t -> 'lr t
     *   val make_merge : 'a t -> 'a t -> 'a t
     *   val make_merge_list : 'a t list -> 'a t
     *   val wrap_label : (< .. > as 'l, 'v) method_ -> 'v t -> 'l t
     *   val wrap_label_fun : (< .. > as 'o, 'v) method_ -> ('p -> 'v) t -> ('p -> 'o) t
     *   val wrap_label_fun2 : (< .. > as 'o, 'v) method_ -> ('p -> 'q -> 'v) t -> ('p -> 'q -> 'o) t
     *   val generate : 'a t -> 'a
     * end *) =
struct
  type 'a t =
    | Single of 'a single  (** (A) delayed merge involving recvars *)
    | Merge of 'a single list * 'a cache

  and 'a single =
    | Val : 'a body * hook -> 'a single  (** fully resolved merge *)
    | DisjMerge :
        'l t * 'r t * ('lr, 'l, 'r) disj_merge * 'lr cache
        -> 'lr single  (** (B) disjoint merge involving recvars (output) *)
    | RecVar : 'a t lazy_t * 'a cache -> 'a single
        (** (C) a recursion variable *)

  and 'a body = { mergefun : 'a -> 'a -> 'a; valuefun : Flag.t -> 'a }

  and 'a cache = (Flag.t -> 'a) lazy_t

  and hook = unit lazy_t

  exception UnguardedLoop

  let merge_body (ll, hl) (rr, hr) =
    let hook =
      lazy
        (Lazy.force hl;
         Lazy.force hr)
    in
    ( {
        mergefun = ll.mergefun;
        valuefun =
          (fun once -> ll.mergefun (ll.valuefun once) (rr.valuefun once));
      },
      hook )

  let disj_merge_body :
        'lr 'l 'r. ('lr, 'l, 'r) disj_merge -> 'l body -> 'r body -> 'lr body =
   fun mrg bl br ->
    let mergefun lr1 lr2 =
      mrg.disj_merge
        (bl.mergefun (mrg.disj_splitL lr1) (mrg.disj_splitL lr2))
        (br.mergefun (mrg.disj_splitR lr1) (mrg.disj_splitR lr2))
    in
    let valuefun once =
      (* we can only choose one of them -- distribute the linearity flag among merged objects *)
      mrg.disj_merge (bl.valuefun once) (br.valuefun once)
    in
    { valuefun; mergefun }

  let merge_hook hl hr =
    lazy
      (Lazy.force hl;
       Lazy.force hr)

  (** * Resolve delayed merges *)
  let rec resolve_merge : type x. x t lazy_t list -> x t -> x body * hook =
   fun hist t ->
    match t with
    | Single s -> resolve_merge_single hist s
    | Merge (ss, _) ->
        (* (A) merge involves recursion variables *)
        resolve_merge_list hist ss

  and resolve_merge_single :
      type x. x t lazy_t list -> x single -> x body * hook =
   fun hist -> function
    | Val (v, hook) ->
        (* already resolved *)
        (v, hook)
    | DisjMerge (l, r, mrg, _d) ->
        (* (B) disjoint merge involves recursion variables *)
        (* we can safely reset the history; as the split types are different from the merged one, the same type variable will not occur. *)
        let l, hl = resolve_merge [] l in
        let r, hr = resolve_merge [] r in
        (disj_merge_body mrg l r, merge_hook hl hr)
    | RecVar (t, _d) ->
        (* (C) a recursion variable *)
        if find_physeq hist t then
          (* we found μt. .. ⊔ t ⊔ .. *)
          raise UnguardedLoop
        else
          (* force it, and resolve it. at the same time, check that t occurs again or not by adding t to the history  *)
          let b, _ = resolve_merge (t :: hist) (Lazy.force t) in
          (b, Lazy.from_val ())
  (* dispose the hook -- recvar is already evaluated *)

  and resolve_merge_list :
      type x. x t lazy_t list -> x single list -> x body * hook =
   fun hist ss ->
    (* remove unguarded recursions *)
    let solved : (x body * hook) list =
      List.fold_left
        (fun acc u ->
          try resolve_merge_single hist u :: acc
          with UnguardedLoop ->
            prerr_endline "WARNING: an unbalanced loop detected";
            (* remove it. *)
            acc)
        [] ss
    in
    (* then, merge them altogether *)
    match solved with
    | [] -> raise UnguardedLoop
    | x :: xs -> List.fold_left merge_body x xs

  let force_mergeable : 'a. 'a t -> Flag.t -> 'a =
   fun t ->
    let v, hook = resolve_merge [] t in
    Lazy.force hook;
    v.valuefun

  let make ~hook ~mergefun ~valuefun =
    Single (Val ({ mergefun; valuefun }, hook))

  let make_recvar_single t =
    let rec d = RecVar (t, lazy (force_mergeable (Single d))) in
    d

  let make_recvar t = Single (make_recvar_single t)

  let make_merge_single : 'a. 'a single list -> 'a t =
   fun us ->
    let rec d = Merge (us, lazy (force_mergeable d)) in
    d

  let make_merge : 'a. 'a t -> 'a t -> 'a t =
   fun l r ->
    match (l, r) with
    | Single (Val (ll, hl)), Single (Val (rr, hr)) ->
        let blr, hlr = merge_body (ll, hl) (rr, hr) in
        Single (Val (blr, hlr))
    | Single v1, Single v2 -> make_merge_single [ v1; v2 ]
    | Single v, Merge (ds, _) | Merge (ds, _), Single v ->
        make_merge_single (v :: ds)
    | Merge (d1, _), Merge (d2, _) -> make_merge_single (d1 @ d2)

  let make_merge_list = function
    | [] -> failwith "merge_all: empty"
    | m :: ms -> List.fold_left make_merge m ms

  let make_disj_merge :
        'lr 'l 'r. ('lr, 'l, 'r) disj_merge -> 'l t -> 'r t -> 'lr t =
   fun mrg l r ->
    match (l, r) with
    | Single (Val (bl, hl)), Single (Val (br, hr)) ->
        let blr = disj_merge_body mrg bl br in
        let hlr = merge_hook hl hr in
        Single (Val (blr, hlr))
    | _ ->
        let rec d =
          Single (DisjMerge (l, r, mrg, lazy (force_mergeable d)))
          (* prerr_endline "WARNING: internal choice involves recursion variable"; *)
        in
        d

  let mapbody : 'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p body -> 'q body =
   fun f g b ->
    {
      valuefun = (fun once -> f (b.valuefun once));
      mergefun = (fun l r -> f (b.mergefun (g l) (g r)));
    }

  let rec map_single :
            'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p single -> 'q single =
   fun f g -> function
    | Val (b, h) -> Val (mapbody f g b, h)
    | RecVar (_, _) ->
        assert false (* make_recvar_single (lazy (map f g (Lazy.force t))) *)
    | DisjMerge _ -> assert false

  and map : 'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> 'p t -> 'q t =
   fun f g -> function
    | Single s -> Single (map_single f g s)
    | Merge (ss, _) ->
        make_merge_list (List.map (fun s -> Single (map_single f g s)) ss)

  let mapfun :
        'p 'q 'x. ('p -> 'q) -> ('q -> 'p) -> ('p -> 'x) t -> ('q -> 'x) t =
   fun f g -> map (fun px q -> px (g q)) (fun qx p -> qx (f p))

  let wrap_label : 'v. ((< .. > as 'o), 'v) method_ -> 'v t -> 'o t =
   fun meth -> map meth.make_obj meth.call_obj

  let wrap_label_fun :
      type p v. ((< .. > as 'o), v) method_ -> (p -> v) t -> (p -> 'o) t =
   fun meth ->
    map (fun pv p -> meth.make_obj (pv p)) (fun po p -> meth.call_obj (po p))

  let wrap_label_fun2 :
      type p q v.
      ((< .. > as 'o), v) method_ -> (p -> q -> v) t -> (p -> q -> 'o) t =
   fun meth ->
    map
      (fun pqv p q -> meth.make_obj (pqv p q))
      (fun pqo p q -> meth.call_obj (pqo p q))

  let generate t =
    match t with
    | Single (Val (b, h)) ->
        Lazy.force h;
        b.valuefun (Flag.create ())
    | Single (RecVar (_, d)) -> Lazy.force d (Flag.create ())
    | Single (DisjMerge (_, _, _, d)) -> Lazy.force d (Flag.create ())
    | Merge (_, d) -> Lazy.force d (Flag.create ())
end

let delay_force m =
  lazy
    (ignore (Mergeable.generate m);
     ())

module HList = struct
  type _ vec = HCons : 'hd * 'tl vec -> < c : 'hd * 'tl > vec
  type vec_all_empty = (< c : unit * 'a > as 'a) vec

  let rec vec_all_empty = HCons ((), vec_all_empty)

  let hlist_head : type hd tl. < c : hd * tl > vec -> hd = function
    | HCons (hd, _) -> hd

  let hlist_tail : type hd tl. < c : hd * tl > vec -> tl vec = function
    | HCons (_, tl) -> tl

  let rec vec_get : type hd hd1 xs ys. (hd, hd1, xs, ys) lens -> xs vec -> hd =
   fun l xs ->
    match l with Zero -> hlist_head xs | Succ l -> vec_get l (hlist_tail xs)

  let rec vec_put :
      type hd hd1 xs ys. (hd, hd1, xs, ys) lens -> xs vec -> hd1 -> ys vec =
   fun l xs y ->
    match l with
    | Zero -> HCons (y, hlist_tail xs)
    | Succ l -> HCons (hlist_head xs, vec_put l (hlist_tail xs) y)
end

open HList

module Inp : sig
  type ('a, 'buf) inp

  val receive : ('a, 'buf) inp -> 'a Lwt.t

  val create_inp :
    ('k, 'k, 'ks, 'ks) lens ->
    (_, ([> ] as 'var), 'v -> _, 'v * 't, _, 'k, 'buf) slabel ->
    ('ks vec -> 't) Mergeable.t ->
    ('ks vec -> ('var, 'buf) inp) Mergeable.t

  val create_inp_conn :
    (unit, 'k, 'ks1, 'ks2) lens ->
    (_, 'var, 'v -> _, 'v * 't, _, 'k, 'buf) slabel ->
    ('ks2 vec -> 't) Mergeable.t ->
    ('ks1 vec -> 'k -> ('var, 'buf) inp) Mergeable.t

  val create_inp_discon :
    ('k, unit, 'ks1, 'ks2) lens ->
    (_, 'var, 'v -> _, 'v * 't, _, 'k, 'buf) slabel ->
    ('ks2 vec -> 't) Mergeable.t ->
    ('ks1 vec -> ('var, 'buf) inp) Mergeable.t
end = struct
  type ('l, 'r) either = Left of 'l | Right of 'r

  type ('a, 'buf) inp = {
    once : Flag.t;
    handler : 'buf option -> ('buf option, 'a) either Lwt.t;
  }

  let receive { once; handler } =
    Flag.use once;
    Lwt.bind (handler None) (function
      | Left _ -> Lwt.fail (Failure "receiption failed")
      | Right v -> Lwt.return v)

  let merge_inp f1 f2 =
    let seq h1 h2 buf =
      Lwt.bind (h1 buf) (function
        | Left buf -> h2 buf
        | Right success -> Lwt.return (Right success))
    in
    fun k0 ->
      let inp1, inp2 = (f1 k0, f2 k0) in
      { once = inp1.once; handler = seq inp1.handler inp2.handler }

  let ( let* ) = Lwt.bind

  let try_read (Slabel slabel) k ks cont buf =
    let* buf, res = slabel.handler.read k buf in
    match res with
    | None -> Lwt.return (Left buf)
    | Some v ->
        let cont = Mergeable.generate cont ks in
        Lwt.return @@ Right (slabel.label.var (v, cont))

  let create_inp :
      type k ks v t var buf.
      (k, k, ks, ks) lens ->
      (_, var, v -> _, v * t, _, k, buf) slabel ->
      (ks vec -> t) Mergeable.t ->
      (ks vec -> (var, buf) inp) Mergeable.t =
   fun lens slabel cont ->
    Mergeable.make ~hook:(delay_force cont) ~mergefun:merge_inp
      ~valuefun:(fun once ks ->
        {
          once;
          handler =
            (fun buf ->
              let k = vec_get lens ks in
              try_read slabel k ks cont buf);
        })

  let create_inp_conn :
      type k ks1 ks2 v t var buf.
      (unit, k, ks1, ks2) lens ->
      (_, var, v -> _, v * t, _, k, buf) slabel ->
      (ks2 vec -> t) Mergeable.t ->
      (ks1 vec -> k -> (var, buf) inp) Mergeable.t =
   fun lens slabel cont ->
    Mergeable.make ~hook:(delay_force cont)
      ~mergefun:(fun l r k -> merge_inp (l k) (r k))
      ~valuefun:(fun once ks1 k ->
        {
          once;
          handler =
            (fun buf ->
              let ks2 = vec_put lens ks1 k in
              try_read slabel k ks2 cont buf);
        })

  let create_inp_discon :
      type k ks1 ks2 v t var buf.
      (k, unit, ks1, ks2) lens ->
      (_, var, v -> _, v * t, _, k, buf) slabel ->
      (ks2 vec -> t) Mergeable.t ->
      (ks1 vec -> (var, buf) inp) Mergeable.t =
   fun lens slabel cont ->
    Mergeable.make ~hook:(delay_force cont) ~mergefun:merge_inp
      ~valuefun:(fun once ks1 ->
        {
          once;
          handler =
            (fun buf ->
              let k = vec_get lens ks1 in
              let ks2 = vec_put lens ks1 () in
              try_read slabel k ks2 cont buf);
        })
end

module Out : sig
  val create_out :
    ('k, 'k, 'ks, 'ks) lens ->
    ('obj, _, 'v -> 't, 'v * _, 'k, _, _) slabel ->
    ('ks vec -> 't) Mergeable.t ->
    ('ks vec -> 'obj) Mergeable.t

  val create_out_conn :
    (unit, 'k, 'ks1, 'ks2) lens ->
    ('obj, _, 'v -> 't, 'v * _, 'k, _, _) slabel ->
    ('ks2 vec -> 't) Mergeable.t ->
    ('ks1 vec -> 'k -> 'obj) Mergeable.t

  val create_out_discon :
    ('k, unit, 'ks1, 'ks2) lens ->
    ('obj, _, 'v -> 't, 'v * _, 'k, _, _) slabel ->
    ('ks2 vec -> 't) Mergeable.t ->
    ('ks1 vec -> 'obj) Mergeable.t
end = struct
  let write writefun k v cont =
    Lwt.bind (writefun k v) (fun () -> Lwt.return cont)

  let merge_out f _ =
    prerr_endline "INFO: output from non-enabled role";
    f

  let create_out :
      type ks k obj v t buf.
      (k, _, ks, _) lens ->
      (obj, _, v -> t, v * _, k, _, buf) slabel ->
      (ks vec -> t) Mergeable.t ->
      (ks vec -> obj) Mergeable.t =
   fun lens (Slabel label) cont ->
    Mergeable.make ~hook:(delay_force cont) ~mergefun:merge_out
      ~valuefun:(fun once ks ->
        label.label.obj.make_obj (fun v ->
            let cont = Mergeable.generate cont ks in
            Flag.use once;
            write label.handler.write (vec_get lens ks) v cont))

  let create_out_conn :
      type k ks1 ks2 obj v t buf.
      (unit, k, ks1, ks2) lens ->
      (obj, _, v -> t, v * _, k, _, buf) slabel ->
      (ks2 vec -> t) Mergeable.t ->
      (ks1 vec -> k -> obj) Mergeable.t =
   fun lens (Slabel label) cont ->
    Mergeable.make ~hook:(delay_force cont) ~mergefun:merge_out
      ~valuefun:(fun once ks1 k ->
        label.label.obj.make_obj (fun v ->
            let ks2 = vec_put lens ks1 k in
            let cont = Mergeable.generate cont ks2 in
            Flag.use once;
            write label.handler.write k v cont))

  let create_out_discon :
      type k ks1 ks2 obj v t buf.
      (k, unit, ks1, ks2) lens ->
      (obj, _, v -> t, v * _, k, _, buf) slabel ->
      (ks2 vec -> t) Mergeable.t ->
      (ks1 vec -> obj) Mergeable.t =
   fun lens (Slabel label) cont ->
    Mergeable.make ~hook:(delay_force cont) ~mergefun:merge_out
      ~valuefun:(fun once ks1 ->
        label.label.obj.make_obj (fun v ->
            let k = vec_get lens ks1 in
            let ks2 = vec_put lens ks1 () in
            let cont = Mergeable.generate cont ks2 in
            Flag.use once;
            write label.handler.write k v cont))
end

module Close : sig
  type close

  val close : close -> unit Lwt.t
  val mclose : (vec_all_empty -> close) Mergeable.t
end = struct
  type close = unit

  let merge_close _ _ _ = ()
  let close _ = Lwt.return_unit

  let mclose =
    Mergeable.make ~hook:(Lazy.from_val ()) ~mergefun:merge_close
      ~valuefun:(fun once _ ->
        Flag.use once;
        ())
end

module Discon : sig
  val create_discon :
    ('k, unit, 'ks1, 'ks2) lens ->
    ('ks2 vec -> 't) Mergeable.t ->
    ('ks1 vec -> < disconnect : 't Lwt.t >) Mergeable.t
end = struct
  let create_discon :
        'k 'ks1 'ks2.
        ('k, unit, 'ks1, 'ks2) lens ->
        ('ks2 vec -> 't) Mergeable.t ->
        ('ks1 vec -> < disconnect : 't Lwt.t >) Mergeable.t =
   fun lens cont ->
    Mergeable.make ~hook:(delay_force cont)
      ~mergefun:(fun d _ -> d)
      ~valuefun:(fun _once ks1 ->
        object
          method disconnect =
            Lwt.return @@ Mergeable.generate cont (vec_put lens ks1 ())
        end)
end

module Seq
    (*        : sig
     *   type _ t
     *
     *   exception UnguardedLoopSeq
     *
     *   val lens_get : ('a, _, 'aa, _) lens -> 'aa t -> 'a Mergeable.t
     *   val lens_put : ('a, 'b, 'aa, 'bb) lens -> 'aa t -> 'b Mergeable.t -> 'bb t
     *
     *   val seq_merge : 'a t -> 'a t -> 'a t
     *   val recvar : 'a t lazy_t -> 'a t
     *   val all_closed : (<c: (vec_all_empty -> Close.close) * 'a>as 'a) t
     *   val partial_force : 'x t -> 'x t
     * end *) =
struct
  type _ t =
    (* hidden *)
    | SeqCons : 'hd Mergeable.t * 'tl t -> < c : 'hd * 'tl > t
    | SeqFinish : (< c : (vec_all_empty -> Close.close) * 'a > as 'a) t
    | SeqRecVars : 'a t lazy_t list -> 'a t
    | SeqBottom : 'a t

  exception UnguardedLoopSeq

  let all_closed = SeqFinish
  let recvar l = SeqRecVars [ l ]

  let rec seq_head : type hd tl. < c : hd * tl > t -> hd Mergeable.t = function
    | SeqCons (hd, _) -> hd
    | SeqRecVars ds -> Mergeable.make_merge_list (List.map seqvar_head ds)
    | SeqFinish -> Close.mclose
    | SeqBottom -> raise UnguardedLoopSeq

  and seqvar_head : type hd tl. < c : hd * tl > t lazy_t -> hd Mergeable.t =
   fun d -> Mergeable.make_recvar (lazy (seq_head (Lazy.force d)))

  let rec seq_tail : type hd tl. < c : hd * tl > t -> tl t = function
    | SeqCons (_, tl) -> tl
    | SeqRecVars ds -> SeqRecVars (List.map seqvar_tail ds)
    | SeqFinish -> SeqFinish
    | SeqBottom -> raise UnguardedLoopSeq

  and seqvar_tail : type hd tl. < c : hd * tl > t lazy_t -> tl t lazy_t =
   fun d -> lazy (seq_tail (Lazy.force d))

  let rec lens_get :
      type a b xs ys. (a, b, xs, ys) lens -> xs t -> a Mergeable.t =
   fun ln xs ->
    match ln with Zero -> seq_head xs | Succ ln' -> lens_get ln' (seq_tail xs)

  let rec lens_put :
      type a b xs ys. (a, b, xs, ys) lens -> xs t -> b Mergeable.t -> ys t =
   fun ln xs b ->
    match ln with
    | Zero -> SeqCons (b, seq_tail xs)
    | Succ ln' -> SeqCons (seq_head xs, lens_put ln' (seq_tail xs) b)

  let rec seq_merge : type x. x t -> x t -> x t =
   fun l r ->
    match (l, r) with
    | SeqCons (_, _), _ ->
        let hd = Mergeable.make_merge (seq_head l) (seq_head r) in
        let tl = seq_merge (seq_tail l) (seq_tail r) in
        SeqCons (hd, tl)
    | _, SeqCons (_, _) -> seq_merge r l
    (* delayed constructors are left as-is *)
    | SeqRecVars us1, SeqRecVars us2 -> SeqRecVars (us1 @ us2)
    (* repeat *)
    | SeqFinish, _ -> SeqFinish
    | _, SeqFinish -> SeqFinish
    (* bottom *)
    | SeqBottom, _ -> raise UnguardedLoopSeq
    | _, SeqBottom -> raise UnguardedLoopSeq

  let rec force_recvar : type x. x t lazy_t list -> x t lazy_t -> x t =
   fun hist w ->
    if find_physeq hist w then raise UnguardedLoopSeq
    else
      match Lazy.force w with
      | SeqRecVars [ w' ] -> force_recvar (w :: hist) w'
      | s -> s

  let rec partial_force : type x. x t -> x t = function
    | SeqCons (hd, tl) ->
        let tl =
          try partial_force tl
          with UnguardedLoopSeq ->
            (* we do not raise exception here;
             * in recursion, an unguarded loop will occur in the last part of the sequence.
             * when one tries to take head/tail of SeqBottom, an exception will be raised.
             *)
            SeqBottom
        in
        ignore (Mergeable.generate hd);
        SeqCons (hd, tl)
    | SeqRecVars [] -> assert false
    | SeqRecVars (d :: ds as dss) ->
        partial_force
          (List.fold_left seq_merge (force_recvar dss d)
             (List.map (force_recvar dss) ds))
    | SeqFinish -> SeqFinish
    | SeqBottom -> SeqBottom
end

module Local : sig
  type ('a, 'buf) inp = ('a, 'buf) Inp.inp
  type close = Close.close

  val receive : ('a, 'buf) inp -> 'a Lwt.t
  val close : close -> unit Lwt.t
end = struct
  include Inp
  include Out
  include Close
end

module Global
    (*        : sig
     *   open Close
     *   open Inp
     *   open Out
     *
     *   val fix : ('a Seq.t -> 'a Seq.t) -> 'a Seq.t
     *   val finish : (<c : (vec_all_empty -> close) * 'a > as 'a) Seq.t
     *
     *   val choice_at :
     *         (_, _, unit * ('ksA -> 'epA) * 'g12 * 'g3, _*_*_*_) role ->
     *         ('epA, 'epA1, 'epA2) disj_merge ->
     *         (_, _, ('ksA -> 'epA1) * unit * 'g1 * 'g12, _*_*_*_) role * 'g1 Seq.t ->
     *         (_, _, ('ksA -> 'epA2) * unit * 'g2 * 'g12,  _*_*_*_) role * 'g2 Seq.t ->
     *         'g3 Seq.t
     *
     *   val choice_req_at :
     *         (_, _, unit * ('ksA -> 'kA -> 'epA) * 'g12 * 'g3, _*_*_*_) role ->
     *         ('epA, 'epA1, 'epA2) disj_merge ->
     *         (_, _, ('ksA -> 'kA -> 'epA1) * unit * 'g1 * 'g12, _*_*_*_) role * 'g1 Seq.t ->
     *         (_, _, ('ksA -> 'kA -> 'epA2) * unit * 'g2 * 'g12,  _*_*_*_) role * 'g2 Seq.t ->
     *         'g3 Seq.t
     *
     *   val (-->) :
     *         (< .. > as 'rA, ([>  ] as 'var, 'buf) inp, ('ksA vec -> 'epA) * ('ksA vec -> 'rB) * 'g1 * 'g2, 'kB * 'kB * 'ksB * 'ksB) role ->
     *         (< .. > as 'rB, < .. > as 'obj, ('ksB vec -> 'epB) * ('ksB vec -> 'rA) * 'g0 * 'g1, 'kA * 'kA * 'ksA * 'ksA) role ->
     *         ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel -> 'g0 Seq.t -> 'g2 Seq.t
     *
     *   val disconnect :
     *         (_, _, ('ksA1 vec -> 'epA) * ('ksA0 vec -> <disconnect:'epA Lwt.t>) * 'g1 * 'g2, 'kB * unit * 'ksB0 * 'ksB1) role ->
     *         (_, _, ('ksB1 vec -> 'epB) * ('ksB0 vec -> <disconnect:'epB Lwt.t>) * 'g0 * 'g1, 'kA * unit * 'ksA0 * 'ksA1) role ->
     *         'g0 Seq.t -> 'g2 Seq.t
     *
     *   val (-!->) :
     *         (< .. > as 'rA, ([>  ] as 'var, 'buf) inp, ('ksA2 vec -> 'epA) * ('ksA1 vec -> 'kA -> 'rB) * 'g1 * 'g2, unit * 'kB * 'ksB1 * 'ksB2) role ->
     *         (< .. > as 'rB, < .. > as 'obj, ('ksB2 vec -> 'epB) * ('ksB1 vec -> 'kB -> 'rA) * 'g0 * 'g1, unit * 'kA * 'ksA1 * 'ksA2) role ->
     *         ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel -> 'g0 Seq.t -> 'g2 Seq.t
     *
     *   val (-?->) :
     *         (< .. > as 'rA, ([>  ] as 'var, 'buf) inp, ('ksA2 vec -> 'epA) * ('ksA1 vec -> 'rB) * 'g1 * 'g2, 'kB * unit * 'ksB1 * 'ksB2) role ->
     *         (< .. > as 'rB, < .. > as 'obj, ('ksB2 vec -> 'epB) * ('ksB1 vec -> 'rA) * 'g0 * 'g1, 'kA * unit * 'ksA1 * 'ksA2) role ->
     *         ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel -> 'g0 Seq.t -> 'g2 Seq.t
     *
     *   (\** forces delayed merges. *\)
     *   val gen : 'a Seq.t -> 'a Seq.t
     *
     *   val get_ch :
     *     (_, _, (vec_all_empty -> 'ep) * _ * 'xs * _, _*_*_*_) role -> 'xs Seq.t -> 'ep
     * end *) =
struct
  open Inp
  open Out
  open Discon

  let fix f =
    let rec body = lazy (f (Seq.recvar body)) in
    Lazy.force body

  let finish = Seq.all_closed

  let munit =
    Mergeable.make ~hook:(Lazy.from_val ())
      ~mergefun:(fun _ _ -> ())
      ~valuefun:(fun _ -> ())

  let lift_merge :
        'lr 'l 'r 'ks.
        ('lr, 'l, 'r) disj_merge ->
        ('ks -> 'lr, 'ks -> 'l, 'ks -> 'r) disj_merge =
   fun mrg ->
    {
      disj_merge = (fun l r ks -> mrg.disj_merge (l ks) (r ks));
      disj_splitL = (fun lr ks -> mrg.disj_splitL (lr ks));
      disj_splitR = (fun lr ks -> mrg.disj_splitR (lr ks));
    }

  let lift_merge2 :
        'lr 'l 'r 'ks.
        ('lr, 'l, 'r) disj_merge ->
        ('ks -> 'k -> 'lr, 'ks -> 'k -> 'l, 'ks -> 'k -> 'r) disj_merge =
   fun mrg ->
    {
      disj_merge = (fun l r ks k -> mrg.disj_merge (l ks k) (r ks k));
      disj_splitL = (fun lr ks k -> mrg.disj_splitL (lr ks k));
      disj_splitR = (fun lr ks k -> mrg.disj_splitR (lr ks k));
    }

  let choice_at :
        'epA 'epA1 'epA2 'g1 'g2 'g12 'g3.
        (_, _, unit * ('ksA -> 'epA) * 'g12 * 'g3, _) role ->
        ('epA, 'epA1, 'epA2) disj_merge ->
        (_, _, ('ksA -> 'epA1) * unit * 'g1 * 'g12, _) role * 'g1 Seq.t ->
        (_, _, ('ksA -> 'epA2) * unit * 'g2 * 'g12, _) role * 'g2 Seq.t ->
        'g3 Seq.t =
   fun (Role rA0) mrg (Role rA1, g1) (Role rA2, g2) ->
    let epA1, epA2 =
      (Seq.lens_get rA1.role_index g1, Seq.lens_get rA2.role_index g2)
    in
    let g1, g2 =
      ( Seq.lens_put rA1.role_index g1 munit,
        Seq.lens_put rA2.role_index g2 munit )
    in
    let epA = Mergeable.make_disj_merge (lift_merge mrg) epA1 epA2 in
    let g = Seq.seq_merge g1 g2 in
    let g = Seq.lens_put rA0.role_index g epA in
    g

  let choice_req_at (Role rA0) mrg (Role rA1, g1) (Role rA2, g2) =
    let epA1, epA2 =
      (Seq.lens_get rA1.role_index g1, Seq.lens_get rA2.role_index g2)
    in
    let g1, g2 =
      ( Seq.lens_put rA1.role_index g1 munit,
        Seq.lens_put rA2.role_index g2 munit )
    in
    let epA = Mergeable.make_disj_merge (lift_merge2 mrg) epA1 epA2 in
    let g = Seq.seq_merge g1 g2 in
    let g = Seq.lens_put rA0.role_index g epA in
    g

  let ( --> ) :
        'rA 'rB 'epA 'epB 'ksA 'ksB 'kA 'obj 'var 'buf.
        ( (< .. > as 'rA),
          (([> ] as 'var), 'buf) inp,
          ('ksA vec -> 'epA) * ('ksA vec -> 'rB) * 'g1 * 'g2,
          'kB * 'kB * 'ksB * 'ksB )
        role ->
        ( (< .. > as 'rB),
          (< .. > as 'obj),
          ('ksB vec -> 'epB) * ('ksB vec -> 'rA) * 'g0 * 'g1,
          'kA * 'kA * 'ksA * 'ksA )
        role ->
        ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel ->
        'g0 Seq.t ->
        'g2 Seq.t =
   fun (Role rA) (Role rB) slabel g ->
    let epB = Seq.lens_get rB.role_index g in
    let epB = create_inp rA.role_index0 slabel epB in
    let epB = Mergeable.wrap_label_fun rA.role_label epB in
    let g = Seq.lens_put rB.role_index g epB in
    let epA = Seq.lens_get rA.role_index g in
    let epA = create_out rB.role_index0 slabel epA in
    let epA = Mergeable.wrap_label_fun rB.role_label epA in
    Seq.lens_put rA.role_index g epA

  let disconnect :
        'epA 'ksA0 'ksA1 'ksB0 'ksB1 'kA 'kB 'g1 'g0.
        ( _,
          _,
          ('ksA1 vec -> 'epA)
          * ('ksA0 vec -> < disconnect : 'epA Lwt.t >)
          * 'g1
          * 'g2,
          'kB * unit * 'ksB0 * 'ksB1 )
        role ->
        ( _,
          _,
          ('ksB1 vec -> 'epB)
          * ('ksB0 vec -> < disconnect : 'epB Lwt.t >)
          * 'g0
          * 'g1,
          'kA * unit * 'ksA0 * 'ksA1 )
        role ->
        'g0 Seq.t ->
        'g2 Seq.t =
   fun (Role rA) (Role rB) g ->
    let epB = Seq.lens_get rB.role_index g in
    let epB = create_discon rA.role_index0 epB in
    let g = Seq.lens_put rB.role_index g epB in
    let epA = Seq.lens_get rA.role_index g in
    let epA = create_discon rB.role_index0 epA in
    Seq.lens_put rA.role_index g epA

  let ( -!-> ) :
        'rA 'rB 'epA 'epB 'ksA1 'ksA2 'ksB1 'ksB2 'kA 'obj 'var 'buf.
        ( (< .. > as 'rA),
          (([> ] as 'var), 'buf) inp,
          ('ksA2 vec -> 'epA) * ('ksA1 vec -> 'kA -> 'rB) * 'g1 * 'g2,
          unit * 'kB * 'ksB1 * 'ksB2 )
        role ->
        ( (< .. > as 'rB),
          (< .. > as 'obj),
          ('ksB2 vec -> 'epB) * ('ksB1 vec -> 'kB -> 'rA) * 'g0 * 'g1,
          unit * 'kA * 'ksA1 * 'ksA2 )
        role ->
        ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel ->
        'g0 Seq.t ->
        'g2 Seq.t =
   fun (Role rA) (Role rB) slabel g ->
    let epB = Seq.lens_get rB.role_index g in
    let epB = create_inp_conn rA.role_index0 slabel epB in
    let epB = Mergeable.wrap_label_fun2 rA.role_label epB in
    let g = Seq.lens_put rB.role_index g epB in
    let epA = Seq.lens_get rA.role_index g in
    let epA = create_out_conn rB.role_index0 slabel epA in
    let epA = Mergeable.wrap_label_fun2 rB.role_label epA in
    Seq.lens_put rA.role_index g epA

  let ( -?-> ) :
        'rA 'rB 'epA 'epB 'ksA1 'ksA2 'ksB1 'ksB2 'kA 'obj 'var 'buf.
        ( (< .. > as 'rA),
          (([> ] as 'var), 'buf) inp,
          ('ksA2 vec -> 'epA) * ('ksA1 vec -> 'rB) * 'g1 * 'g2,
          'kB * unit * 'ksB1 * 'ksB2 )
        role ->
        ( (< .. > as 'rB),
          (< .. > as 'obj),
          ('ksB2 vec -> 'epB) * ('ksB1 vec -> 'rA) * 'g0 * 'g1,
          'kA * unit * 'ksA1 * 'ksA2 )
        role ->
        ('obj, 'var, 'v -> 'epA, 'v * 'epB, 'kA, 'kB, 'buf) slabel ->
        'g0 Seq.t ->
        'g2 Seq.t =
   fun (Role rA) (Role rB) slabel g ->
    let epB = Seq.lens_get rB.role_index g in
    let epB = create_inp_discon rA.role_index0 slabel epB in
    let epB = Mergeable.wrap_label_fun rA.role_label epB in
    let g = Seq.lens_put rB.role_index g epB in
    let epA = Seq.lens_get rA.role_index g in
    let epA = create_out_discon rB.role_index0 slabel epA in
    let epA = Mergeable.wrap_label_fun rB.role_label epA in
    Seq.lens_put rA.role_index g epA

  let gen g = Seq.partial_force g

  let get_ch (Role r) g =
    Mergeable.generate (Seq.lens_get r.role_index g) HList.vec_all_empty
end

module Util = struct
  let a =
    Role
      {
        role_label =
          {
            make_obj =
              (fun v ->
                object
                  method role_A = v
                end);
            call_obj = (fun o -> o#role_A);
          };
        role_index = Zero;
        role_index0 = Zero;
      }

  let b =
    Role
      {
        role_label =
          {
            make_obj =
              (fun v ->
                object
                  method role_B = v
                end);
            call_obj = (fun o -> o#role_B);
          };
        role_index = Succ Zero;
        role_index0 = Succ Zero;
      }

  let c =
    Role
      {
        role_label =
          {
            make_obj =
              (fun v ->
                object
                  method role_C = v
                end);
            call_obj = (fun o -> o#role_C);
          };
        role_index = Succ (Succ Zero);
        role_index0 = Succ (Succ Zero);
      }

  let d =
    Role
      {
        role_label =
          {
            make_obj =
              (fun v ->
                object
                  method role_D = v
                end);
            call_obj = (fun o -> o#role_D);
          };
        role_index = Succ (Succ (Succ Zero));
        role_index0 = Succ (Succ (Succ Zero));
      }

  let msg =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method msg = f
              end);
          call_obj = (fun o -> o#msg);
        };
      var = (fun v -> `msg v);
    }

  let left =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method left = f
              end);
          call_obj = (fun o -> o#left);
        };
      var = (fun v -> `left v);
    }

  let right =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method right = f
              end);
          call_obj = (fun o -> o#right);
        };
      var = (fun v -> `right v);
    }

  let middle =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method middle = f
              end);
          call_obj = (fun o -> o#middle);
        };
      var = (fun v -> `middle v);
    }

  let ping =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method ping = f
              end);
          call_obj = (fun o -> o#ping);
        };
      var = (fun v -> `ping v);
    }

  let pong =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method pong = f
              end);
          call_obj = (fun o -> o#pong);
        };
      var = (fun v -> `pong v);
    }

  let fini =
    {
      obj =
        {
          make_obj =
            (fun f ->
              object
                method fini = f
              end);
          call_obj = (fun o -> o#fini);
        };
      var = (fun v -> `fini v);
    }

  let left_or_right =
    {
      disj_merge =
        (fun l r ->
          object
            method left = l#left
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }

  let right_or_left =
    {
      disj_merge =
        (fun l r ->
          object
            method right = l#right
            method left = r#left
          end);
      disj_splitL = (fun lr -> (lr :> < right : _ >));
      disj_splitR = (fun lr -> (lr :> < left : _ >));
    }

  let to_ m r1 r2 r3 =
    let ( ! ) (Role x) = x.role_label in
    {
      disj_merge =
        (fun l r ->
          !r1.make_obj (m.disj_merge (!r2.call_obj l) (!r3.call_obj r)));
      disj_splitL = (fun lr -> !r2.make_obj (m.disj_splitL @@ !r1.call_obj lr));
      disj_splitR = (fun lr -> !r3.make_obj (m.disj_splitR @@ !r1.call_obj lr));
    }

  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c

  let left_middle_or_right =
    {
      disj_merge =
        (fun l r ->
          object
            method left = l#left
            method middle = l#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ ; middle : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }

  let left_or_middle =
    {
      disj_merge =
        (fun l r ->
          object
            method left = l#left
            method middle = r#middle
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < middle : _ >));
    }

  let left_or_middle_right =
    {
      disj_merge =
        (fun l r ->
          object
            method left = l#left
            method middle = r#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < left : _ >));
      disj_splitR = (fun lr -> (lr :> < middle : _ ; right : _ >));
    }

  let middle_or_right =
    {
      disj_merge =
        (fun l r ->
          object
            method middle = l#middle
            method right = r#right
          end);
      disj_splitL = (fun lr -> (lr :> < middle : _ >));
      disj_splitR = (fun lr -> (lr :> < right : _ >));
    }
end

include Global
include Local
include Util
