open Base
open Common

module Make(EP:S.ENDPOINT) = struct
module Mergeable = Mergeable.Make(EP)
type 'a mergeable = 'a Mergeable.t

type _ t =
  | SeqCons : 'hd Mergeable.t * 'tl t -> [`cons of 'hd * 'tl] t
  | SeqRepeat : int * (int -> 'a Mergeable.t) -> ([`cons of 'a * 'tl] as 'tl) t
  | SeqRecVars : 'a seqvar list -> 'a t
  | SeqBottom : 'x t
and 'a seqvar = 'a t lazy_t

type (_,_,_,_) lens =
  | Zero  : ('hd0, 'hd1, [`cons of 'hd0 * 'tl] t, [`cons of 'hd1 * 'tl] t) lens
  | Succ : ('a, 'b, 'tl0 t, 'tl1 t) lens
           -> ('a,'b, [`cons of 'hd * 'tl0] t, [`cons of 'hd * 'tl1] t) lens

let rec int_of_lens : type a b c d. (a,b,c,d) lens -> int = function
  | Zero -> 0
  | Succ l -> int_of_lens l + 1

exception UnguardedLoopSeq

let rec seq_head : type hd tl. [`cons of hd * tl] t -> hd Mergeable.t =
  function
  | SeqCons(hd,_) -> hd
  | SeqRecVars ds -> Mergeable.make_merge_list (List.map seqvar_head ds)
  | SeqRepeat(i,f) -> f i
  | SeqBottom -> raise UnguardedLoopSeq
and seqvar_head : type hd tl. [`cons of hd * tl] t lazy_t -> hd Mergeable.t = fun d ->
  Mergeable.make_recvar (lazy (seq_head (Lazy.force d)))

let rec seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
  function
  | SeqCons(_,tl) -> tl
  | SeqRecVars ds -> SeqRecVars(List.map seqvar_tail ds)
  | (SeqRepeat (i,f)) -> SeqRepeat(i+1,f)
  | SeqBottom -> raise UnguardedLoopSeq
and seqvar_tail : type hd tl. [`cons of hd * tl] t lazy_t -> tl t lazy_t = fun d ->
  lazy (seq_tail (Lazy.force d))

let rec get : type a b xs ys. (a, b, xs, ys) lens -> xs -> a Mergeable.t = fun ln xs ->
  match ln with
  | Zero -> seq_head xs
  | Succ ln' -> get ln' (seq_tail xs)

let rec put : type a b xs ys. (a,b,xs,ys) lens -> xs -> b Mergeable.t -> ys =
  fun ln xs b ->
  match ln with
  | Zero -> SeqCons(b, seq_tail xs)
  | Succ ln' -> SeqCons(seq_head xs, put ln' (seq_tail xs) b)

let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
  match l,r with
  | SeqCons(_,_), _ ->
     let hd = Mergeable.make_merge (seq_head l) (seq_head r) in
     let tl = seq_merge (seq_tail l) (seq_tail r) in
     SeqCons(hd, tl)
  | _, SeqCons(_,_) -> seq_merge r l
  (* delayed constructors are left as-is *)
  | SeqRecVars(us1), SeqRecVars(us2) -> SeqRecVars(us1 @ us2)
  (* repeat *)
  | SeqRepeat(_), _ -> l
  | _, SeqRepeat(_) -> r
  (* bottom *)
  | SeqBottom,_  -> raise UnguardedLoopSeq
  | _, SeqBottom -> raise UnguardedLoopSeq

let rec resolve_seqvar_ : type x. x t lazy_t list -> x t lazy_t -> x t =
  fun hist w ->
  if Common.find_physeq hist w then begin
      raise UnguardedLoopSeq
    end else begin
      match Lazy.force w with
      | SeqRecVars [w'] -> resolve_seqvar_ (w::hist) w'
      | s -> s
    end

let rec partial_force : type x. x t lazy_t list -> x t -> x t =
  fun hist ->
  function
  | SeqRecVars [d] ->
     (* recursion variable -- try to expand it *)
     partial_force [] (resolve_seqvar_ [] d)
  | SeqRecVars ds ->
     (* A choice between recursion variables -- do not try to resolve.
      * (will happen at after c of fix (fun t -> choice (a->b)t (a->b)t))
      * Mergeable.resolve_merge will resolve it later during get_ep
      *)
     SeqRecVars ds
  | SeqCons(hd,tl) ->
     let tl =
       try
         partial_force [] tl
       with
         UnguardedLoopSeq ->
         (* we do not raise exception here;
          * in recursion, an unguarded loop will occur in the last part of the sequence.
          * when one tries to take head/tail of SeqBottom, an exception will be raised.
          *)
         SeqBottom
     in
     SeqCons(hd, tl)
  | SeqRepeat(_) as xs -> xs
  | SeqBottom -> SeqBottom

let seq_merge_all : type x. x t list -> x t = function
  | [] -> failwith "seq_merge_all"
  | s::ss -> List.fold_left seq_merge s ss

let rec effective_length : type x. x t -> int = function
  | SeqRepeat(_) -> 0
  | SeqBottom -> 0
  | SeqCons(_,_) as s -> 1 + effective_length (seq_tail s)
  | SeqRecVars ds -> effective_length (seq_merge_all (List.map Lazy.force ds))
end
