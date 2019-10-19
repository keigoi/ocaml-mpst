open Base
module Make(EP:S.ENDPOINTS) = struct

  type _ t =
    (* hidden *)
    | SeqCons : 'hd EP.t * 'tl t -> [`cons of 'hd * 'tl] t
    | SeqRepeat : int * (int -> 'a EP.t) -> ([`cons of 'a * 'tl] as 'tl) t
    | SeqRecVars : 'a t lazy_t list -> 'a t
    | SeqBottom : 'a t

  exception UnguardedLoopSeq

  let recvar l = SeqRecVars [l]
  let repeat i f = SeqRepeat(i,f)

  let rec seq_head : type hd tl. [`cons of hd * tl] t -> hd EP.t =
    function
    | SeqCons(hd,_) -> hd
    | SeqRecVars ds -> EP.make_merge_list (List.map seqvar_head ds)
    | SeqRepeat(i,f) -> f i
    | SeqBottom -> raise UnguardedLoopSeq
  and seqvar_head : type hd tl. [`cons of hd * tl] t lazy_t -> hd EP.t = fun d ->
    EP.make_recvar (lazy (seq_head (Lazy.force d)))

  let rec seq_tail : type hd tl. [`cons of hd * tl] t -> tl t =
    function
    | SeqCons(_,tl) -> tl
    | SeqRecVars ds -> SeqRecVars(List.map seqvar_tail ds)
    | SeqRepeat(i,f) -> SeqRepeat(i+1,f)
    | SeqBottom -> raise UnguardedLoopSeq
  and seqvar_tail : type hd tl. [`cons of hd * tl] t lazy_t -> tl t lazy_t = fun d ->
    lazy (seq_tail (Lazy.force d))

  let rec lens_get : type a b xs ys. (a, b, xs, ys) lens -> xs t -> a EP.t = fun ln xs ->
    match ln with
    | Zero -> seq_head xs
    | Succ ln' -> lens_get ln' (seq_tail xs)

  let rec lens_put : type a b xs ys. (a,b,xs,ys) lens -> xs t -> b EP.t -> ys t =
    fun ln xs b ->
    match ln with
    | Zero -> SeqCons(b, seq_tail xs)
    | Succ ln' -> SeqCons(seq_head xs, lens_put ln' (seq_tail xs) b)

  let rec seq_merge : type x. x t -> x t -> x t = fun l r ->
    match l,r with
    | SeqCons(_,_), _ ->
       let hd = EP.make_merge (seq_head l) (seq_head r) in
       let tl = seq_merge (seq_tail l) (seq_tail r) in
       SeqCons(hd, tl)
    | _, SeqCons(_,_) -> seq_merge r l
    (* delayed constructors are left as-is *)
    | SeqRecVars(us1), SeqRecVars(us2) -> SeqRecVars(us1 @ us2)
    (* repeat *)
    | SeqRepeat(i,f), _ -> SeqRepeat(i,f)
    | _, SeqRepeat(i,f) -> SeqRepeat(i,f)
    (* bottom *)
    | SeqBottom,_  -> SeqBottom
    | _, SeqBottom -> SeqBottom

  let rec force_recvar : type x. x t lazy_t list -> x t lazy_t -> x t =
    fun hist w ->
    if Common.find_physeq hist w then begin
        raise UnguardedLoopSeq
      end else begin
        match Lazy.force w with
        | SeqRecVars [w'] -> force_recvar (w::hist) w'
        | s -> s
      end

  let rec resolve_merge : type x. x t -> x t =
    function
    | SeqCons(hd,tl) ->
       let tl =
         try
           resolve_merge tl
         with
           UnguardedLoopSeq ->
           (* we do not raise exception here;
            * in recursion, an unguarded loop will occur in the last part of the sequence.
            * when one tries to take head/tail of SeqBottom, an exception will be raised.
            *)
           SeqBottom
       in
       SeqCons(hd, tl)
    | SeqRecVars [] -> assert false
    | SeqRecVars ((d::ds) as dss) ->
       resolve_merge
         (List.fold_left seq_merge (force_recvar dss d) (List.map (force_recvar dss) ds))
    | SeqRepeat(i,f) -> SeqRepeat(i,f)
    | SeqBottom -> SeqBottom

  let rec force_all : type x. x t -> unit = function
    | SeqCons(hd,tl) ->
       EP.force_merge hd;
       force_all tl
    | SeqRecVars [] -> assert false
    | SeqRecVars ((d::ds) as dss) ->
       force_all
         (List.fold_left seq_merge (force_recvar dss d) (List.map (force_recvar dss) ds))
    | SeqRepeat(_,_) -> ()
    | SeqBottom -> ()

  let rec int_of_lens : type a b c d. (a,b,c,d) lens -> int = function
    | Zero -> 0
    | Succ l -> int_of_lens l + 1

  let seq_merge_all : type x. x t list -> x t = function
    | [] -> failwith "seq_merge_all"
    | s::ss -> List.fold_left seq_merge s ss

  let rec effective_length : type x. x t -> int = function
    | SeqRepeat(_,_) -> 0
    | SeqBottom -> 0
    | SeqCons(_,_) as s -> 1 + effective_length (seq_tail s)
    | SeqRecVars ds -> effective_length (seq_merge_all (List.map Lazy.force ds))
end

