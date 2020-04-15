module Ex0 : sig
  type nil = [`cons of unit * 'a] as 'a
  type _ seq0 =
    | Cons : 'hd * 'tl seq0 -> [`cons of 'hd * 'tl] seq0
    | Nil : nil seq0
    
  type (_,_,_,_) idx =
    Zero : ([`cons of 't * 'tl], 't, [`cons of 'u * 'tl], 'u) idx
  | Succ : ('tl1, 't, 'tl2, 'u) idx -> ([`cons of 'hd * 'tl1], 't, [`cons of 'hd * 'tl2], 'u) idx
  val seq_get : 'xs seq0 -> ('xs, 't, 'ys, 'u) idx -> 't
  val seq_put : 'xs seq0 -> ('xs, 't, 'ys, 'u) idx -> 'u -> 'ys seq0
    
  type ('obj, 'mt) method_ =
    {make_obj: 'mt -> 'obj;
     call_obj: 'obj -> 'mt}
    
  type ('obj, 'ot, 'var, 'vt) label =
    {obj: ('obj, 'ot) method_;
     var: 'vt -> 'var}
    
  type ('g0, 't, 'g1, 'u, 'obj, 'mt) role =
    {role_label : ('obj,'mt) method_;
     role_index : ('g0,'t,'g1,'u) idx}
    
  type ('v, 't) out
  type 'v inp = 'v Event.event
              
  val ( --> ) : ('g1, 'ti, 'g2, 'ui, 'ri as 'uj, 'var inp) role ->
                ('g0, 'tj, 'g1, 'uj, 'rj as 'ui, 'obj) role ->
                ('obj, ('v, 'ti) out, 'var, 'v * 'tj) label ->
                'g0 seq0 -> 'g2 seq0
  val mixed : [ `cons of int * [ `cons of string * [ `cons of bool * nil ] ] ] seq0
end= struct
  type nil = [`cons of unit * 'a] as 'a
  type _ seq0 =
    | Cons : 'hd * 'tl seq0 -> [`cons of 'hd * 'tl] seq0
    | Nil : nil seq0

  let mixed :
        [ `cons of int * [ `cons of string * [ `cons of bool * nil ] ] ] seq0
    = Cons(100, Cons("abc", Cons(true, Nil)))
  type (_,_,_,_) idx =
    Zero : ([`cons of 't * 'tl], 't, [`cons of 'u * 'tl], 'u) idx
  | Succ : ('tl1, 't, 'tl2, 'u) idx -> ([`cons of 'hd * 'tl1], 't, [`cons of 'hd * 'tl2], 'u) idx

  let rec seq_head : type hd tl. [`cons of hd * tl] seq0 -> hd =
    function
    | Cons(hd,_) -> hd
    | Nil -> ()

  let rec seq_tail : type hd tl. [`cons of hd * tl] seq0 -> tl seq0 =
    function
    | Cons(_,tl) -> tl
    | Nil -> Nil

  let rec seq_get : type a b xs ys. xs seq0 -> (xs, a, ys, b) idx -> a = fun xs ln ->
    match ln with
    | Zero -> seq_head xs
    | Succ ln' -> seq_get (seq_tail xs) ln'

  let rec seq_put : type a b xs ys. xs seq0 -> (xs,a,ys,b) idx -> b -> ys seq0 =
    fun xs ln b ->
    match ln with
    | Zero -> Cons(b, seq_tail xs)
    | Succ ln' -> Cons(seq_head xs, seq_put (seq_tail xs) ln' b)

  type ('obj, 'mt) method_ =
    {make_obj: 'mt -> 'obj;
     call_obj: 'obj -> 'mt}
    
  type ('obj, 'ot, 'var, 'vt) label =
    {obj: ('obj, 'ot) method_;
     var: 'vt -> 'var}
    
  type ('g0, 't, 'g1, 'u, 'obj, 'mt) role =
    {role_label : ('obj,'mt) method_;
     role_index : ('g0,'t,'g1,'u) idx}
    
  let mixed2 = seq_put mixed  Zero "abc"
  let mixed3 = seq_put mixed2 (Succ (Succ (Succ Zero))) 1.0

  type ('v, 't) out = {chan: 'v Event.channel; cont: 't}
  type 'v inp = 'v Event.event

  let ( --> ) r_i r_j lab g0 =
    (*   : 'ti 'ri 'tj 'rj 'g0 'g1 'g2 'var 'obj 'v.
     *     ('ti mer, 'si mer, 'g1, 'g2, 'ri as 'sj, 'var inp) role ->
     *     ('tj mer, 'sj mer, 'g0, 'g1, 'rj as 'si, 'obj) role ->
     *     ('obj, ('v, 'ti) out, 'var, 'v * 'tj) label ->
     *     'g0 seq -> 'g2 seq
     * = fun r_i r_j lab g0 -> *)
    let s = Event.new_channel () in
    let c_j = seq_get g0 r_j.role_index in
    let inp = Event.wrap (Event.receive s) (fun v -> lab.var (v, c_j)) in
    let c_j' = r_i.role_label.make_obj inp in
    let g1 = seq_put g0 r_j.role_index c_j' in
    let c_i = seq_get g1 r_i.role_index in
    let out = lab.obj.make_obj {chan=s; cont=c_i} in
    let c_i' = r_j.role_label.make_obj out in
    let g2 = seq_put g1 r_i.role_index c_i' in
    g2
end


module Ex1 : sig
  type 't mergeable = {value: 't; merge:'t -> 't -> 't}

  type nil = [`cons of unit * 'a] as 'a
  type _ seq =
    | Cons : 'hd mergeable * 'tl seq -> [`cons of 'hd * 'tl] seq
    | Nil : nil seq

  type (_,_,_,_) idx =
    Zero : ([`cons of 't * 'tl], 't, [`cons of 'u * 'tl], 'u) idx
  | Succ : ('tl1, 't, 'tl2, 'u) idx -> ([`cons of 'hd * 'tl1], 't, [`cons of 'hd * 'tl2], 'u) idx

  val seq_get : 'ts seq -> ('ts, 't, 'us, 'u) idx -> 't mergeable
  val seq_put : 'ts seq -> ('ts, 't, 'us, 'u) idx -> 'u mergeable -> 'us seq

  type ('obj, 'mt) method_ =
    {make_obj: 'mt -> 'obj;
     call_obj: 'obj -> 'mt}

  type ('obj, 'ot, 'var, 'vt) label =
    {obj: ('obj, 'ot) method_;
     var: 'vt -> 'var}

  type ('lr, 'l, 'r) disj =
    {disj_merge: 'l -> 'r -> 'lr;
     disj_splitL: 'lr -> 'l;
     disj_splitR: 'lr -> 'r;
    }

  type ('g0, 't, 'g1, 'u, 'obj, 'mt) role =
    {role_label : ('obj,'mt) method_;
     role_index : ('g0,'t,'g1,'u) idx}

  type ('v, 't) out
  type 'v inp = 'v Event.event

  val ( --> ) : ('g1, 'ti, 'g2, 'ui, 'ri as 'uj, 'var inp) role ->
                ('g0, 'tj, 'g1, 'uj, 'rj as 'ui, 'obj) role ->
                ('obj, ('v, 'ti) out, 'var, 'v * 'tj) label ->
                'g0 seq -> 'g2 seq

  val choice_at :
    ('g1, unit, 'g2, 'tlr, _, _) role ->
    ('tlr, 'tl, 'tr) disj ->
    ('gl, 'tl, 'g1, unit, _, _) role * 'gl seq ->
    ('gr, 'tr, 'g1, unit, _, _) role * 'gr seq -> 'g2 seq

end = struct

  type 't mergeable = {value: 't; merge:'t -> 't -> 't}

  let merge_unit () () = ()

  let munit =
    {value=(); merge=merge_unit}

  type nil = [`cons of unit * 'a] as 'a
  type _ seq =
  | Cons : 'hd mergeable * 'tl seq -> [`cons of 'hd * 'tl] seq
  | Nil : nil seq

  type (_,_,_,_) idx =
    Zero : ([`cons of 't * 'tl], 't, [`cons of 'u * 'tl], 'u) idx
  | Succ : ('tl1, 't, 'tl2, 'u) idx -> ([`cons of 'hd * 'tl1], 't, [`cons of 'hd * 'tl2], 'u) idx

  let rec seq_head : type hd tl. [`cons of hd * tl] seq -> hd mergeable =
    function
    | Cons(hd,_) -> hd
    | Nil -> munit

  let rec seq_tail : type hd tl. [`cons of hd * tl] seq -> tl seq =
    function
    | Cons(_,tl) -> tl
    | Nil -> Nil

  let rec seq_get : type a b xs ys. xs seq -> (xs, a, ys, b) idx -> a mergeable = fun xs ln ->
    match ln with
    | Zero -> seq_head xs
    | Succ ln' -> seq_get (seq_tail xs) ln'

  let rec seq_put : type a b xs ys. xs seq -> (xs,a,ys,b) idx -> b mergeable -> ys seq =
    fun xs ln b ->
    match ln with
    | Zero -> Cons(b, seq_tail xs)
    | Succ ln' -> Cons(seq_head xs, seq_put (seq_tail xs) ln' b)

  type ('obj, 'mt) method_ =
    {make_obj: 'mt -> 'obj;
     call_obj: 'obj -> 'mt}

  type ('obj, 'ot, 'var, 'vt) label =
    {obj: ('obj, 'ot) method_;
     var: 'vt -> 'var}

  type ('lr, 'l, 'r) disj =
    {disj_merge: 'l -> 'r -> 'lr;
     disj_splitL: 'lr -> 'l;
     disj_splitR: 'lr -> 'r;
    }
  
  type ('g0, 't, 'g1, 'u, 'obj, 'mt) role =
    {role_label : ('obj,'mt) method_;
     role_index : ('g0,'t,'g1,'u) idx}
    
  type ('v, 't) out = {chan: 'v Event.channel ref; cont: 't mergeable}
  type 'v inp = 'v Event.event
  
  let merge m1 m2 = {value=m1.merge m1.value m2.value; merge=m1.merge}


  let merge_inp role_label c1 c2 =
    let inp1 = role_label.call_obj c1 in
    let inp2 = role_label.call_obj c2 in
    let inp12 = Event.choose [inp1; inp2] in
    role_label.make_obj inp12
  
  let merge_out role_label out_label c1 c2 =
    let out1 = out_label.call_obj (role_label.call_obj c1) in
    let out2 = out_label.call_obj (role_label.call_obj c2) in
    out2.chan := !(out1.chan);
    let out12 = {chan=out1.chan; cont = merge out1.cont out2.cont} in
    role_label.make_obj (out_label.make_obj out12)

  let ( --> ) 
      (* : 'ti 'ri 'tj 'rj 'g0 'g1 'g2 'var 'obj 'v.
       *   ('ti mer, 'si mer, 'g1, 'g2, 'ri as 'sj, 'var inp) role ->
       *   ('tj mer, 'sj mer, 'g0, 'g1, 'rj as 'si, 'obj) role ->
       *   ('obj, ('v, 'ti) out, 'var, 'v * 'tj) label ->
       *   'g0 seq -> 'g2 seq *)
    = fun r_i r_j lab g0 ->
    let s = ref (Event.new_channel ()) in
    let c_j = (seq_get g0 r_j.role_index).value in
    let inp = Event.guard (fun () -> Event.wrap (Event.receive !s) (fun v -> lab.var (v, c_j))) in
    let c_j' = r_i.role_label.make_obj inp in
    let m_j' = {value=c_j'; merge=merge_inp r_i.role_label} in
    let g1 = seq_put g0 r_j.role_index m_j' in
    let m_i = seq_get g1 r_i.role_index in
    let out = lab.obj.make_obj {chan=s; cont=m_i} in
    let c_i' = r_j.role_label.make_obj out in
    let m_i' = {value=c_i'; merge=merge_out r_j.role_label lab.obj} in
    let g2 = seq_put g1 r_i.role_index m_i' in
    g2

  let rec seq_merge : type t. t seq -> t seq -> t seq = fun gl gr ->
    match gl,gr with
    | Cons(cl,gl'), Cons(cr,gr') ->
       Cons({merge=cl.merge; value=cl.merge cl.value cr.value}, seq_merge gl' gr')
    | Nil, _ ->
       Nil
    | _, Nil ->
       Nil

  let merge_disj disj mergefun_l mergefun_r c1 c2 =
    disj.disj_merge
      (mergefun_l (disj.disj_splitL c1) (disj.disj_splitL c2))
      (mergefun_r (disj.disj_splitR c1) (disj.disj_splitR c2))

  let choice_at
      : 'g1 'g2 'gl 'gr 'tlr 'tl 'tr.
        ('g1, unit, 'g2, 'tlr, _, _) role ->
        ('tlr, 'tl, 'tr) disj ->
        ('gl, 'tl, 'g1, unit, _, _) role * 'gl seq ->
        ('gr, 'tr, 'g1, unit, _, _) role * 'gr seq -> 'g2 seq
     = fun r disj (r_l,g_l) (r_r,g_r) ->
    let c_l, c_r = seq_get g_l r_l.role_index, seq_get g_r r_r.role_index in
    let g_l', g_r' = seq_put g_l r_l.role_index munit, seq_put g_r r_r.role_index munit in
    let c =
      {merge=merge_disj disj c_l.merge c_r.merge;
       value=disj.disj_merge c_l.value c_r.value}
    in
    let g' = seq_merge g_l' g_r' in
    let g'' = seq_put g' r.role_index c in
    g''
  
end

