include Base
include Session
include Global
include Util.Labels

let lval = Lazy.from_val

type ('a,'b,'c) tup = ('a lazy_t * 'b lazy_t * 'c lazy_t)
         
let obj3 a b c : (_,_,_) tup lazy_t =
  lval (lval a, lval b, lval c)
         
let finish : (_,_,_) tup lazy_t =
  lazy begin
    lazy (Close : ((_ * _ * _), close) prot),
    lazy (Close : ((_ * _ * _), close) prot),
    lazy (Close : ((_ * _ * _), close) prot)
  end
  
         
let finish_ : (_,_,_) tup lazy_t =
  obj3
    (Close : ((unit, unit, unit) tup, close) prot)
    (Close : ((unit, unit, unit) tup, close) prot)
    (Close : ((unit, unit, unit) tup, close) prot)

type a = A
type b = B
type c = C

let force = Lazy.force

let fst_ (x,_,_) = force x
let snd_ (_,x,_) = force x
let thd_ (_,_,x) = force x

let merge3 c1 c2 =
  lazy begin
    (lazy (Internal.merge (fst_ @@ force c1) (fst_  @@ force c2))),
    (lazy (Internal.merge (snd_ @@ force c1) (snd_  @@ force c2))),
    (lazy (Internal.merge (thd_ @@ force c1) (thd_  @@ force c2)))
  end
  
  

let a : (a, 'x, 'y, ('x,'b,'c) tup, ('y,'b,'c) tup) role = {role=A; lens={get=(fun (lazy c) -> fst_ c); put=(fun s v -> lazy (lval v, lazy (snd_ @@ force s), lazy (thd_ @@ force s)))}}
let b : (b, 'x, 'y, ('a,'x,'c) tup, ('a,'y,'c) tup) role = {role=B; lens={get=(fun (lazy c) -> snd_ c); put=(fun s v -> lazy (lazy (fst_ @@ force s), lval v, lazy (thd_ @@ force s)))}}
let c : (c, 'x, 'y, ('a,'b,'x) tup, ('a,'b,'y) tup) role = {role=C; lens={get=(fun (lazy c) -> thd_ c); put=(fun s v -> lazy (lazy (fst_ @@ force s), lazy (snd_ @@ force s), lval v))}}

let get_sess r m =
  let p = r.lens.get m in
  Sess((lval Memory, lval Memory, lval Memory), p)

let get_sess_ r m =
  let p = r.lens.get m in
  Sess((lval (), lval (), lval ()), p)

let get_sess__ r (a, b, c) m =
  let p = r.lens.get m in
  Sess((lval a, lval b, lval c), p)


let choice_at x = Global.choice_at merge3 x
