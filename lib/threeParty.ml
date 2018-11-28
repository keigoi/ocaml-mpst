include Base
include Session
include Global
include Util.Labels

let lval = Lazy.from_val

let obj3 a b c =
  lval (lval a, lval b, lval c)
         
let finish =
  lazy begin
    lazy (Close : ((_ * _ * _), close) prot),
    lazy (Close : ((_ * _ * _), close) prot),
    lazy (Close : ((_ * _ * _), close) prot)
  end
  
         
let finish_ =
  obj3
    (Close : (unit lazy_t * unit lazy_t * unit lazy_t, close) prot)
    (Close : (unit lazy_t * unit lazy_t * unit lazy_t, close) prot)
    (Close : (unit lazy_t * unit lazy_t * unit lazy_t, close) prot)

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
  
  

let a = {role=A; lens={get=(fun (lazy c) -> fst_ c); put=(fun s v -> lazy (lval v, lazy (snd_ @@ force s), lazy (thd_ @@ force s)))}}
let b = {role=B; lens={get=(fun (lazy c) -> snd_ c); put=(fun s v -> lazy (lazy (fst_ @@ force s), lval v, lazy (thd_ @@ force s)))}}
let c = {role=C; lens={get=(fun (lazy c) -> thd_ c); put=(fun s v -> lazy (lazy (fst_ @@ force s), lazy (snd_ @@ force s), lval v))}}

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
