include Base
include Session
include Global
include Util.Labels

let obj3 a b c =
  Mpst (lazy (object
              method a=Lazy.force a
              method b=Lazy.force b
              method c=Lazy.force c
            end))

let lval = Lazy.from_val
         
let finish =
  Mpst (lazy (object
              method a=Close
              method b=Close
              method c=Close
            end))

type a = A
type b = B
type c = C

let unmpst (Mpst (lazy o)) = o
                           
let merge3 c1 c2 =
  obj3
    (lazy (Internal.merge (unmpst c1)#a (unmpst c2)#a))
    (lazy (Internal.merge (unmpst c1)#b (unmpst c2)#b))
    (lazy (Internal.merge (unmpst c1)#c (unmpst c2)#c))

let a = {role=A; lens={get=(fun (Mpst (lazy c)) -> c#a); put=(fun s v -> obj3 (lval v) (lazy (unmpst s)#b) (lazy (unmpst s)#c))}}
let b = {role=B; lens={get=(fun (Mpst (lazy c)) -> c#b); put=(fun s v -> obj3 (lazy (unmpst s)#a) (lval v) (lazy (unmpst s)#c))}}
let c = {role=C; lens={get=(fun (Mpst (lazy c)) -> c#c); put=(fun s v -> obj3 (lazy (unmpst s)#a) (lazy (unmpst s)#b) (lval v))}}

let get_sess r m =
  let p = r.lens.get m in
  Sess(Mpst (lazy (object method a=() method b=() method c=() end)), p)

let choice_at x = Global.choice_at merge3 x
