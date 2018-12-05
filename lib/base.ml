type ('a, 'b) either = Left of 'a | Right of 'b

type ('v1, 'v2, 's1, 's2) lens = {
    get : 's1 lazy_t -> 'v1;
    put : 's1 lazy_t -> 'v2 -> 's2 lazy_t;
  }

let lens_get l s = l.get (Lazy.from_val s)
let lens_put l s v = Lazy.force (l.put (Lazy.from_val s) v)

let root = {get = (fun s -> Lazy.force s); put = (fun _ v -> Lazy.from_val v)}
