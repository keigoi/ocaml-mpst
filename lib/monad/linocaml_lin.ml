module Lin : Mpst.S.LIN with type 'a lin = 'a Linocaml.lin
  = struct
  type 'a lin = 'a Linocaml.lin
  type 'a gen = 'a

  open Linocaml

  let use t = t.__lin

  let create v = {__lin=v}
  let create_nolin v = v
  let create_dummy v = {__lin=v}
  let fresh t = t

  let map_gen f x = f x

  let merge_gen f l r = {__lin=f l.__lin r.__lin}

  let lift_disj_merge mrg = mrg
end

module EP : Mpst.S.ENDPOINTS with type 'a lin = 'a Lin.lin =
  Mpst.Endpoints.Make(Lin)

