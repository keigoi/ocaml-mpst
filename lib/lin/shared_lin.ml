open Concur_shims
open Mpst.M

let (let*) = IO.bind

include Shared.Make(DynLin.NoCheck)(Comm_lin.Lin)

let accept sh r =
  {Linocaml.__m=(fun lpre ->
       let* ep = accept sh r in
       IO.return (lpre, {Linocaml.__lin=ep})
     )}

let connect sh r =
  {Linocaml.__m=(fun lpre ->
       let* ep = connect sh r in
       IO.return (lpre, {Linocaml.__lin=ep})
     )}
