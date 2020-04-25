open Concur_shims
open Mpst

include Shared.Make(Dyn_lin.NoCheck)(Global_combinators_lin.Lin)

include Global_combinators_lin

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
