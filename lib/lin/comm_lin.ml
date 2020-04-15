open Mpst.M
open Base

module Static : Comm.LIN with type 'a lin = 'a Linocaml.lin = struct
  type 'a lin = 'a Linocaml.lin
  let mklin x = {Linocaml.__lin=x}
end

module Comm = Comm.Make(DynLin.NoCheck)(Static)

