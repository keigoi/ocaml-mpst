module Local_monad = Local_monad
module Global_monad = Global_monad
module Nocheck = Nocheck


module Global =
  Global_monad.Make
    (Mpst.Peripheral.Pure)
    (Mpst.Peripheral.Event)
    (Mpst.Peripheral.Serial)
    (Linocaml.Direct)

module Local =
  Local_monad.Make
    (Mpst.Peripheral.Pure)
    (Mpst.Peripheral.Event)
    (Linocaml.Direct)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util.Make(Nocheck.Nodyncheck)
end
include Default
