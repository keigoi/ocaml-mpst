module Local_monad = Local_monad

module Global =
  Mpst.Global.Make
    (Mpst.Peripheral.Pure)
    (Mpst.Peripheral.Event)
    (Mpst.Peripheral.Serial)
    (struct
      type 'a lin = 'a Linocaml.lin
      let mklin x = {Linocaml.__lin=x}
      let unlin x = x.Linocaml.__lin
    end)

module Local =
  Local_monad.Make
    (Mpst.Peripheral.Pure)
    (Mpst.Peripheral.Event)
    (Linocaml.Direct)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util
end
include Default
