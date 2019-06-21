module Global = Mpst_async.Global

module Local =
  Mpst_monad.Local_monad.Make
    (Mpst_async.Peripheral.AsyncMonad)
    (Mpst_async.Peripheral.AsyncEvent)
    (Linocaml_async)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util
end
include Default
