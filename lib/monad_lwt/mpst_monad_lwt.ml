module Global =
  Mpst_monad.Global_monad.Make
    (Mpst_lwt.Peripheral.Lwt)
    (Mpst_lwt.Peripheral.LwtEvent)
    (Mpst_lwt.Peripheral.LwtSerial)
    (Linocaml_lwt)

module Local =
  Mpst_monad.Local_monad.Make
    (Mpst_lwt.Peripheral.Lwt)
    (Mpst_lwt.Peripheral.LwtEvent)
    (Linocaml_lwt)

module Default = struct
  include Mpst.Base
  include Global
  include Local
  include Mpst.Util
end
include Default
