module Global = Mpst_lwt.Global

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
