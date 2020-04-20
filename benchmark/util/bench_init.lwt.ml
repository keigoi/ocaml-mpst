
let init () =
  Lwt_engine.set (new Lwt_engine.libev ())
