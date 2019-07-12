open Base
open Common

module Make
         (EP:S.ENDPOINTS)
         (M:S.MONAD)
         (EV:S.EVENT with type 'a monad = 'a M.t)
       : S.LOCAL
       with type 'a monad = 'a M.t
       with type 't out = 't Out.Make(EP)(M)(EV).out
       with type 't inp = 't Inp.Make(EP)(M)(EV).inp
       with type 't lin = 't EP.lin
  = struct

  module Inp_ = Inp.Make(EP)(M)(EV)
  module Out_ = Out.Make(EP)(M)(EV)

  type 'a monad = 'a M.t
  type 'a lin = 'a EP.lin

  type 't out = 't Out_.out
  type 't inp = 't Inp_.inp
  let receive = Inp_.receive
  let send = Out_.send
  let sendmany = Out_.sendmany

  let close _ = ()
end[@@inline]
