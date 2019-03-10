
module Make(X:sig type conn end) = struct
  module Global = Global.Make(X)
  module Session = Global.Session
end

module Util = Util
