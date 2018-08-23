module Base = Mpst__Base
module Session = Mpst__Session
module Global = Mpst__Global
module Util = Mpst__Util

module ThreeParty = struct
  include Mpst__Base
  include Mpst__Session
  include Mpst__Global
  include Mpst__Util.Labels
  include Mpst__Util.Roles
end
