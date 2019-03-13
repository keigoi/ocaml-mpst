module Lens = Lens
module S = S

exception RoleNotEnabled
exception ReceiveFail

let repeat num f =
  let r = ref [] in
  for i=0 to num-1
  do
    r := (f i)::!r
  done;
  !r
            
