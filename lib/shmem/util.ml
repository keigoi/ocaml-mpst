open Mpst_base

module Make(F:S.FLAG)(L:S.LIN) = struct
  module Global = Global.Make(F)(L)
  open Global

let msg = {select_label=(fun f -> object method msg v=f v end);
           offer_label=(fun (v,c) -> `msg(v,c))}
let left = {select_label=(fun f -> object method left v=f v end);
           offer_label=(fun (v,c) -> `left(v,c))}
let right = {select_label=(fun f -> object method right v=f v end);
           offer_label=(fun (v,c) -> `right(v,c))}

let left_or_right =
  {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}

let right_or_left =
  {label_merge=(fun ol or_ -> object method left=or_#left method right=ol#right end)}
end
