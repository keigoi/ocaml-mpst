open Session
open Global

let m = raise Not_found
   
let msg = {select_label=(fun f -> object method msg v=f v end);
             offer_label=(fun (v,c) -> `msg(v,c));
             channel=m
            }
let left = {select_label=(fun f -> object method left v=f v end);
              offer_label=(fun (v,c) -> `left(v,c));
              channel=m
             }
let right = {select_label=(fun f -> object method right v=f v end);
               offer_label=(fun (v,c) -> `right(v,c));
               channel=m
              }

let left_or_right =
  {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}
