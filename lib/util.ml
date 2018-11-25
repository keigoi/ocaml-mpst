open Base
open Session
open Global

module Labels = struct

  let msg_ o =
    {make_channel=o#ch_msg;
     select_label=(fun f -> object method msg=f end);
     offer_label=(fun l -> `msg(l))}

  let left_ o =
    {make_channel=o#ch_left;
     select_label=(fun f -> object method left=f end);
     offer_label=(fun l -> `left(l))}

  let right_ o =
    {make_channel=o#ch_right;
     select_label=(fun f -> object method right=f end);
     offer_label=(fun l -> `right(l))}

  let left_or_right =
    {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}

  let right_or_left =
    {label_merge=(fun or_ ol -> object method left=ol#left method right=or_#right end)}

  module Shmem = struct
    let shmem () =
      let st, push = Lwt_stream.create () in
      {receiver=(fun () -> Lwt_stream.next st);
       sender=(fun () v -> push (Some v))}

    let msg =
      {make_channel=shmem;
       select_label=(fun f -> object method msg=f end);
       offer_label=(fun l -> `msg(l))}

    let left =
      {make_channel=shmem;
       select_label=(fun f -> object method left=f end);
       offer_label=(fun l -> `left(l))}

    let right =
      {make_channel=shmem;
       select_label=(fun f -> object method right=f end);
       offer_label=(fun l -> `right(l))}

    let mklabel f g =
      {make_channel=shmem;
       select_label=f;
       offer_label=g}
  end

end
