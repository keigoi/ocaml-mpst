open Base
open Session
open Global

module Labels = struct

  class type ['k1,'k2] ch_msg =
  object
    method ch_msg : 'v. unit -> ('k1,'k2,'v) channel
  end

  let msg (o:('k1,'k2) #ch_msg) =
    {make_channel=o#ch_msg;
     select_label=(fun f -> object method msg=f end);
     offer_label=(fun l -> `msg(l))}

  class type ['k1,'k2] ch_left =
  object
    method ch_left : 'v. unit -> ('k1,'k2,'v) channel
  end
    
  let left (o:('k1,'k2) #ch_left) =
    {make_channel=o#ch_left;
     select_label=(fun f -> object method left=f end);
     offer_label=(fun l -> `left(l))}

  class type ['k1,'k2] ch_right =
  object
    method ch_right : 'v. unit -> ('k1,'k2,'v) channel
  end

  let right (o:('k1,'k2) #ch_right) =
    {make_channel=o#ch_right;
     select_label=(fun f -> object method right=f end);
     offer_label=(fun l -> `right(l))}

  class type ['k1,'k2] ch_deleg =
  object
    method ch_deleg : 'v. unit -> ('k1,'k2,'v) channel
  end

  let deleg prot (o:('k1,'k2) #ch_deleg) =
    let open Lwt in
    let ch = o#ch_deleg() in
    {make_channel=(fun () ->
       {sender=(fun k (Sess (ks, _)) -> ch.sender k ks);
        receiver=(fun k -> ch.receiver k >>= fun ks -> Lwt.return (Sess (ks, prot)))}
     );
     select_label=(fun f -> object method deleg=f end);
     offer_label=(fun l -> `deleg(l))}

  let left_or_right =
    {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}

  let right_or_left =
    {label_merge=(fun or_ ol -> object method left=ol#left method right=or_#right end)}

  class type ['k1,'k2] standard =
    object
      inherit ['k1,'k2] ch_msg
      inherit ['k1,'k2] ch_left
      inherit ['k1,'k2] ch_right
    end

  class type ['k1,'k2] standard_deleg =
    object
      inherit ['k1,'k2] standard
      inherit ['k1,'k2] ch_deleg
    end

  let make_shmem_channel () =
    let st, push = Lwt_stream.create () in
    {receiver=(fun () -> Lwt_stream.next st);
     sender=(fun () v -> push (Some v))}

  class shmem : [unit,unit] standard_deleg =
  object
    method ch_msg : 'v. unit -> (unit,unit,'v) channel = make_shmem_channel
    method ch_left : 'v. unit -> (unit,unit,'v) channel = make_shmem_channel
    method ch_right : 'v. unit -> (unit,unit,'v) channel = make_shmem_channel
    method ch_deleg : 'v. unit -> (unit,unit,'v) channel = make_shmem_channel
  end

  module Shmem = struct

    let msg =
      {make_channel=make_shmem_channel;
       select_label=(fun f -> object method msg=f end);
       offer_label=(fun l -> `msg(l))}

    let left =
      {make_channel=make_shmem_channel;
       select_label=(fun f -> object method left=f end);
       offer_label=(fun l -> `left(l))}

    let right =
      {make_channel=make_shmem_channel;
       select_label=(fun f -> object method right=f end);
       offer_label=(fun l -> `right(l))}

    let mklabel f g =
      {make_channel=make_shmem_channel;
       select_label=f;
       offer_label=g}
  end

end
