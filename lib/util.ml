open Base
open Session
open Global

module Labels = struct

  class type ['k1,'k2] ch_msg =
  object
    method ch_msg : ('k1,'k2,'v) channel
  end

  let msg (o:('k1,'k2) #ch_msg) =
    {channel=o#ch_msg;
     select_label=(fun f -> object method msg=f end);
     offer_label=(fun l -> `msg(l))}

  class type ['k1,'k2] ch_left =
  object
    method ch_left : ('k1,'k2,'v) channel
  end
    
  let left (o:('k1,'k2) #ch_left) =
    {channel=o#ch_left;
     select_label=(fun f -> object method left=f end);
     offer_label=(fun l -> `left(l))}

  class type ['k1,'k2] ch_right =
  object
    method ch_right : ('k1,'k2,'v) channel
  end

  let right (o:('k1,'k2) #ch_right) =
    {channel=o#ch_right;
     select_label=(fun f -> object method right=f end);
     offer_label=(fun l -> `right(l))}

  let deleg_dist :
        ('ks, 'b) prot ->
        ('k1, 'k2,'ks) channel ->
        (< deleg : ('ks, 's) sess -> 'sa >,
         [> `deleg of ('ks, 's) sess * 'sb ],
         'sa, 'sb, 'k1, 'k2, ('ks, 's) sess) label =
    fun prot dch ->
    {channel=
       {sender=(fun k (Sess (ks, _)) -> dch.sender k ks);
        receiver=(fun k ->
             Lwt.bind (dch.receiver k) (fun ks ->
             Lwt.return (Sess (ks, prot))))};
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
      method ch_deleg : 'v. ('k1, 'k2, 'v) channel
    end

  (** dummy - functions are never called *)
  let shmem_channel =
    {sender=(fun Memory _ -> ());
     receiver=(fun Memory -> Lwt.fail_with "impossible")}

  class shmem : [memory,memory] standard =
    object
      method ch_msg : 'v. (memory,memory,'v) channel= shmem_channel
      method ch_left : 'v. (memory,memory,'v) channel= shmem_channel
      method ch_right : 'v. (memory,memory,'v) channel= shmem_channel
      method ch_deleg : 'v. (memory,memory,'v) channel= shmem_channel
    end
  
    
  module Shmem = struct

    let msg =
      {channel=shmem_channel;
       select_label=(fun f -> object method msg=f end);
       offer_label=(fun l -> `msg(l))}

    let left =
      {channel=shmem_channel;
       select_label=(fun f -> object method left=f end);
       offer_label=(fun l -> `left(l))}

    let right =
      {channel=shmem_channel;
       select_label=(fun f -> object method right=f end);
       offer_label=(fun l -> `right(l))}

    let mklabel f g =
      {channel=shmem_channel;
       select_label=f;
       offer_label=g}
  end

end
