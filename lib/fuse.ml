module type CHANNEL = sig
  type 'a t
  val create : unit -> 'a t
  val send : 'a t -> 'a -> unit
  val receive : 'a t -> 'a
end

module Make(Channel:CHANNEL) 
: sig
  type 'a io = 'a
  type 'a out
  type 'a inp
  type 'a out_many
  type 'a inp_many

  val merge_out : 'a out -> 'a out -> 'a out
  val merge_inp : 'a inp -> 'a inp -> 'a inp

  val send : 'a out -> 'a -> unit io
  val receive : 'a inp -> 'a io

  val merge_out_many : 'a out_many -> 'a out_many -> 'a out_many
  val send_many : 'a out_many -> (int -> 'a) -> unit io

  (* val merge_inp_many : 'a inp_many -> 'a inp_many -> 'a inp_many *)
  val receive_many : 'a inp_many -> 'a io

  val create : ('a -> 'b) -> 'a out * 'b inp
  val create_out_many : int -> (int -> 'a -> 'b) -> 'a out_many * 'b inp list
  val create_inp_many : int -> ('a list -> 'b) -> 'a out list * 'b inp_many

end
 = struct
  type 'a io = 'a

  type 't st = 't Channel.t
  let create_st () = Channel.create ()
  
  type ('t,'u) proj =
    | Id : ('u, 'u) proj
    | Fst : ('u1*'u2, 'u1) proj
    | Snd : ('u1*'u2, 'u2) proj
    | Either : ('t1, 'u) proj * ('t2, 'u) proj -> (('t1,'t2) Either.t, 'u) proj
    | Comp : ('s,'t) proj * ('t,'u) proj -> ('s,'u) proj

  let rec eval_proj : type s t. (s,t) proj -> s -> t = function
    | Id -> fun x -> x
    | Fst -> fun (x,_) -> x
    | Snd -> fun (_,x) -> x
    | Either(p1,p2) -> (function Either.Left x -> eval_proj p1 x | Right x -> eval_proj p2 x)
    | Comp(p1,p2) -> fun x -> eval_proj p1 x |> eval_proj p2

  type 's out =
    's out_ ref
  and 's out_ =
    (* triple of original stream, wraping function and merged (merged wrappers) of same type *)
      Out : ('s, 't) out0 -> 's out_
  and ('s,'t) out0 =
    {o_st: 't st;
     o_wrap: ('s -> 't);
     o_merged: 's out list;
     o_inp_refs: 't inp_ref list}  
  and 't inp_ref =
      InpRef : 'u inp * ('t, 'u) proj -> 't inp_ref
  and 'u inp =
    'u inp_ ref
  and 'u inp_ =
      Inp : ('t, 'u) inp0 -> 'u inp_
  and ('t,'u) inp0 =
    {i_st: 't st;
     i_wrap: ('t, 'u) proj;
     i_merged: 'u inp list;
     i_out_refs: 't out_ref list}  
  and _ out_ref =
      OutRef : 's out * ('s -> 't) -> 't out_ref
  
  type ('w, 'u) inp_many0 =
    {g_inplist : 'u inp list;
     g_wrap: ('u list -> 'w);
     g_merged: 'w inp_many list}
  and _ inp_many_ =
      Gather : ('w, 'u) inp_many0 -> 'w inp_many_
  and 'w inp_many =
    'w inp_many_ ref
  
  type 'v out_many =
    'v out list
    
  let id x = x
  let in_left x = Either.Left x
  let in_right x = Either.Right x
  
  (** assign to the input the given stream and wrapper, via given inp_ref *)
  let update_inp
      (type t u) ((st : u st), (wrap : (u,t) proj), (out_refs : u out_ref list))
    : t inp_ref -> u inp_ref =
    function
    | InpRef({contents=Inp({i_merged; _})} as r, wrap_orig) ->
      let wrap_new = Comp (wrap, wrap_orig) in
      r := Inp({i_st = st;
                i_wrap = wrap_new;
                i_out_refs = out_refs;
                i_merged});
      InpRef(r, wrap_new)
  
  (** Merge two streams into one via output endpoint  *)
  let merge_out : type t. t out -> t out -> t out = fun outL outR ->
    match outL, outR with
    | _ when outL==outR ->
       outL
    | {contents=Out(outL0)}, {contents=Out(outR0)} ->
      let st : (_ * _) st = create_st () in
      (* make a new pair stream *) 
      let outs = List.append outL0.o_merged outR0.o_merged in
      let wrap x = (outL0.o_wrap x, outR0.o_wrap x) in
      let out_refs = List.map (fun out -> OutRef(out, wrap)) outs in
      let inp_refs =
        List.map (update_inp (st, Fst, out_refs)) outL0.o_inp_refs @
        List.map (update_inp (st, Snd, out_refs)) outR0.o_inp_refs
      in
      let new_out =
        Out {o_st = st;
             o_wrap = wrap;
             o_merged = outs;
             o_inp_refs = inp_refs
            }
      in
      (* let merged outputs point to the new one *)
      List.iter (fun out -> out := new_out) outs;
      outL
  
  let update_out
      (type t u) ((st : u st), (wrap : t -> u), (inp_refs : u inp_ref list))
    : t out_ref -> u out_ref =
    function
    | OutRef({contents=Out({o_merged; _})} as out, wrap_orig) ->
      let wrap_new x = wrap (wrap_orig x) in
      (* update output refs *)
      out := Out {o_st = st;
                  o_wrap = wrap_new;
                  o_inp_refs = inp_refs;
                  o_merged;
                 };
      OutRef(out, wrap_new)
  
  (** Merge two input endpoints into one  *)
  let merge_inp : type t. t inp -> t inp -> t inp = fun inpL inpR ->
    match inpL, inpR with
    | _ when inpL==inpR ->
       inpL
    | {contents=Inp(inpL0)},
      {contents=Inp(inpR0)} ->
      let st : (_,_) Either.t st = create_st () in
      (* new wrap function *)
      let wrap = Either(inpL0.i_wrap, inpR0.i_wrap) in
      let merged = List.append inpL0.i_merged inpR0.i_merged in
      let inp_refs = List.map (fun r0 -> InpRef(r0, wrap)) merged in
      let out_refs =
        (* contents are updated later *)
        List.map (update_out (st, in_left, inp_refs)) inpL0.i_out_refs @
        List.map (update_out (st, in_right, inp_refs)) inpR0.i_out_refs
      in
      let new_inp =
        Inp {i_st = st;
             i_wrap = wrap;
             i_merged = merged;
             i_out_refs = out_refs}
      in
      (* update inputs to the merged ones *)
      List.iter (fun r0 -> r0 := new_inp) merged;
      inpL
  
  (* let hetero_merge_inp : type t u. t inp -> u inp -> (t,u) Either.t inp =
    fun {contents=Inp(inpL0)} {contents=Inp(inpR0)} ->
      let st : (_,_) Either.t st = create_st () in
      let wrap = function
        | Either.Left x -> Either.Left (inpL0.i_wrap x)
        | Right x -> Right (inpR0.i_wrap x)
      in
      let inp = ref @@ (*dummy*)Inp{i_st = st; i_wrap = wrap; i_merged = []; i_out_refs = []} in
      let inp_ref = InpRef(inp, wrap) in
      let out_refs =
        (* contents are updated later *)
        List.map (update_out (st, in_left, [inp_ref])) inpL0.i_out_refs @
        List.map (update_out (st, in_right, [inp_ref])) inpR0.i_out_refs
      in
      (* wiring it *)
      let inp_ = Inp {i_st = st; i_wrap = wrap; i_merged = [inp]; i_out_refs = out_refs} in
      inp := inp_;
      inp *)
  
  let merge_out_many = fun outLs outRs ->
    List.map2 merge_out outLs outRs
  
  (* let merge_inp_many : type t. t inp_many -> t inp_many -> t inp_many = fun gl gr ->
    match gl, gr with
    | {contents=Gather {g_inplist=inpsL; g_wrap=wrapL; g_merged=mergedL}},
      {contents=Gather {g_inplist=inpsR; g_wrap=wrapR; g_merged=mergedR}} ->
      let inps = List.map2 hetero_merge_inp inpsL inpsR in
      let wrap_left = function
        | Either.Left x -> x
        | Right _ -> failwith "inp_many: reception failure: Right"
      and wrap_right = function
        | Either.Right x -> x
        | Left _ -> failwith "inp_many: reception failure: Left"
      in
      let wrap = function
        | Either.Left x::xs -> wrapL @@ x::List.map wrap_left xs
        | Right x::xs -> wrapR @@ x::List.map wrap_right xs
        | [] -> failwith "inp_many: reception failure: empty"
      in
      let merged = mergedL @ mergedR in
      let g0 = Gather {g_inplist = inps; g_wrap = wrap; g_merged = merged} in
      List.iter (fun g -> g := g0) merged;
      gl *)
  
  let create wrap =
    let st = create_st () in
    let rec out = {contents=Out({o_st = st; o_wrap = wrap; o_merged=[out]; o_inp_refs = [InpRef(inp, Id)]})}
    and inp = {contents=Inp({i_st = st; i_wrap = Id; i_merged=[inp]; i_out_refs = [OutRef(out, wrap)]})}
    in
    out, inp
  
  let create_out_many cnt f =
    let outs, inps = List.split @@ List.init cnt (fun i -> create (f i)) in
    outs, inps
  
  let create_inp_many cnt (f : 'v list -> 't) =
    let outs, inps = List.split @@ List.init cnt (fun _ -> create id) in
    let rec inp_many = {contents=Gather {g_inplist = inps; g_wrap = f; g_merged = [inp_many]}} in
    outs, inp_many
  
  let[@inline] send {contents=Out({o_st; o_wrap; _})} v =
    Channel.send o_st (o_wrap v)
  
  let[@inline] receive {contents=Inp({i_st; i_wrap; _})} =
    eval_proj i_wrap @@ Channel.receive i_st
  
  let send_many outs f =
    List.iteri (fun i out -> send out (f i)) outs
  
  let receive_many : type t. t inp_many -> t =
    function {contents=Gather{g_inplist = inps; g_wrap = wrap; _}} ->
      wrap @@ List.map receive inps
end
