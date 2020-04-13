type 't st = {
    stream : 't Lwt_stream_opt.t;
    mutable out_refs : 't out_ref list;
    mutable inp_refs : 't inp_ref list;
  }
and 't st_gather = {
    stream_g : ('t Lwt_stream_opt.t * 't out_ref) list;
    mutable inp_refs_g : 't gather_ref list;
  }
and _ out_ref =
  (* reference to the wrappeed channel and wraping function *)
  |  OutRef : 'u out * ('u -> 't) -> 't out_ref
and 't out_ =
  (* triple of original stream, wraping function and siblings (merged wrappers) of same type *)
  Out : 'u st * ('t -> 'u) * 't out list -> 't out_
and 't out = 't out_ ref
and _ inp_ref =
  |  InpRef : 'u inp * ('t -> 'u option) -> 't inp_ref
and _ inp_ =
  | Inp : 't st * ('t -> 'u option) * 'u inp list -> 'u inp_
and 'u inp = 'u inp_ ref
and _ gather_ref =
  |  GatherRef : 'u gather * ('t list -> 'u option) -> 't gather_ref
and _ gather_ =
  | Gather : 't st list * ('t list  -> 'u option) * 'u inp list -> 'u gather_
and 'u gather = 'u gather_ ref

(* auxiliary data types for "internal protocol" *)
type ('a, 'b) either = Left of 'a | Right of 'b

let in_left x = Left(x)
let in_right x = Right(x)
let fst_some (x,_) = Some x
let snd_some (_,x) = Some x

let join_option f m0 =
  match m0 with
  | Some x -> f x
  | None -> None

let create_raw () =
  let t = {
      stream = Lwt_stream_opt.create ();
      out_refs = [];
      inp_refs = [];
    }
  in
  t

let update_out_ref : type t u. u st -> (t -> u) -> t out_ref -> u out_ref =
  fun new_st f -> function
  | OutRef({contents=Out(_,_,siblings)} as r, outf) ->
     let outf' = fun x -> f (outf x) in
     r := Out(new_st, outf', siblings);
     OutRef(r,outf')

let update_inp_ref : type t u. u st -> (u -> t option) -> t inp_ref -> u inp_ref =
  fun st g -> function
  | (InpRef({contents=Inp(_,_,siblings)} as r,in_)) ->
     let g = fun x -> join_option in_ (g x) in
     r := Inp(st,g,siblings);
     InpRef(r,g)

(** Merge two streams into one, via input endpoint  *)
let merge_inp : type t. t inp -> t inp -> t inp = fun inpL inpR ->
  match inpL, inpR with
  | _ when inpL==inpR ->
     inpL
  | {contents=Inp(st_l, f_l, siblings_l)},
    {contents=Inp(st_r, f_r, siblings_r)} ->
     let st : (_,_) either st = create_raw () in
     let f_lr = function Left x -> f_l x | Right x -> f_r x in
     let siblings = List.append siblings_l siblings_r in
     let inp = Inp(st, f_lr, siblings) in
     List.iter (fun r0 -> r0 := inp) siblings;
     st.out_refs <-
       List.map (update_out_ref st in_left) st_l.out_refs @
       List.map (update_out_ref st in_right) st_r.out_refs;
     st.inp_refs <-
       List.map (fun inp -> InpRef(inp, f_lr)) siblings;
     inpL

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out -> t out -> t out = fun outL outR ->
  match outR, outR with
  | _ when outL==outL ->
     outL
  | {contents=Out(st_l, f_l, siblings_l)},
    {contents=Out(st_r, f_r, siblings_r)} ->
    (* make a new pair stream *) 
    let st : (_ * _) st = create_raw () in
    (* new output wrapper *)
    let f_lr = fun x -> (f_l x, f_r x) in
    let siblings = List.append siblings_l siblings_r in
    (* construct the new out referring the new stream *)
    let new_out = Out(st, f_lr, siblings) in    
    (* udpate siblings to the new out (including outL and outR) *)
    List.iter (fun r0 -> r0 := new_out) siblings;
    (* wiring new out/inp refs *)
    st.out_refs <-
      List.map (fun out -> OutRef(out, f_lr)) siblings;
    st.inp_refs <-
      List.map (update_inp_ref st fst_some) st_l.inp_refs @
      List.map (update_inp_ref st snd_some) st_r.inp_refs;
    st_l.inp_refs <- [];
    st_l.out_refs <- [];
    st_r.inp_refs <- [];
    st_r.out_refs <- [];
    outL

let wrap_inp st f =
  let f x = Some (f x) in
  let rec inp = {contents=Inp(st,f,[inp])} in
  st.inp_refs <- [InpRef(inp, f)];
  inp

let wrap_out st f =
  let rec out = {contents=Out(st,f,[out])} in
  st.out_refs <- [OutRef(out, f)];
  out

let create f =
  let st = create_raw () in
  wrap_out st (fun x -> x), wrap_inp st f

let send o v =
  match o with
  | {contents=Out(st,f,_)} ->
     (* FIXME replace List.nth with lazy?? *)
     Lwt_stream_opt.send st.stream (f v)

let receive = function
  | {contents=Inp(st,f,_)} ->
      (* FIXME replace List.nth with lazy?? *)
     Lwt_stream_opt.receive_wrap ~f st.stream
