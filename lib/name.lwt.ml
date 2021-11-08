type 't st = 't Stream_opt.t

let create_st () = Stream_opt.create ()

type ('t, 'u) out0 = {
  o_st : 'u st;
  o_wrap : 't -> 'u;
  o_merged : 't out list;
  o_inp_refs : 'u inp_ref list;
}

and 't out_ =
  (* triple of original stream, wraping function and merged (merged wrappers) of same type *)
  | Out : ('t, 'u) out0 -> 't out_

and 't out = 't out_ ref

and ('u, 't) inp0 = {
  i_st : 't st;
  i_wrap : 't -> 'u option;
  i_merged : 'u inp list;
  i_out_refs : 't out_ref list;
}

and _ inp_ = Inp : ('u, 't) inp0 -> 'u inp_

and 'u inp = 'u inp_ ref

and _ out_ref = OutRef : 'u out * ('u -> 't) -> 't out_ref

and _ inp_ref = InpRef : 't inp * ('u -> 't option) -> 'u inp_ref

type ('w, 'u) inp_many0 = {
  g_inplist : 'u inp list;
  g_wrap : 'u list -> 'w;
  g_merged : 'w inp_many list;
}

and _ inp_many_ = Gather : ('w, 'u) inp_many0 -> 'w inp_many_

and 'w inp_many = 'w inp_many_ ref

type 'v out_many = 'v out list
type ('a, 'b) either = Left of 'a | Right of 'b

let id x = x
let in_left x = Left x
let in_right x = Right x
let fst_some (x, _) = Some x
let snd_some (_, x) = Some x
let bind_option f m0 = match m0 with Some x -> f x | None -> None
let map_option f m0 = match m0 with Some x -> Some (f x) | None -> None

(** assign to the input the given stream and wrapper, via given inp_ref *)
let update_inp (type t u)
    ((st : u st), (wrap : u -> t option), (out_refs : u out_ref list)) :
    t inp_ref -> u inp_ref = function
  | InpRef (({ contents = Inp { i_merged; _ } } as r), wrap_orig) ->
      let wrap_new x = bind_option wrap_orig (wrap x) in
      r := Inp { i_st = st; i_wrap = wrap_new; i_out_refs = out_refs; i_merged };
      InpRef (r, wrap_new)

(** Merge two streams into one via output endpoint *)
let merge_out : type t. t out -> t out -> t out =
 fun outL outR ->
  match (outL, outR) with
  | _ when outL == outR -> outL
  | { contents = Out outL0 }, { contents = Out outR0 } ->
      let st : (_ * _) st = create_st () in
      (* make a new pair stream *)
      let outs = List.append outL0.o_merged outR0.o_merged in
      let wrap x = (outL0.o_wrap x, outR0.o_wrap x) in
      let out_refs = List.map (fun out -> OutRef (out, wrap)) outs in
      let inp_refs =
        List.map (update_inp (st, fst_some, out_refs)) outL0.o_inp_refs
        @ List.map (update_inp (st, snd_some, out_refs)) outR0.o_inp_refs
      in
      let new_out =
        Out { o_st = st; o_wrap = wrap; o_merged = outs; o_inp_refs = inp_refs }
      in
      (* let merged outputs point to the new one *)
      List.iter (fun out -> out := new_out) outs;
      outL

let update_out (type t u)
    ((st : u st), (wrap : t -> u), (inp_refs : u inp_ref list)) :
    t out_ref -> u out_ref = function
  | OutRef (({ contents = Out { o_merged; _ } } as out), wrap_orig) ->
      let wrap_new x = wrap (wrap_orig x) in
      (* update output refs *)
      out :=
        Out { o_st = st; o_wrap = wrap_new; o_inp_refs = inp_refs; o_merged };
      OutRef (out, wrap_new)

(** Merge two input endpoints into one *)
let merge_inp : type t. t inp -> t inp -> t inp =
 fun inpL inpR ->
  match (inpL, inpR) with
  | _ when inpL == inpR -> inpL
  | { contents = Inp inpL0 }, { contents = Inp inpR0 } ->
      let st : (_, _) either st = create_st () in
      (* new wrap function *)
      let wrap = function
        | Left x -> inpL0.i_wrap x
        | Right x -> inpR0.i_wrap x
      in
      let merged = List.append inpL0.i_merged inpR0.i_merged in
      let inp_refs = List.map (fun r0 -> InpRef (r0, wrap)) merged in
      let out_refs =
        (* contents are updated later *)
        List.map (update_out (st, in_left, inp_refs)) inpL0.i_out_refs
        @ List.map (update_out (st, in_right, inp_refs)) inpR0.i_out_refs
      in
      let new_inp =
        Inp
          { i_st = st; i_wrap = wrap; i_merged = merged; i_out_refs = out_refs }
      in
      (* update inputs to the merged ones *)
      List.iter (fun r0 -> r0 := new_inp) merged;
      inpL

let hetero_merge_inp : type t u. t inp -> u inp -> (t, u) either inp =
 fun { contents = Inp inpL0 } { contents = Inp inpR0 } ->
  let st : (_, _) either st = create_st () in
  let wrap = function
    | Left x -> map_option (fun x -> Left x) @@ inpL0.i_wrap x
    | Right x -> map_option (fun x -> Right x) @@ inpR0.i_wrap x
  in
  let inp =
    ref
    @@ (*dummy*)
    Inp { i_st = st; i_wrap = wrap; i_merged = []; i_out_refs = [] }
  in
  let inp_ref = InpRef (inp, wrap) in
  let out_refs =
    (* contents are updated later *)
    List.map (update_out (st, in_left, [ inp_ref ])) inpL0.i_out_refs
    @ List.map (update_out (st, in_right, [ inp_ref ])) inpR0.i_out_refs
  in
  (* wiring it *)
  let inp_ =
    Inp { i_st = st; i_wrap = wrap; i_merged = [ inp ]; i_out_refs = out_refs }
  in
  inp := inp_;
  inp

let merge_out_many outLs outRs = List.map2 merge_out outLs outRs

let merge_inp_many : type t. t inp_many -> t inp_many -> t inp_many =
 fun gl gr ->
  match (gl, gr) with
  | ( {
        contents =
          Gather { g_inplist = inpsL; g_wrap = wrapL; g_merged = mergedL };
      },
      {
        contents =
          Gather { g_inplist = inpsR; g_wrap = wrapR; g_merged = mergedR };
      } ) ->
      let inps = List.map2 hetero_merge_inp inpsL inpsR in
      let wrap_left = function
        | Left x -> x
        | Right _ -> failwith "inp_many: reception failure: Right"
      and wrap_right = function
        | Right x -> x
        | Left _ -> failwith "inp_many: reception failure: Left"
      in
      let wrap = function
        | Left x :: xs -> wrapL @@ (x :: List.map wrap_left xs)
        | Right x :: xs -> wrapR @@ (x :: List.map wrap_right xs)
        | [] -> failwith "inp_many: reception failure: empty"
      in
      let merged = mergedL @ mergedR in
      let g0 = Gather { g_inplist = inps; g_wrap = wrap; g_merged = merged } in
      List.iter (fun g -> g := g0) merged;
      gl

let create f =
  let st = create_st () in
  let wrap x = Some (f x) in
  let rec out =
    {
      contents =
        Out
          {
            o_st = st;
            o_wrap = id;
            o_merged = [ out ];
            o_inp_refs = [ InpRef (inp, wrap) ];
          };
    }
  and inp =
    {
      contents =
        Inp
          {
            i_st = st;
            i_wrap = wrap;
            i_merged = [ inp ];
            i_out_refs = [ OutRef (out, id) ];
          };
    }
  in
  (out, inp)

let create_out_many cnt f =
  let outs, inps = List.split @@ List.init cnt (fun i -> create (f i)) in
  (outs, inps)

let create_inp_many cnt (f : 'v list -> 't) =
  let outs, inps = List.split @@ List.init cnt (fun _ -> create id) in
  let rec inp_many =
    {
      contents =
        Gather { g_inplist = inps; g_wrap = f; g_merged = [ inp_many ] };
    }
  in
  (outs, inp_many)

let[@inline] send { contents = Out { o_st; o_wrap; _ } } v =
  Stream_opt.send o_st (o_wrap v)

let[@inline] receive { contents = Inp { i_st; i_wrap; _ } } =
  Stream_opt.receive_wrap ~f:i_wrap i_st

let send_many outs f = Lwt_list.iteri_p (fun i out -> send out (f i)) outs

let receive_many : type t. t inp_many -> t Lwt.t = function
  | { contents = Gather { g_inplist = inps; g_wrap = wrap; _ } } ->
      Lwt.map wrap (Lwt_list.map_s receive inps)
