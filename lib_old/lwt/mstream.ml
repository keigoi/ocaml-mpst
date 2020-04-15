(* An lwt stream that is 
 * (1) optimized for unbounded buffering
 * (2) mergeable on both input and output send
 * (3) supports scatter/gather
 *)
type 'a one = 'a Mpst.Base.one
type 't st = {
    id: string;
    stream : 't Lwt_stream_opt.t list;
    mutable out_refs : 't out_refs;
    mutable inp_refs : 't inp_refs
  }
and 't out_refs = 't wrap_out list
and 't inp_refs = 't wrap_inp list
and _ wrap_out =
  |  WrapOut : 'u one out * ('u -> 't) -> 't wrap_out
  |  WrapOutMany : 'u list out * ('u -> 't) -> 't wrap_out
and _ wrap_inp =
  |  WrapInp : 'u one inp * ('t -> 'u option) -> 't wrap_inp
  |  WrapInpMany : 'u list inp * ('t -> 'v option) * ('v list -> 'u) -> 't wrap_inp
and 't out = 't out_ ref
and 't inp = 't inp_ ref
and 't out_ =
  Out_ : int * 'u st * ('t -> 'u) * 't one out list -> 't one out_
| OutMany_ : 'u st * ('t -> 'u) * 't list out list -> 't list out_
and 't inp_ =
  Inp_ : int * 'u st * ('u -> 't option) list * 't one inp list -> 't one inp_
| InpMany_ : 'u st * ('u -> 'v option) * ('v list -> 't) * 't list inp list -> 't list inp_

let len st = List.length st.stream

let id x = x
let in_some x = Some(x)
let join_option f m0 =
  match m0 with
  | Some x -> f x
  | None -> None
let map_option f m0 =
  match m0 with
  | Some x -> Some (f x)
  | None -> None
let orelse f g x =
  match f x with
  | None -> g x
  | x-> x
let compose f g x = f (g x)
let compose_opt f g x =
  match g x with
  | Some x -> f x
  | None -> None

let[@inline] debug str f x =
  (* print_endline str; *)
  f x

let[@inline] debug_ str =
  (* print_endline str; *)
  ()

let makeid =
  let c = ref 0 in
  fun () ->
  let i = !c in
  c := i+1; (* XXX must be atomic *)
  i

let create_raw n =
  let t = {
      id=string_of_int (makeid());
      stream = List.init n (fun _ -> Lwt_stream_opt.create ());
      out_refs = [];
      inp_refs = [];
    }
  in
  t

let update_out_ref : type t u. u st -> (t -> u) -> t wrap_out -> u wrap_out =
  fun st f w ->
  match w with
  | WrapOut({contents=Out_(i,_,_,rs)} as r,out) ->
     let f = fun x -> f (out x) in
     r := Out_(i, st, f, rs);
     List.iter
       (fun ({contents=Out_(idx,_,_,rs)} as r) -> r := Out_(idx,st,f,rs))
       rs;
     WrapOut(r,f)
  | WrapOutMany({contents=OutMany_(st0,f0,rs)} as r,out) ->
     let o = OutMany_(st, (fun x -> f (out x)), rs) in
     List.iter (fun r -> r := o) rs;
     r := o;
     WrapOutMany(r,(fun x -> f (out x)))

let update_inp_ref : type t u. u st -> (u -> t option) -> t wrap_inp -> u wrap_inp =
  fun st g w ->
  match w with
  | (WrapInp({contents=Inp_(i,_,_,rs)} as r,in_)) ->
     let size = List.length st.stream in
     let g = fun x -> join_option in_ (g x) in
     let gs =
       List.init size (fun _ -> g)
     in
     r := Inp_(i,st,gs,rs);
     List.iter
       (fun ({contents=Inp_(i,_,_,rs)} as r) ->
         r := Inp_(i,st,gs,rs))
       rs;
     WrapInp(r,g)
  | (WrapInpMany({contents=InpMany_(_,_,_,rs)} as r,in1,in2)) ->
     let g x = join_option in1 (g x) in
     let i = InpMany_(st, g, in2, rs) in
     List.iter (fun r -> r := i) rs;
     r := i;
     WrapInpMany(r, g, in2)

(* auxiliary data types for "internal protocol" *)
type ('a, 'b) either = Left of 'a | Right of 'b
type ('a, 'b) prod = FstOnly of 'a | SndOnly of 'b | Both of 'a * 'b

let in_left x = Left(x)
let in_right x = Right(x)
let fst_prod = function
  | FstOnly(x) -> Some x
  | SndOnly(_) -> None
  | Both(x,_) -> Some x
let snd_prod = function
  | FstOnly(_) -> None
  | SndOnly(x) -> Some x
  | Both(_,x) -> Some x
(* these functions are __dangerous__, as they result in blocking inputs (`None`). *)
let in_fstonly x = FstOnly(x)
let in_sndonly x = SndOnly(x)
let from_left = function Left x -> Some x | _ -> None
let from_right = function Right x -> Some x | _ -> None

let merge_st_either : type t u. t st -> u st -> (t,u) either st = fun sl sr ->
  let n = len sl in
  assert (n = len sr);
  let st : (_,_) either st = create_raw n in
  (* merge in/out references pointing to sl/sr *)
  (* and update each reference to point to this stream  *)
  st.out_refs <-
    List.map (update_out_ref st in_left) sl.out_refs @
      List.map (update_out_ref st in_right) sr.out_refs;
  (* this part is potentially dangerous, but actually safe for our purpose 
     since these are anyway overwritten later by merge_inp *)
  st.inp_refs <-
    List.map (update_inp_ref st from_left) sl.inp_refs
    @ List.map (update_inp_ref st from_right) sr.inp_refs;
  (* below is not necessary. here, this just explicitly discards sl and sr *)
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  st

(* Merges two streams into one. Let lr = merge ll rr, then:
   (1) Writing x to the left delivers FstOnly(x). 
       It can also consumed by receiving on ll.
   (2) Similarly, writing on the right delivers FstOnly(x) and
       it disappears by receiving on rr.
   (3) Writing Both(x,y) can be consumed by one of ll, rr or lr.
   (4) Message order is preserved. 
       If the queue head is FstOnly, rr blocks.
       Similarly, SndOnly will block ll.
 *)
let merge_st_prod : type t u. t st -> u st -> (t,u) prod st = fun sl sr ->
  let n = len sl in
  assert (n = len sr);
  let st : (_,_) prod st = create_raw n in
  st.out_refs <- (* possibly dangerous, but actually safe (see merge_inp) *)
    List.map (update_out_ref st in_fstonly) sl.out_refs @
    List.map (update_out_ref st in_sndonly) sr.out_refs;
  st.inp_refs <-
    List.map (update_inp_ref st fst_prod) sl.inp_refs @
    List.map (update_inp_ref st snd_prod) sr.inp_refs;
  sl.inp_refs <- [];
  sl.out_refs <- [];
  sr.inp_refs <- [];
  sr.out_refs <- [];
  st

let fromleftX = function
  | Left x -> x
  | Right _ -> failwith "impossible: fromleftX: possible scatter/gather bug?"

let fromrightX = function
  | Right x -> x
  | Left _ -> failwith "impossible: fromrightX: possible scatter/gather bug?"

let fixeither fl fr = function
  | [] -> failwith "impossible: fixeither: possible scatter/gather bug"
  | Left x::xs ->
     fl (x::List.map fromleftX xs)
  | Right x::xs ->
     fr (x::List.map fromrightX xs)

(** Merge two streams into one, via input endpoint  *)
let merge_inp : type t. t inp -> t inp -> t inp = fun ll rr ->
  match !ll,!rr with
  | Inp_(_,sl,_,_),Inp_(_,sr,_,_) when sl.id=sr.id ->
     ll
  | Inp_(i,sl,fls,ls),Inp_(i_,sr,frs,rs) ->
     assert (i=i_);
     (* The following wrongly wires ll and rr, as Left(x) and Right(x) are 
        delivered only to ll or rr respectively. 
        (i.e. other side will block.)
        This is fixed right after this.
      *)
     let st : (_,_) either st = merge_st_either sl sr in
     let flrs =
       List.map2
         (fun fl fr -> function Left x -> fl x|Right x -> fr x)
         fls frs
     in
     (* Gather all `t inp`s *)
     let is = ll::rr::(List.append ls rs) in
     (* and update to the desired ones *)
     List.iter
       (fun ({contents=Inp_(idx,_,_,is)} as r0) ->
         r0 := Inp_(idx,st,flrs,is))
       is;
     ll
  | InpMany_(sl,fl,fl2,ls),InpMany_(sr,fr,fr2,rs) ->
     let st : (_,_) either st = merge_st_either sl sr in
     let flr = function
         Left x -> map_option in_left (fl x)
       | Right x -> map_option in_right (fr x) in
     let is = ll::rr::(List.append ls rs) in
     (** assuming all senders agree, we map all payloads on either fl or fr only  *)
     List.iter
       (fun ({contents=InpMany_(_,_,_,is)} as r0) -> r0 := InpMany_(st,flr,fixeither fl2 fr2, is))
       is;
     ll

(** Merge two streams into one via output endpoint  *)
let merge_out : type t. t out -> t out -> t out = fun ll rr ->
  match !ll,!rr with
  | Out_(_,sl,_,_),Out_(_,sr,_,_) when sl.id=sr.id ->
     ll
  | Out_(i,sl,fl,ls),Out_(i_,sr,fr,rs) ->
     assert (i=i_);
     (* The underlying protocol is ('a,'b) prod which conveys:
        (1) FstOnly(x) delivers x to ll only,
        (2) SndOnly(x) delivers x to rr only, or
        (3) Both(x,y) delivers to ll __or__ rr (earlier one).
        Here, (1) and (2) are __NOT__ what we want, as both ll and rr shares 
        the same payload type `t`, they should treated equally.
        Similar to merge_inp, this is fixed right after this:
      *)
     let st : (_,_) prod st = merge_st_prod sl sr in
     let flr = fun x -> Both(fl x, fr x) in
     (* Here, we gather all `t out`s in one place *)
     let os = ll::rr::(List.append ls rs) in
     (* and put the correct wrapper which delivers `Both`. *)
     List.iter
       (fun ({contents=Out_(idx,_,_,os)} as r0) -> r0 := Out_(idx,st,flr,os))
       os;
     ll
  | OutMany_(sl,fl,ls),OutMany_(sr,fr,rs) ->
     let st : (_,_) prod st = merge_st_prod sl sr in
     let flr = fun x -> Both(fl x, fr x) in
     let os = ll::rr::(List.append ls rs) in
     List.iter
       (fun ({contents=OutMany_(_,_,os)} as r0) -> r0 := OutMany_(st,flr,os))
       os;
     ll

let wrap st f =
  assert (List.length st.stream = 1);
  let f x = Some (f x) in
  let out = ref (Out_(0,st,id,[])) in
  let inp = ref (Inp_(0,st,[f],[])) in
  st.out_refs <- [WrapOut(out, id)];
  st.inp_refs <- [WrapInp(inp, f)];
  out, inp

let wrap_scatter st f =
  let size = List.length st.stream in
  let f = (fun i x -> Some (f i x)) in
  let fs = List.init size f in
  let out = ref (OutMany_(st,id,[])) in
  let inps =
    List.init size
      (fun i -> ref (Inp_(i,st,fs,[])))
  in
  List.iteri (fun i ({contents=Inp_(idx,st,f,_)} as r) ->
      r := Inp_(idx,st,f, List.filter (fun r' -> r != r') inps)
    ) inps;
  st.out_refs <- [WrapOutMany(out, id)];
  st.inp_refs <- List.mapi (fun i inp -> WrapInp(inp, f i)) inps;
  out, inps

let wrap_gather st f =
  let outs = List.init (List.length st.stream) (fun i -> ref (Out_(i,st,id,[]))) in
  List.iteri (fun i ({contents=Out_(idx,st,f,_)}as r) ->
      r := Out_(idx,st,f, List.filter (fun r' -> r != r') outs);
    ) outs;
  let inp = ref (InpMany_(st,in_some,f,[])) in
  st.out_refs <- List.map (fun o -> WrapOut(o, id)) outs;
  st.inp_refs <- [WrapInpMany(inp, in_some,f)];
  outs, inp

let create ~num =
  create_raw num

let create_one () =
  wrap (create_raw 1) id

let send o v =
  match o with
  | {contents=Out_(i,st,f,_)} ->
     (* FIXME replace List.nth with lazy?? *)
     Lwt_stream_opt.send (List.nth st.stream i) (f v)

let send_many o vf =
  match o with
  | {contents=OutMany_(st,f,_)} ->
     Lwt_list.iteri_p
       (fun i st -> Lwt_stream_opt.send st (f (vf i)))
       st.stream

let receive = function
  | {contents=Inp_(i,st,fs,_)} ->
      (* FIXME replace List.nth with lazy?? *)
     Lwt_stream_opt.receive_wrap ~f:(List.nth fs i) (List.nth st.stream i)

let receive_many = function
  | {contents=InpMany_(st,f,g,_)} ->
     Lwt.map g @@ Lwt_list.map_p (fun st -> Lwt_stream_opt.receive_wrap st ~f) st.stream

