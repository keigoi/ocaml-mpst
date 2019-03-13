open Mpst_base

type 'n conn = {name:'n; out:Obj.t -> unit; inp:Obj.t Lwt_stream.t; close:unit -> unit}

let (>>=) = Lwt.(>>=)

let create ~name ~inp:fdin ~out:fdout =
  let inpchan = Unix.in_channel_of_descr fdin in
  let outchan = Unix.out_channel_of_descr fdout in
  {name;
   out=(fun v -> output_value outchan v; flush outchan);
   inp=
     Lwt_stream.from_direct (fun () ->
         Some(input_value inpchan)
       );
   close=(fun () -> Unix.close fdin; Unix.close fdout)
  }

let write wrap {name;out; _} v =
  out (Obj.repr (wrap v))
  
let try_read unwrap {name;inp; _} =
  Lwt_stream.peek inp >>= function
  | None -> Lwt.fail (Failure "end of stream")
  | Some(v) -> 
     match unwrap (Obj.obj v) with
     | Some(v) ->
        Lwt_stream.next inp >>= fun _ ->
        Lwt.return (Obj.obj v)
     | None ->
        Lwt.fail ReceiveFail

let bipipe parentname childname =
  let p_in, c_out = Unix.pipe () in
  let c_in, p_out = Unix.pipe () in
  create ~name:parentname ~inp:p_in ~out:p_out,
  create ~name:childname ~inp:c_in ~out:c_out
       
let mpst_pipes roles =
  let rec loop myrole children =
    (* make pipes between myrole and children, and update children with a pipe to myrole *)
    let mypipes, children =
      List.map (fun (childrole, childparents_rev) ->
          let mypipe, childpipe = bipipe myrole childrole in
          (childrole, mypipe), (childrole, (myrole, childpipe)::childparents_rev)) children
      |> List.split
    in
    match children with
    | [] -> [], []
    | (r, childparents_rev)::children ->
       (* and make pipes among children recursively *)
       let childchildren, rest = loop r children in
       let childpipes = List.rev_append childparents_rev childchildren in
       mypipes, childpipes::rest
  in
  match roles with
  | [] -> []
  | r::rs ->
     let ps, pss = loop r (List.map (fun r -> (r,[])) rs) in
     ps::pss

let repeat f c =
  let xs = ref [] in
  for i=0 to (c-1) do
    xs := f i :: !xs
  done;
  List.rev !xs

let rec transpose : 'a list list -> 'a list list = fun xss ->
  match xss with
  | [] -> []
  | []::_ -> []
  | xss ->
     let hds, tls =
       List.map (fun xs -> List.hd xs, List.tl xs) xss |> List.split
     in
     hds :: transpose tls

(* outer list: parents, inner list: children*)
let pipes (myname,mycount) (childname,childcount) =
  repeat (fun i ->
      repeat (fun j -> bipipe myname childname) childcount |> List.split
    ) mycount |> List.split

type 'a entry = Entry of 'a
type 'a peer = Peer of 'a
  
let mpst_pipes_groups roles =
  let rec loop myrole mycount children =
    let mypipes, children =
      List.map (fun (childrole, childcount, childparents_rev) ->
          let mypipes, childpipes = pipes (myrole,mycount) (childrole,childcount) in
          let childpipes = transpose childpipes in
          ((childrole, mypipes), (childrole, childcount, (myrole, childpipes)::childparents_rev))) children
      |> List.split
    in
    match children with
    | [] -> [], []
    | (childrole,childcount,childparents_rev)::children ->
       let childchildren, rest = loop childrole childcount children in
       let childpipes = List.rev_append childparents_rev childchildren in
       mypipes, childpipes::rest
  in
  match roles with
  | [] -> []
  | (r,c)::rs ->
     let ps, pss = loop r c (List.map (fun (r,c) -> (r,c,[])) rs) in
     List.combine (List.map fst roles) (ps::pss)
