
type conn_ = {name:string; out:Obj.t -> unit; inp:Obj.t Lwt_stream.t}

module M = Functor.F(struct type conn=conn_ end)
module Session = M.Session
module Global = M.Global

open Session
open Global

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

let conv (name,inp,out) = create ~name ~inp ~out

let bipipe_ parentname childname =
  let p_in, c_out = Unix.pipe () in
  let c_in, p_out = Unix.pipe () in
  (parentname ^ "_" ^ childname, p_in, p_out), (childname ^ "_" ^ parentname, c_in, c_out)

let bipipe parentname childname =
  let p, c = bipipe_ parentname childname in
  conv p, conv c

type proc = {procname:string; procbody:conn_ list -> unit}
          
let forkmany fs =
  let rec loop myname fps(* list of pairs of a function and the list (in reverse order) of the pipes to the parents *) =
    (* create the pipes connected to children *)
    let my_pipes, fps =
      List.map (fun (f, parent_pipes_rev) ->
          (* create a new pipe pair and append it to the parent pipe list *)
          let my_pipe, my_pipe_child = bipipe myname f.procname in
          (my_pipe, (f, my_pipe_child::parent_pipes_rev))) fps
      |> List.split
    in
    match fps with
    | [] -> []
    | (f, parent_pipes_rev)::fps ->
       (* start the rest of processes recursively, and get the pipes connected to them *)
       let pipes = loop f.procname fps in
       let pipes = List.rev_append parent_pipes_rev pipes in
       if Unix.fork () = 0 then begin
           (f.procbody pipes : unit);
           exit 0
         end else begin
           my_pipes
         end
  in
  loop "main" (List.map (fun f -> (f, [])) fs)

let repeat f c =
  let xs = ref [] in
  for i=0 to (c-1) do
    xs := f i :: !xs
  done;
  List.rev !xs

type group = {groupname:string; count:int; groupbody:int -> conn_ list list -> unit}

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
let add_connections (myname,mycount) (childname,childcount) =
  repeat (fun i ->
      repeat (fun j -> bipipe myname childname) childcount |> List.split
    ) mycount |> List.split


let forkmany_groups fs =
  (* Printf.printf "forkmany_groups: length:%d\n" @@ List.length fs; *)
  flush stdout;
  let rec loop myname mycount fps(* list of pairs of a function and the pipes to the parents *) =
    (* Printf.printf "%s (size:%d)\n" myname mycount; *)
    flush stdout;
    (* add pipes connected to myself *)
    let my_pipes, fps =
      List.map (fun (f, pipes_rev) ->
          (* Printf.printf "  %s: parent size:%d\n" f.groupname (List.fold_left (fun n xs -> n + List.length xs) 0 pipes_rev); *)
          flush stdout;
          let my_pipes, child_pipes = add_connections (myname,mycount) (f.groupname,f.count)
          in
          let child_pipes = transpose child_pipes in
          (* Printf.printf "  %s: children size:%d\n" f.groupname (List.length child_pipes);
           * Printf.printf "  %s: children all size:%d\n" f.groupname (List.fold_left (fun n xs -> n + List.length xs) 0 child_pipes); *)
          flush stdout;
          (my_pipes, (f, child_pipes::pipes_rev))) fps |> List.split
    in
    match fps with
    | [] -> []
    | ({groupbody;count;groupname}, parent_pipes_rev)::fps ->
       (* Printf.printf "parent pipes size %s is %d:\n" groupname (List.length parent_pipes_rev); *)
       let pipes = loop groupname count fps in
       (* Printf.printf "recursive call on %s done. size %d:\n" groupname (List.length pipes); *)
       let pipes = List.rev_append parent_pipes_rev pipes in
       for i=0 to count-1 do
         (* Printf.printf "%s start. size %d:\n" groupname (List.length ((List.nth pipes i))); *)
         flush stdout;
         if Unix.fork () = 0 then begin
             (groupbody i (List.map (fun xs -> List.nth xs i) pipes) : unit);
             (* print_endline @@ "started: " ^ groupname; *)
             exit 0
           end
       done;
       my_pipes
  in
  let css = loop "A" 1 (List.map (fun f -> (f, [])) fs) in
  List.map List.hd css


let msg =
  {select_label=(fun f -> object method msg v=f v end);
   offer_label=(fun (v,c) -> `msg(v,c));
   channel={
       sender=(fun k -> write (fun v -> `msg(v)) k);
       receiver=(fun k -> try_read (function `msg(v) -> Some(v) | _ -> None) k)
  }}
  
let left =
  {select_label=(fun f -> object method left v=f v end);
   offer_label=(fun (v,c) -> `left(v,c));
   channel={
       sender=(fun k -> write (fun v -> `left(v)) k);
       receiver=(fun k -> try_read (function `left(v) -> Some(v) | _ -> None) k)
  }}

let right =
  {select_label=(fun f -> object method right v=f v end);
   offer_label=(fun (v,c) -> `right(v,c));
   channel={
       sender=(fun k -> write (fun v -> `right(v)) k);
       receiver=(fun k -> try_read (function `right(v) -> Some(v) | _ -> None) k)
  }}

let left_or_right =
  {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}
