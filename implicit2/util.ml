
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

let conv ((inp,out),name) = create ~name ~inp ~out

let bipipe () =
  (* let c_in, s_out = Lwt_unix.pipe_in () in *)
  let c_in, s_out = Unix.pipe () in
  let s_in, c_out = Unix.pipe () in
  (c_in, c_out), (s_in, s_out)

type proc = {procname:string; procbody:conn_ list -> unit}
          
let forkmany fs =
  let rec loop name fs(* list of pairs of a function and the pipes to the parents *) =
    (* add pipes connected to myself *)
    let cs, fs =
      List.map (fun (f,ss) ->
          let c, s = bipipe () in
          ((c,name ^ "_" ^ f.procname), (f, (s,f.procname ^ "_" ^ name)::ss))) fs
      |> List.split
    in
    (* let temp_in, temp_out = Unix.pipe () in
     * let temp_in2, temp_out2 = Unix.pipe () in *)
    match fs with
    | [] -> []
    | (f,ss)::fs ->
       if Unix.fork () = 0 then begin
           (* let temp_out = Unix.out_channel_of_descr temp_out in
            * let temp_in2 = Unix.in_channel_of_descr temp_in2 in
            * output_value temp_out (); flush temp_out;
            * (input_value temp_in2 : unit); *)
           let cs = loop f.procname fs in
           (f.procbody (List.map conv (List.rev_append ss cs)) : unit);
           exit 0
         end else begin
           (* let temp_in = Unix.in_channel_of_descr temp_in in
            * let temp_out2 = Unix.out_channel_of_descr temp_out2 in
            * output_value temp_out2 (); flush temp_out2;
            * (input_value temp_in : unit); *)
           cs
         end
  in
  let cs = loop "A" (List.map (fun f -> (f, [])) fs) in
  List.map conv cs

let repeat f c =
  let xs = ref [] in
  for i=0 to (c-1) do
    xs := f () :: !xs
  done;
  List.rev !xs

type group = {groupname:string; count:int; func:int -> conn_ list list -> unit}
           
(* let forkmany_groups fs =
 *   let rec loop fs(\* list of pairs of a function and the pipes to the parents *\) =
 *     (\* add pipes connected to myself *\)
 *     let css, fs =
 *       List.map (fun (f,sss) ->
 *           let cs, ss = repeat bipipe f.count |> List.split in
 *           (cs, (f, ss::sss))) fs
 *       |> List.split
 *     in
 *     match fs with
 *     | [] -> []
 *     | ({func;count;groupname},sss)::fs ->
 *        for i=0 to count-1 do
 *          if Unix.fork () <> 0 then begin
 *              let css = loop fs in
 *              (func i (List.map (List.map (conv groupname)) (List.rev_append sss css)) : unit);
 *              exit 0
 *            end
 *        done;
 *        css
 *   in
 *   let css = loop (List.map (fun f -> (f, [])) fs) in
 *   List.map (List.map (conv "main")) css *)



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
