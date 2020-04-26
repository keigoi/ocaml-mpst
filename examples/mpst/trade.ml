(* Trade example from: 	Pierre-Malo Deniélou, Nobuko Yoshida:
 * Multiparty Session Types Meet Communicating Automata. ESOP 2012: 194-213. *)

open Concur_shims
open Mpst
open Mpst.Types
open Mpst.Util

module Util = struct
  let s = {role_index=Zero;
           role_label=
             {make_obj=(fun v -> object method role_S=v end);
              call_obj=(fun o -> o#role_S)}}
  let item =
    {obj={make_obj=(fun v -> object method item=v end);
          call_obj=(fun o -> o#item)};
     var=(fun v -> `item v)}
  let offer =
    {obj={make_obj=(fun v -> object method offer=v end);
          call_obj=(fun o -> o#offer)};
     var=(fun v -> `offer v)}
  let counter =
    {obj={make_obj=(fun v -> object method counter=v end);
          call_obj=(fun o -> o#counter)};
     var=(fun v -> `counter v)}
  let final =
    {obj={make_obj=(fun v -> object method final=v end);
          call_obj=(fun o -> o#final)};
     var=(fun v -> `final v)}
  let result =
    {obj={make_obj=(fun v -> object method result=v end);
          call_obj=(fun o -> o#result)};
     var=(fun v -> `result v)}

  let to_c_or_s =
    {disj_concat=(fun l r -> object method role_C=l#role_C method role_S=r#role_S end);
     disj_splitL=(fun lr -> (lr :> <role_C : _>));
     disj_splitR=(fun lr -> (lr :> <role_S : _>))}
end
open Util

let g =
  gen @@
  (s --> b) item @@
  fix (fun t ->
  choice_at b to_c_or_s
  (b, (b --> c) offer @@
      (c --> b) counter @@
      t)
  (b, (b --> s) final @@
      (b --> c) result @@
      finish))

let () = Random.self_init ()
let (let*) m f = IO.bind m (fun x -> IO.bind (Unix.sleepf (Random.float 0.1)) (fun () -> f x))
let return = IO.return

let ts () =
  let debug s = IO.printl ("s) " ^ s) in
  let (ch:'c) = get_ch s g in
  let itemname = "item0" in
  let* () = debug @@ "sending item name " ^ itemname ^ " to b" in
  let* ch = send ch#role_B#item itemname in
  let* () = debug "sent. receiving the final price from b.." in
  let* `final(price, ch) = receive ch#role_B in
  let* () = debug ("received the final price: " ^ string_of_int price) in
  close ch
    
let tb init_price () =
  let debug s = IO.printl ("b) " ^ s) in
  let ch = get_ch b g in
  let* () = debug "receiving item name from s" in
  let* `item(name,(ch:'c)) = receive ch#role_S in
  let* () = debug @@ "received: "^name in
  let rec loop (ch:'c) pr =
    if pr>100 then begin
        let* () = debug @@ "offering price to c:" ^ string_of_int pr in
        let* ch = send ch#role_C#offer pr in
        let* () = debug "receiving new price from c" in
        let* `counter((pr':int), ch) = receive ch#role_C in
        let* () = debug @@ "received from c:" ^ string_of_int pr' in
        loop ch pr'
      end else begin
        IO.return (ch,pr)
      end
  in
  let* (ch,pr) = loop ch init_price in
  let* () = debug @@ "sending final price to s: " ^ string_of_int pr in
  let* ch = send ch#role_S#final pr in
  let* () = debug @@ "sending the result to c: " ^ string_of_int pr in
  let* ch = send ch#role_C#result pr in
  close ch

let tc () =
  let debug s = IO.printl ("c) " ^ s) in
  let (ch:'c) = get_ch c g in
  let rec loop (ch:'c) =
    let* () = debug "waiting b" in
    let* lab = receive ch#role_B in
    match lab with
    | `offer(price,ch) ->
       let* () = debug @@ "received offer from b:" ^ string_of_int price in
       let newpr = price/2 in
       let* () = debug @@ "sending new price (counter) to b:" ^ string_of_int newpr in
       let* ch = send ch#role_B#counter newpr in
       loop ch
    | `result(pr,ch) ->
       let* () = debug @@ "received the result from b:" ^ string_of_int pr in
       close ch
  in
  loop ch
  
let () =
  IO.main_run (IO_list.iter Thread.join (List.map (fun f -> Thread.create f ()) [ts; tb 250; tc]))
