(* Trade example from: 	Pierre-Malo Deniélou, Nobuko Yoshida:
 * Multiparty Session Types Meet Communicating Automata. ESOP 2012: 194-213. *)

open Concur_shims
open Mpst
open Mpst.Types
open Mpst.Util
open Printf

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

  let offer_or_result =
      {disj_concat=(fun l r -> object method offer=l#offer method result=r#result end);
       disj_splitL=(fun lr -> (lr :> <offer : _>));
       disj_splitR=(fun lr -> (lr :> <result : _>))}

  let result_or_offer =
    {disj_concat=(fun l r -> object method result=l#result method offer=r#offer end);
      disj_splitL=(fun lr -> (lr :> <result : _>));
      disj_splitR=(fun lr -> (lr :> <offer : _>))}
end
open Util


(* two buyer protocol, s: seller, b: bob, c: carol *)

let g =
    (s --> b) item @@
    fix (fun t ->
      choice_at b (to_c result_or_offer)
      (b, (b --> c) result @@
          (b --> s) final @@
          finish)
      (b, (b --> c) offer @@
          (c --> b) counter @@
          t))

let channels = gen g

let chs = get_ch s channels
let chb = get_ch b channels
let chc = get_ch c channels

let seller initial () =
  let chs = send chs#role_B#item (initial) in
  let `final(_price, chs) = receive chs#role_B in
  close chs
    
let bob () =
  let `item(initial,(ch:'c)) = receive chb#role_S in
  let rec loop (ch:'c) price =
    printf "Bob: price: %d\n" price;
    if price>100 then begin
        let ch = send ch#role_C#offer (initial - price) in
        let `counter(pr', ch) = receive ch#role_C in
        loop ch (initial - pr')
      end else begin
        (ch,price)
      end
  in
  let (ch,price) = loop ch (initial/2) in
  printf "Bob: the final price: %d\n" price;
  let ch = send ch#role_C#result (initial - price) in
  let ch = send ch#role_S#final price in
  close ch

let carol () =
  let rec loop (chc:'c) =
    match receive chc#role_B with
    | `offer(price,chc) ->
      printf "Carol: price: %d\n" price;
      let chc = send chc#role_B#counter (price + 10) in
       loop chc
    | `result(price,chc) ->
      printf "Carol: the final price: %d\n" price;
       close chc
  in
  loop chc
  
let () =
  List.iter
    Thread.join (List.map (fun f -> Thread.create f ())
    [seller 250; bob; carol])
