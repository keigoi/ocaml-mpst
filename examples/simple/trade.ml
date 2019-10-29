(* Trade example from: 	Pierre-Malo Deniélou, Nobuko Yoshida:
 * Multiparty Session Types Meet Communicating Automata. ESOP 2012: 194-213. *)

open Mpst;;

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
  {disj_merge=(fun l r -> object method role_C=l#role_C method role_S=r#role_S end);
   disj_splitL=(fun lr -> (lr :> <role_C : _>));
   disj_splitR=(fun lr -> (lr :> <role_S : _>))}
  
let g () =
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


