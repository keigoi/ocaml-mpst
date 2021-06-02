open Concur_shims
open Mpst
open Mpst.Types
open Mpst.Util

let d = {role_index=Succ(Succ(Succ Zero)); role_label={make_obj=(fun x -> object method role_D=x end); call_obj=(fun obj->obj#role_D)}}


let sa, sb, sc, sd = 
  (* A: b!{left.end, right.end}
   * B: a?{left.d!left, right.d!right.end}
   * C: d!msg.end
   * D: c?msg.b?{left.end, right.end}  <-- *** HERE is the gap between ch-vector value and type ***
   * 
   * PROBLEM: D's continuation b?left and b?right is NOT merged in the channel-vector level
   *)
  let g =
    gen @@
    fix (fun t ->
      choice_at a (to_b left_or_right)
      (a, (a --> b) left @@
          (c --> d) msg @@
          (b --> d) left @@ (* 1: D's session is b?left *)
          t)
      (a, (a --> b) right @@
          (c --> d) msg @@
          (b --> d) right @@ (* 2: D's session is b?right *)
          t))
  in
  get_ch a g, get_ch b g, get_ch c g, get_ch d g

let () = print_endline "start."
(*
 * A
 *)
let (ta:Thread.t) =
  let rec f sa =
    let sa = send sa#role_B#left () in
    f sa
  in
  Thread.create f sa

(*
 * B
 *)
let (tb:Thread.t) =
  let rec f sb =
      match receive sb#role_A with
      | `left((), sb) ->
        f (send sb#role_D#left ())
      | `right((), sb) ->
        f (send sb#role_D#right ())
  in
  Thread.create f sb

(*
 * C
 *)
let (tc:Thread.t) =
  let rec f sc =
    f (send sc#role_D#msg ())
  in
  Thread.create f sc

(*
 * D (deadlock)
 *)  
let () =
  let rec f sd =
    (* sd is non-deterministically chosen between 1: and 2: above *)
    let `msg((),sd) = receive sd#role_C in 
    print_endline "D";
    match receive sd#role_B with (* if sd is 2: above, it stucks (deadlock) *)
    | `left((),sd) -> 
      print_endline "left";
      f sd
    | `right((),sd) -> 
      f sd
  in 
  f sd


(*
 * The fix would involve a whole change of communication APIs.
 * 
 * While two sessions c?msg.b?left and c?msg.b?right must be merged into one,
 * the implementation of c?msg is like 
 *
 *   let f = (fun x -> `msg (x, ch)) in
 *   let v = Event.receive ... in
 *   f v
 *
 * To remedy this, first we must extract the channel-vector ch from the part (fun x -> `msg (x,ch))
 * this is not easy but doable.
 * The issue is that 'c is NOT mergeable: 'c is a bare object <b=inp> without any merging capability.
 *
 * To make 'c mergeable, we must rewrite all Mergeable features into a class, which would require a
 * total rewrite of Mergeable module into a class.
 *
 * This is rather a library design problem but not a fault in the theory; but
 * having a user-friendly type representation is currently a nice feature in ocaml-mpst
 * which might be lost if we rewrite 'c into 'c merge or something.
 * Firstly, we possibly lose method-chain style API like "ch#role_A#msg"
 * because ch is not an object anymore, but an abstract mergeable channel type.
 *
 * Making mergeability ch into an OCaml object would be a solution.
 *)
