open Mpst
open Usecase_util

(* global protocol Proto(role C, role S)
 * {
 * 	choice at S
 * 	{
 * 		playAsA(Game@A) from S to C;
 * 	}
 * 	or
 * 	{
 * 		playAsB(Game@B) from S to C;
 * 	}
 * 	or
 * 	{
 * 		playAsC(Game@C) from S to C;
 * 	}
 * }
 *
 *
 * global protocol Game(role A, role B, role C)
 * {
 * 	// Arbitrary for now
 * 	rec X
 * 	{
 * 		choice at A
 * 		{
 * 			1() from A to B;
 * 			1() from B to C;
 * 			1() from C to A;
 * 			continue X;
 * 		}
 * 		or
 * 		{
 * 			2() from A to B;
 * 			2() from B to C;
 * 			2() from C to A;
 * 		}
 * 	}
 * } *)
  (* let main () = *)

  let srv = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Srv=v end); call_obj=(fun o->o#role_Srv)}}
  let cli = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Cli=v end); call_obj=(fun o->o#role_Cli)}}
  let a = {role_index=Zero; role_label={make_obj=(fun v -> object method role_A=v end); call_obj=(fun o->o#role_A)}}
  let b = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_B=v end); call_obj=(fun o->o#role_B)}}
  let c = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let to_a m = to_ m a a a
  let to_b m = to_ m b b b
  let to_c m = to_ m c c c
  let to_srv m = to_ m srv srv srv
  let to_cli m = to_ m cli cli cli

  let game () =
    fix (fun t ->
        choice_at a (to_b left_or_right)
          (a, (a --> b) left @@
              (b --> c) left @@
              (c --> a) left @@
              t)
          (a, (a --> b) right @@
              (b --> c) right @@
              (c --> a) right @@
              finish))

  let main () =
    choice_at srv (to_cli playAsA_playAsB_or_playAsC)
      (srv, choice_at srv (to_cli playAsA_or_playAsB)
              (srv, (srv --> cli) (playAsA >: prot a (game ())) finish)
              (srv, (srv --> cli) (playAsB >: prot b (game ())) finish))
      (srv, (srv --> cli) (playAsC >: prot c (game ())) finish)
