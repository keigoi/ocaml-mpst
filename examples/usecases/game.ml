open Mpst
open Mpst.Util
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
              (srv, (srv --> cli) (playAsA >: get_ty a (game ())) finish)
              (srv, (srv --> cli) (playAsB >: get_ty b (game ())) finish))
      (srv, (srv --> cli) (playAsC >: get_ty c (game ())) finish)
