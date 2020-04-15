open Mpst
open Mpst.Util
open Usecase_util

(* // 1 = B, 2 = A, 3 = S
 * global protocol TwoBuyer(role A, role B, role S)
 * {
 * 	(String) from A to S;
 * 	(Int) from S to A;
 * 	(Int) from S to B;
 * 	(Int) from A to B;
 * 	do TwoBuyerChoice(A, B, S);
 * }
 *
 * aux global protocol TwoBuyerChoice(role A, role B, role S)
 * {
 * 	choice at B
 * 	{
 * 		ok(String) from B to A;
 * 		ok(String) from B to S;
 * 		(Date) from S to B;
 * 	}
 * 	or
 * 	{
 * 		quit() from B to A;
 * 		quit() from B to S;
 * 	}
 * }
 *)

  let choose () =
    choice_at b (to_a ok_or_quit)  (* full merge on s *)
    (b, (b --> a) ok @@
        (b --> s) ok @@
        (s --> b) date @@
        finish)
    (b, (b --> a) quit @@
        (b --> s) quit @@
        finish)

  let g () =
    (a --> s) title @@
    (s --> a) quote @@
    (s --> b) quote @@
    (a --> b) quote_by_two @@
    choose ()
