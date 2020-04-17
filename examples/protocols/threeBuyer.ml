open Mpst
open Mpst.Util
open Usecase_util

 (* global protocol Proto(role C, role B)
 * {
 * 	(Int) from B to C;  // Should be same value as the Int on line 19
 * 	(TwoBuyerChoice@B) from B to C;
 * 	choice at C
 * 	{
 * 		ok() from C to B;  // Not enforced: consistency of "ok" choice here and in the delegated session
 * 	}
 * 	or
 * 	{
 * 		quit() from C to B;
 * 	}
 * } *)

  let twobuyer_choose () =
    choice_at b (to_a ok_or_quit)  (* full merge on s *)
    (b, (b --> a) ok @@
        (b --> s) ok @@
        (s --> b) date @@
        finish)
    (b, (b --> a) quit @@
        (b --> s) quit @@
        finish)

  let g () =
    (b --> c) msg @@
    (b --> c) (msg >: (get_ty b (twobuyer_choose ()))) @@
    choice_at c (to_b ok_or_quit)
    (c, (c --> b) ok @@ finish)
    (c, (c --> b) quit @@ finish)
