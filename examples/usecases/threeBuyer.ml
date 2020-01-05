open Mpst
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

  let g () =
    (b --> c) msg @@
    (b --> c) (msg >: (prot b (TwoBuyer.choose ()))) @@
    choice_at c (to_b ok_or_quit)
    (c, (c --> b) ok @@ finish)
    (c, (c --> b) quit @@ finish)
