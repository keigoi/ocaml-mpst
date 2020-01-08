open Mpst
open Usecase_util

  let g () =
    (c --> p) propose @@
      (fix (fun x ->
           choice_at p (to_c accpt_reject_or_propose)
             (p, choice_at p (to_c accpt_or_reject)
                   (p, (p --> c) accpt @@
                         (c --> p) confirm @@ finish)
                   (p, (p --> c) reject @@ finish))
             (p, (p --> c) propose @@
                   choice_at c (to_p accpt_reject_or_propose)
                     (c, choice_at c (to_p accpt_or_reject)
                           (c, (c --> p) accpt @@
                                 (p --> c) confirm @@
                                   finish)
                           (c, (c --> p) reject @@ finish))
                     (c, (c --> p) propose @@
                           x))))

(* global protocol Negotiate(role C, role P)
 * {
 * 	propose(SAP) from C to P;
 * 	rec X
 * 	{
 * 		choice at P
 * 		{
 * 			accpt() from P to C;
 * 			confirm() from C to P;
 * 		}
 * 		or
 * 		{
 * 			reject() from P to C;
 * 		}
 * 		or
 * 		{
 * 			propose(SAP) from P to C;
 * 			choice at C
 * 			{
 * 				accpt() from C to P;
 * 				confirm() from P to C;
 * 			}
 * 			or
 * 			{
 * 				reject() from C to P;
 * 			}
 * 			or
 * 			{
 * 				propose(SAP) from C to P;
 * 				continue X;
 * 			}
 * 		}
 * 	}
 * } *)
