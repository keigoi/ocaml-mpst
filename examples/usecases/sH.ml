open Mpst
open Usecase_util

  let g () =
    (p --> r) plane @@
    fix (fun loop ->
        choice_at p (to_r isabove_or_close)
        (p, (p --> r) is_above @@
          (r --> p) res @@
            (p --> r) is_above @@
              (r --> p) res @@
                choice_at p (to_r bothin_bothout_or_intersect) (* full merge on c (both_in/sec_out/sec_in)*)
                  (p, choice_at p (to_r bothin_or_bothout)
                        (p, (p --> r) both_in @@
                              (p --> c) both_in @@
                                (r --> p) res @@
                                  loop)
                        (p, (p --> r) both_out @@
                              (p --> c) both_out @@
                                loop))
                  (p, (p --> r) intersect @@
                        (r --> p) res @@
                          choice_at p (to_c secout_or_secin)
                            (p, (p --> c) sec_out @@
                                  loop)
                            (p, (p --> c) sec_in @@
                                  loop)))
        (p, (p --> r) close_ @@
              (p --> c) close_ @@
                finish))

(* global protocol SH(role P, role R, role C)
 * {
 * 	Plane(x1:int, x2:int, x3:int, x4:int) from P to R;
 * 	do Loop(P, R, C);
 * }
 *
 * aux global protocol Loop(role P, role R, role C)
 * {
 * 	choice at P
 * 	{
 * 		IsAbove(v1:int) from P to R;
 * 		Res(b1:int) from R to P; // @"b1=0 || b1=1"
 * 		IsAbove(v2:int) from P to R;
 * 		Res(b2:int) from R to P; // @"b2=0 || b2=1"
 * 		choice at P
 * 		{
 * 			BothIn() from P to R; // @"b1=1 && b2=1"
 * 			BothIn(r1:int) from P to C;
 * 			do Loop(P, R, C);
 * 		}
 * 		or
 * 		{
 * 			BothOut() from P to R; // @"b1=0 && b2=0"
 * 			BothOut() from P to C;
 * 			do Loop(P, R, C);
 * 		}
 * 		or
 * 		{
 * 			Intersct(y1:int, y2:int) from P to R; // @"(b1=1 && b2=0) || (b1=0 && b2=1)" // && y1=v1 && y2:v2
 * 			Res(i:int) from R to P;
 * 			choice at P
 * 			{
 * 				SecOut(r2:int) from P to C; // @"b2=0" // && r2=i
 * 				do Loop(P, R, C);
 * 			}
 * 			or
 * 			{
 * 				SecIn(r3:int, r4:int) from P to C; // @"b2=1" //  && (r3=i && r4=v2)
 * 				do Loop(P, R, C);
 * 			}
 * 		}
 * 	}
 * 	or
 * 	{
 * 		Close() from P to R;
 * 		Close() from P to C;
 * 	}
 * } *)
