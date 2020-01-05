open Mpst
open Usecase_util

(* global protocol Calc(role S , role C ) {
 * 	rec Loop {
 * 		choice at C {
 * 			sum(int, int) from C to S ;
 * 			result(int) from S to C ;
 * 			continue Loop ;
 * 		} or {
 * 			multiply(int, int) from S to C;
 * 			result(int) from S to C ;
 * 			continue Loop ;
 * 		} or {
 * 			quit() from C to S ;
 * 			terminate() from S to C ;
 * 		}
 * 	}
 * } *)

  let s = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
  let c = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let to_s m = to_ m s s s
  let to_c m = to_ m c c c

  let g () =
    fix (fun loop ->
        choice_at c (to_s sum_multiply_or_quit)
          (c, choice_at c (to_s (sum_or_multiply))
                (c, (c --> s) sum @@
                      (s --> c) result @@
                        loop)
                (c, (c --> s) multiply @@
                      (s --> c) result @@
                      loop))
          (c, (c --> s) quit @@
                finish))

