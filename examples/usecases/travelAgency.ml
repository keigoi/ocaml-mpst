open Mpst
open Usecase_util

(* global protocol Booking(role C, role A, role S)
 * {
 * 	choice at C
 * 	{
 * 		Query(String) from C to A;
 * 		Quote(Int) from A to C;
 * 		Dummy() from A to S;   // Dummy
 * 		do Booking(C, A, S);
 * 	}
 * 	or
 * 	{
 * 		choice at C
 * 		{
 * 			Yes() from C to A;
 * 			Yes() from A to S;
 * 			Payment(String) from C to S;
 * 			Ack() from S to C;
 * 		}
 * 		or
 * 		{
 * 			No() from C to A;
 * 			No() from A to S;
 * 		}
 * 		Bye() from C to A;
 * 	}
 * } *)
  let c = {role_index=Zero; role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let a = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_A=v end); call_obj=(fun o->o#role_A)}}
  let s = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}

  let to_c m = to_ m c c c
  let to_a m = to_ m a a a
  let to_s m = to_ m s s s

  let g () =
    fix (fun t ->
        choice_at c (to_a query_or_yes_no) (* full merging on s: dummy or yes *)
          (c, (c --> a) query @@
                (a --> c) quote @@
                  (a --> s) dummy @@
                    t)
          (c, choice_at c (to_a yes_or_no) (* full merging on s again: yes or no*)
                (c, (c --> a) yes @@
                      (a --> s) yes @@
                        (c --> s) payment @@
                          (s --> c) ack @@
                            (c --> a) bye @@ finish)
                (c, (c --> a) no @@
                      (a --> s) no @@
                        (c --> a) bye @@ finish)))
