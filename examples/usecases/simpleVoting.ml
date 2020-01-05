open Mpst
open Usecase_util

  let s = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
  let v = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_V=v end); call_obj=(fun o->o#role_V)}}
  let to_s m = to_ m s s s
  let to_v m = to_ m v v v

  let g () =
    choice_at s (to_v ok_or_reject)
      (s, (s --> v) ok @@
            choice_at v (to_s yes_or_no)
              (v, (v --> s) yes @@ (s --> v) result finish)
              (v, (v --> s) no @@ (s --> v) result finish))
      (s, (s --> v) reject @@ finish)

(* global protocol EVoting(role V, role S){
 * 	Authenticate(String) from V to S;
 * 	choice at S {
 * 	   	Ok(String) from S to V;
 * 	   	choice at V {
 * 	     		Yes(String) from V to S;
 * 	   	} or {
 * 			No(String) from V to S;
 * 	    	}
 * 	    Result(Int) from S to V;
 * 	} or {
 *
 *    	Reject(String) from S to V;
 * 	}
 * } *)
