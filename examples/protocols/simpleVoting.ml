open Mpst
open Usecase_util

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
