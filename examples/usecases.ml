open Mpst
open Usecase_util

module TwoBuyer = struct
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

  let s = {c with role_label={make_obj=(fun v->object method role_S=v end);
                              call_obj=(fun o->o#role_S)}}
  let to_s mrg = to_ mrg s s s
  
  let choose () =
    choice_at b (to_a ok_or_quit)
    (b, (b --> a) ok @@
        (b --> s) ok @@
        (s --> b) date @@
        finish)
    (b, (b --> a) quit @@
        (b --> s) quit @@
        finish)
    
  let g () =
    gen @@
    (a --> s) title @@
    (s --> a) quote @@
    (s --> b) quote @@
    (a --> b) quote_by_two @@
    choose ()

end

    
module ThreeBuyer = struct
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
end  

module Calc = struct
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
    
end  

module Fibo = struct
(* global protocol Fibonacci(role A, role B)
 * {
 * 	rec Fib
 * 	{
 * 		choice at A
 * 		{
 * 			fibonacci(Long) from A to B;
 * 			fibonacci(Long) from B to A;
 * 			continue Fib;
 * 		}
 * 		or
 * 		{
 * 			stop() from A to B;
 * 		}
 * 	}
 * } *)
  let g () =
    fix (fun fib ->
        choice_at a (to_b fibonacci_or_stop)
          (a, (a --> b) fibonacci @@
                (b --> a) fibonacci @@
                  fib)
          (a, (a --> b) stop @@
                finish))
end

module SH = struct
  let p = {role_index=Zero; role_label={make_obj=(fun v -> object method role_P=v end); call_obj=(fun o->o#role_P)}}
  let r = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_R=v end); call_obj=(fun o->o#role_R)}}
  let c = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let to_p m = to_ m p p p
  let to_r m = to_ m r r r
  let to_c m = to_ m c c c

  let g () =
    (p --> r) plane @@
    fix (fun loop ->
        choice_at p (to_r isabove_or_close)
        (p, (p --> r) is_above @@
          (r --> p) res @@
            (p --> r) is_above @@
              (r --> p) res @@
                choice_at p (to_r bothin_bothout_or_intersect)
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
end
              
module SAPNego = struct
  let c = {role_index=Zero; role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let p = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_P=v end); call_obj=(fun o->o#role_P)}}
  let to_c m = to_ m c c c
  let to_p m = to_ m p p p

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
end  

module SimpleVoting = struct
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
end

module TravelAgency = struct
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
        choice_at c (to_a query_or_yes_no)
          (c, (c --> a) query @@
                (a --> c) quote @@
                  (a --> s) dummy @@
                    t)
          (c, choice_at c (to_a yes_or_no)
                (c, (c --> a) yes @@
                      (a --> s) yes @@
                        (c --> s) payment @@
                          (s --> c) ack @@
                            (c --> a) bye @@ finish)
                (c, (c --> a) no @@
                      (a --> s) no @@
                        (c --> a) bye @@ finish)))
end
