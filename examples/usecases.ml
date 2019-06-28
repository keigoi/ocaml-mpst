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

module SapNego = struct
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

module SupplierInfo_microservice = struct
  let loginsvc     = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Login=v end); call_obj=(fun o->o#role_Login)}}
  let requestor    = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Request=v end); call_obj=(fun o->o#role_Request)}}
  let authorisesvc = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Auth=v end); call_obj=(fun o->o#role_Auth)}}
  let filtersvc    = {role_index=Succ (Succ (Succ Zero)); role_label={make_obj=(fun v -> object method role_Filter=v end); call_obj=(fun o->o#role_Filter)}}
  let suppliersvc  = {role_index=Succ (Succ (Succ (Succ Zero))); role_label={make_obj=(fun v -> object method role_Supply=v end); call_obj=(fun o->o#role_Supply)}}
  let contractsvc  = {role_index=Succ (Succ (Succ (Succ (Succ Zero)))); role_label={make_obj=(fun v -> object method role_Contract=v end); call_obj=(fun o->o#role_Contract)}}

  let to_loginsvc m = to_ m loginsvc loginsvc loginsvc
  let to_requestor m = to_ m requestor requestor requestor
  let to_authorisesvc m = to_ m authorisesvc authorisesvc authorisesvc
  let to_filtersvc m = to_ m filtersvc filtersvc filtersvc
  let to_suppliersvc m = to_ m suppliersvc suppliersvc suppliersvc
  let to_contractsvc m = to_ m contractsvc contractsvc contractsvc


(* aux global protocol FilterInfo<sig Query>
 * (role authorisersvc, role filtersvc) {
 * Query connect authorisersvc to filtersvc;
 * filtered() from filtersvc to authorisersvc;
 * disconnect authorisersvc and filtersvc;
 * } *)
  let filterinfo cont =
    (authorisesvc --> filtersvc) query @@
      (filtersvc --> authorisesvc) filtered @@
        cont

(* aux global protocol SuppInfo (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc
 * ) {
 * choice at authorisersvc {
 * // DENIED
 * deny() from authorisersvc to requestor;
 * } or {
 * connect authorisersvc to suppliersvc;
 * // PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR
 * getsuppliers() from authorisersvc to suppliersvc;
 * suppliers() from suppliersvc to authorisersvc;
 * do FilterInfo
 * <filterSuppliers(usercontext, filters, supplierdetails)>
 * //<filterContracts(usercontext, filters, supplierdetails)>
 * (authorisersvc, filtersvc);
 * disconnect authorisersvc and suppliersvc;
 * suppliers() from authorisersvc to requestor;
 * }
 * } *)
  (* XXX non-directed chocie *)
  let to_requestor_or_suppliersvc =
    {obj_merge=(fun l r -> object method role_Request=l#role_Request method role_Supply=r#role_Supply end);
     obj_splitL=(fun lr -> (lr :> <role_Request : _>));
     obj_splitR=(fun lr -> (lr :> <role_Supply : _>))}

  let suppinfo () =
    choice_at authorisesvc (to_requestor_or_suppliersvc)
      (authorisesvc, (authorisesvc --> requestor) deny @@
                       (* dummy sending due to lack of explicit connection handling  *)
                       (authorisesvc --> suppliersvc) dummy @@
                         (authorisesvc --> filtersvc) dummy @@
                           finish)
      (authorisesvc, (authorisesvc --> suppliersvc) getsuppliers @@
                       (suppliersvc --> authorisesvc) suppliers @@
                         filterinfo @@
                           (authorisesvc --> requestor) suppliers @@
                             finish)


(* aux global protocol ContractInfo (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role contractsvc
 * ) {
 * choice at authorisersvc {
 * // DENIED
 * deny() from authorisersvc to requestor;
 * } or {
 * connect authorisersvc to contractsvc;
 * // PREPARE FILTERED SUPPLIER INFO FOR REQUESTOR
 * getcontracts() from authorisersvc to contractsvc;
 * contracts() from contractsvc to authorisersvc;
 * do FilterInfo
 * <filterContracts(usercontext, filters, contractdetails)>
 * (authorisersvc, filtersvc);
 * disconnect authorisersvc and contractsvc;
 * contracts() from authorisersvc to requestor;
 * }
 * } *)

  (* XXX non-directed chocie *)
  let to_requestor_or_contractsvc =
    {obj_merge=(fun l r -> object method role_Request=l#role_Request method role_Contract=r#role_Contract end);
     obj_splitL=(fun lr -> (lr :> <role_Request : _>));
     obj_splitR=(fun lr -> (lr :> <role_Contract : _>))}

  let contractinfo () =
    choice_at authorisesvc to_requestor_or_contractsvc
      (authorisesvc, (authorisesvc --> requestor) deny @@
                       (* dummy sending due to lack of explicit connection handling  *)
                       (authorisesvc --> contractsvc) dummy @@
                         (authorisesvc --> filtersvc) dummy @@
                           finish)
      (authorisesvc, (authorisesvc --> contractsvc) getcontracts @@
                       (contractsvc --> authorisesvc) contracts @@
                           filterinfo @@
                             (authorisesvc --> requestor) contracts @@ finish)

(* aux global protocol Main (
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc,
 * role contractsvc
 * ) {
 * choice at requestor {
 * // GET SUPPLIER INFO
 * getsuppliers(uuid) from requestor to authorisersvc;
 * do SuppInfo(requestor, authorisersvc, filtersvc, suppliersvc);
 * } or {
 * // GET CONTRACT INFO
 * getcontracts() from requestor to authorisersvc;
 * do ContractInfo(requestor, authorisersvc, filtersvc, contractsvc);
 * }
 * do Main(requestor, authorisersvc, filtersvc, suppliersvc, contractsvc);
 * } *)

  let main () =
    choice_at requestor (to_authorisesvc getsuppliers_or_getcontracts)
      (requestor, (requestor --> authorisesvc) getsuppliers @@
                    (authorisesvc --> contractsvc) dummy @@
                      suppinfo ())
      (requestor, (requestor --> authorisesvc) getcontracts @@
                    (authorisesvc --> suppliersvc) dummy @@
                      contractinfo ())

(* explicit global protocol PartnershipSupplier (
 * role loginsvc,
 * role requestor,
 * role authorisersvc,
 * role filtersvc,
 * role suppliersvc,
 * role contractsvc
 * ) {
 * connect requestor to loginsvc;
 * login(username, password) from requestor to loginsvc;
 * choice at loginsvc {
 * loginfailure() from loginsvc to requestor;
 * disconnect requestor and loginsvc;
 * } or {
 * loginsuccess() from loginsvc to requestor;
 * connect requestor to authorisersvc;
 * do Main(requestor, authorisersvc, filtersvc, suppliersvc, contractsvc);
 * }
 * } *)

  let partnership_supplier () =
    (requestor --> loginsvc) login @@
      choice_at loginsvc (to_requestor loginfailure_or_loginsuccess)
        (loginsvc, (loginsvc --> requestor) loginfailure @@
                     (requestor --> authorisesvc) dummy @@
                       (authorisesvc --> contractsvc) dummy @@
                         (authorisesvc --> suppliersvc) dummy @@
                           (authorisesvc --> filtersvc) dummy @@
                             finish)
        (loginsvc, (loginsvc --> requestor) loginsuccess @@
                     main ())
end

module Smtp = struct
  let s     = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
  let c    = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}
  let to_s m = to_ m s s s
  let to_c m = to_ m c c c

(* aux global protocol Mail(role S, role C)
 * {
 * 	rec Z1
 * 	{
 * 		choice at C
 * 		{
 * 			Mail from C to S; //Mail from:<a@b.com>
 * 			choice at S
 * 			{
 * 				501 from S to C;
 * 				continue Z1;
 * 			}
 * 			or
 * 			{
 * 				250 from S to C;
 *
 * 				rec Z2
 * 				{
 * 					choice at C
 * 					{
 * 						Rcpt from C to S; //Rcpt to:<c@d.com>
 * 						choice at S
 * 						{
 * 							250 from S to C;
 * 							continue Z2;
 * 						}
 * 					}
 * 					or
 * 					{
 * 						Data from C to S;
 * 						354 from S to C;
 * 						rec Z3
 * 						{
 * 							choice at C
 * 							{
 * 								DataLine from C to S;
 * 								continue Z3;
 * 							}
 * 							or
 * 							{
 * 								Subject from C to S; //Subject:<my Subject>
 * 								continue Z3;
 * 							}
 * 							or
 * 							{
 * 								EndOfData from C to S; // CRLF.CRLF
 * 								250 from S to C;
 * 								continue Z1;
 * 							}
 * 						}
 * 					}
 * 				}
 * 			}
 * 		}
 * 		or
 * 		{
 * 			Quit from C to S;
 * 			221 from S to C;
 * 		}
 * 	}
 * } *)

  let mail () =
    fix (fun z1 ->
        choice_at c (to_s mail_or_quit)
          (c, (c --> s) mail @@
                (choice_at s (to_c _501_or_250)
                   (s, (s --> c) _501 z1)
                   (s, (s --> c) _250 @@
                         fix (fun z2 ->
                             choice_at c (to_s rcpt_or_data)
                               (c, (c --> s) rcpt @@
                                     (s --> c) _250 z2)
                               (c, (c --> s) data @@
                                     (s --> c) _354 @@
                                       (c --> s) mailbody @@
                                         (s --> c) _250 z1)))))
          (c, (c --> s) quit @@ finish))

(* aux global protocol Auth(role S, role C)
 * {
 * 	rec Y
 * 	{
 * 		choice at C
 * 		{
 * 			Auth from C to S;
 * 			choice at S
 * 			{
 * 				235 from S to C;
 * 				do Mail(S, C);
 * 			}
 * 			or
 * 			{
 * 				535 from S to C;
 * 				continue Y;
 * 			}
 * 			//.. 501 Invalid base64 Data
 * 		}
 * 		or
 * 		{
 * 			Quit from C to S;
 * 		}
 * 	}
 * } *)
  let auth () =
    fix (fun y ->
        choice_at c (to_s auth_or_quit)
           (c, (c --> s) auth @@
                 choice_at s (to_c _235_or_535)
                 (s, (s --> c) _235 @@
                       mail ())
                 (s, (s --> c) _535 y))
           (c, (c --> s) quit @@
                 finish))

(* aux global protocol SecureEhlo(role S, role C)
 * {
 * 	choice at C
 * 	{
 * 		Ehlo from C to S;
 * 		rec X
 * 		{
 * 			choice at S
 * 			{
 * 				250d from S to C;
 * 				continue X;
 * 			}
 * 			or
 * 			{
 * 				250 from S to C;
 * 				do Auth(S, C);
 * 			}
 * 		}
 * 	}
 * 	or
 * 	{
 * 		Quit from C to S;
 * 	}
 * } *)
  let secure_ehlo () =
    choice_at c (to_s ehlo_or_quit)
      (c, (c --> s) ehlo @@
            fix (fun x ->
                choice_at s (to_c _250d_or_250)
                  (s, (s --> c) _250d x)
                  (s, (s --> c) _250 @@
                        auth ())))
      (c, (c --> s) quit finish)


  (* aux global protocol StartTls(role S, role C)
   * {
   * 	choice at C
   * 	{
   * 		StartTls from C to S;
   * 		220 from S to C;
   * 		// Do TLS handshake here: level below the application level protocol (like regular TCP handshake)
   * 		do SecureEhlo(S, C);
   * 	}
   * 	or
   * 	{
   * 		Quit from C to S;
   * 	}
   * } *)
  let starttls () =
    choice_at c (to_s starttls_or_quit)
      (c, (c --> s) starttls @@
            (s --> c) _220 @@
              secure_ehlo ())
      (c, (c --> s) quit @@
            finish)

  (* aux global protocol Ehlo(role S, role C)
   * {
   * 	choice at C
   * 	{   Ehlo() from C to S;
   * 		rec X
   * 		{
   * 			choice at S
   * 			{
   * 				//250 from S to C;
   * 				250d from S to C;
   * 				continue X;
   * 			}
   * 			or
   * 			{
   * 				250 from S to C;
   * 				do StartTls(S, C);
   * 			}
   * 			or
   * 			{
   * 				220 from S to C;
   * 				do StartTls(S, C);
   * 			}
   * 		}
   * 	}
   * 	or
   * 	{
   * 		Quit from C to S;
   * 	}
   * } *)
  let ehlo () =
    choice_at c (to_s ehlo_or_quit)
      (c, (c --> s) ehlo @@
            fix (fun x ->
                choice_at s (to_c _250d_250_or_220)
                  (s, choice_at s (to_c _250d_or_250)
                        (s, (s --> c) _250d x)
                        (s, (s --> c) _250 @@ starttls ()))
                  (s, (s --> c) _220 @@ starttls ())))
      (c, (c --> s) quit @@ finish)

(* global protocol Smtp(role S, role C)
 * {
 * 	220 from S to C;
 * 	do Ehlo(S, C);
 * } *)
  let smtp () =
    (s --> c) _220 @@ ehlo ()

end
