open Mpst
open Mpst.Util
open Usecase_util

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

