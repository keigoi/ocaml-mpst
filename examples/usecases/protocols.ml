open Mpst
open Usecase_util

module TwoBuyer = TwoBuyer


module ThreeBuyer = ThreeBuyer

module Calc = Calc

module Fibo = Fibo

module Sh = Sh

module SapNego = SapNego

module SimpleVoting = SimpleVoting

module TravelAgency = TravelAgency

module SupplierInfo_microservice = SupplierInfo_microservice

module Smtp = Smtp

module SleepingBarber = SleepingBarber

module Game = Game

module MapReduce = MapReduce

module NQueen = NQueen

module Santa = Santa

module OAuth = OAuth

module Dnsmini = Dnsmini

module SigSmoker = struct
  (* global protocol CigaretteSmoker ( role Arbiter as A , role Smoker [1.. N ] as S ) {
   *     rec Loop {
   *             choice at A {
   *                 start_smoking () from A to S [ i ];
   *                 started_smoking () from S [ i ] to A ;
   *                 continue Loop ;
   *               } or {
   *                 exit () from A to S [ i .. n ];
   *               }}} *)
  (* let a = {role_index=Zero; role_label={make_obj=(fun v -> object method role_A=v end); call_obj=(fun o->o#role_A)}}
   * let s = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
   * let to_a m = to_ m a a a
   * let to_s m = to_ m s s s *)

  (* let g () =
   *   fix (fun loop ->
   *       choice_at a (to_s start_smoking_or_exit)
   *         (a, (oneof a s) start_smoking @@
   *               (oneof s a) started_smoking @@
   *                 loop)
   *         (a, (gather a s) exit @@ finish)) *)

end

module OAuth_paper = struct
  let s = {role_index=Zero; role_label={make_obj=(fun v -> object method role_S=v end); call_obj=(fun o->o#role_S)}}
  let a = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_A=v end); call_obj=(fun o->o#role_A)}}
  let c = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_C=v end); call_obj=(fun o->o#role_C)}}

  let to_s m = to_ m s s s
  let to_a m = to_ m a a a
  let to_c m = to_ m c c c

  let oAuth2 () =
    (s --> c) login @@ (c --> a) password @@ (a --> c) auth finish

  let oAuth3 () =
    choice_at s (to_c login_or_cancel) (* full merging at a: password or quit *)
      (s, (s --> c) login @@
          fix @@ fun t ->
            (c --> a) password @@
            choice_at a (to_s auth_or_again)
            (a, (a --> s) auth @@
                (s --> c) auth @@
                finish)
            (a, (a --> s) again @@
                (s --> c) again @@
                t))
      (s, (s --> c) cancel @@
          (c --> a) quit @@
          finish)

  let oAuth = gen @@
    choice_at s (to_c login_or_cancel)
      (s, (s --> c) login @@
          (c --> a) password @@
          (a --> s) auth @@
          finish)
      (s, (s --> c) cancel @@
          (c --> a) quit @@
          finish)

  let thread f = ignore (Thread.create f ())
  let () =
    thread (fun () ->
      let ep = get_ch s oAuth in
      if true then begin
        let ep = send (ep#role_C#login) "asdf" in
        let `auth((), ep) = receive (ep#role_A) in
        close ep
      end else begin
        let ep = send (ep#role_C#cancel) () in
        close ep
      end);
    thread (fun () ->
      let ep = get_ch c oAuth in
      match receive (ep#role_S) with
      | `login((_x:string), ep) ->
         let ep = send (ep#role_A#password) 123 in
         close ep
      | `cancel((), ep) ->
         let ep = send (ep#role_A#quit) () in
         close ep);
    let ep = get_ch a oAuth in
    match receive (ep#role_C) with
    | `password((_i:int),ep) ->
       let ep = send (ep#role_S#auth) () in
       close ep
    | `quit((), ep) ->
       close ep


end

(*
< s : [> `login of 'a * < a : < password : ('b one * < a : [> `auth of 'c * close ] inp >) ] >

< s : [`login of string * < a : < password : (int * close ) out > > ] >

receive {.s: [`login (string, send {.a : {.password: (int,  close) } } ) ] }

 *)
