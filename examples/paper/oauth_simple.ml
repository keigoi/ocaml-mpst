open Mpst;;
open Oauth_simple_labels;;

module OAuth1 = struct
  let oAuth () =
    (s --> c) login @@ (c --> a) pwd @@ (a --> s) auth finish (* a global protocol combinator *)

  (* The service process *)
  let srvThread ch =
  let ch = send ch#role_C#login "Hi" in
  let `auth(_,ch) = receive ch#role_A in
  close ch

  (* The client process *)
  let cliThread ch =
  let `login(x, ch) = receive ch#role_S in
  let ch = send ch#role_A#pwd "asdf" in
  close ch

  (* The authentication process *)
  let authThread ch =
  let `pwd(code,ch) = receive ch#role_C in
  let ch = send ch#role_S#auth () in
  close ch

  (* First we extract the channel vector for each role and then we start three threads *)
  let start () =
    let g = gen (oAuth ()) in
    let (srvCh, cliCh, authCh) = (get_ch s g, get_ch c g, get_ch a g) in
    List.iter 
      Thread.join 
      [Thread.create srvThread srvCh; Thread.create authThread authCh; Thread.create cliThread cliCh]
end
