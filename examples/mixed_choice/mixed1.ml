open Mpst.BasicCombinators
open Mpst.Unicast

[%%declare_roles a, b, c]
[%%declare_labels req, resp, timeout]

let () =
  let g =
    fix_with [ a; b; c ] (fun t ->
        [%choice_at
          b
            ( (a ==> b) @@ (b --> a) resp t,
              (b --> c) timeout @@ (a ==> b) @@ (b --> a) timeout finish )])
  in
  let (`cons ((sa : 'sa), `cons ((sb : 'sb), `cons (sc, _)))) = extract g in
  let ta =
    Thread.create
      (fun () ->
        print_endline "a started";
        let rec loop (sa : 'sa) i =
          Printf.printf "a: sending %d\n" i;
          let sa = send sa#b i in
          match branch sa#b with
          | `resp sa -> 
            Printf.printf "a: continue\n";
            loop sa (i + 1)
          | `timeout sa -> 
            Printf.printf "a: finishing\n";
            close sa
        in
        loop sa 0)
      ()
  in
  let tb =
    Thread.create
      (fun () ->
        print_endline "b started";
        let rec loop (sb : 'sb) =
          if Random.int 10 <= 2 then begin
            Printf.printf "b: choosing timeout branch\n";
            let _, sb = receive (select sb#c#timeout)#a in
            close (select sb#a#timeout)
          end
          else
            let i, sb = receive sb#a in
            Printf.printf "b: received: %d\n" i;
            loop (select sb#a#resp)
        in
        loop sb)
      ()
  in
  let tc =
    Thread.create
      (fun () ->
        print_endline "c started";
        let (`timeout sc) = branch sc#b in
        Printf.printf "c: finishing\n";
        close sc)
      ()
  in
  List.iter Thread.join [ ta; tb; tc ]
