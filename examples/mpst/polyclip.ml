open Concur_shims
open Mpst
open Polyclip_util

let (let*) = IO.bind

let polyclip () =
  (p --> r) plane @@
  fix @@ fun loop ->
      choice_at p (to_r isabove_or_close)
      (p, (p --> r) is_above @@ (r --> p) res @@
          (p --> r) is_above @@ (r --> p) res @@
          choice_at p (to_c bothin_bothout_or_intersect)
            (p, choice_at p (to_c bothin_or_bothout)
                (p, (p --> c) both_in @@
                    (p --> r) both_in @@ loop)
                (p, (p --> c) both_out @@
                    (p --> r) both_out @@ loop))
            (p, (p --> c) intersect @@
                (p --> r) intersect @@
                (r --> p) res @@
                choice_at p (to_c secout_or_secin)
                (p, (p --> c) secout @@
                    loop)
                (p, (p --> c) secin @@
                    loop)))
      (p, (p --> r) close_ @@
          (p --> c) close_ @@
          finish)
  
let tPro rect planes =
  let g = gen @@ polyclip () in
  let ep = get_ch p g in
  let* (ep : 't) = send (ep#role_R#plane) rect in
  let rec loop (ep : 't) points =
    match points with
    | (v1,v2) :: ps ->
       let* ep = send ep#role_R#is_above v1 in
       let* `res(b1,ep) = receive ep#role_R in
       let* ep = send ep#role_R#is_above v2 in
       let* `res(b2,ep) = receive ep#role_R in
       if b1 && b2 then
         let* ep = send ep#role_C#both_in v2 in
         let* ep = send ep#role_R#both_in () in
         loop ep ps
       else if not (b1 || b2) then
         let* ep = send ep#role_C#both_out () in
         let* ep = send ep#role_R#both_out () in
         loop ep ps
       else
         let* ep = send (ep#role_C#intersect) (v1, v2) in
         let* ep = send (ep#role_R#intersect) (v1, v2) in
         let* `res(ep,i) = receive ep#role_R in
         if not b2 then
           let* ep = send (ep#role_P#secout) i in
           loop ep ps
         else
           let* ep = send (ep#role_P#secin) (i, v2) in
           loop ep ps
    | [] ->
       let* ep = send ep#role_R#close () in
       let* ep = send ep#role_C#close () in
       close ep
  in
  loop ep planes

(* ... we leave rests for readers ...  *)
