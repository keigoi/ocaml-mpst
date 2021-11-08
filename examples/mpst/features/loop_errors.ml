(* Fail-fast error checking *)

open Concur_shims
open Mpst
open Mpst.Util

let ( let* ) m f = IO.bind m (fun x -> IO.bind (IO.pause ()) (fun () -> f x))

(* check that μt.t is detected before get_ch *)
let test1 =
  print_endline "test1: checking that μt.t is detected correctly";
  try
    let bogus = fix (fun t -> fix (fun _ -> t)) in
    let _g = gen @@ (a --> b) msg @@ bogus in
    failwith "unexpected (test1)"
  with Mpst.UnguardedLoopSeq ->
    print_endline "  exception correctly occurred (test1)\n"

(* merging failure between "end" type and reception *)
let test2 =
  print_endline
    "test2: checking that a merging error between μt.t and input type detected \
     correctly";
  try
    let _g =
      gen
      @@ choice_at a (to_b left_or_right)
           ( a,
             (a --> b) left
             @@ fix (fun t -> (a --> b) msg @@ (* closed_at c *) t) )
           (* this "closed_at c" should be there, according to MPST theory. (!) *)
           (a, (a --> b) right @@ (a --> c) msg @@ finish)
    in
    failwith "unexpected (test2)"
  with UnguardedLoopSeq ->
    print_endline "  exception correctly occured (test2)\n"

let test3 =
  try
    print_endline "test3: the same thing as test2, nested under a recursion";
    let _ =
      gen
        (fix (fun t ->
             choice_at a (to_b left_or_middle)
               ( a,
                 (a --> b) left
                 @@ choice_at a
                      (to_b left_middle_or_right)
                      (a, t)
                      ( a,
                        (* here C blocks indefinitely, and must be rejected *)
                        fix (fun u -> (a --> b) right @@ u) ) )
               (a, (a --> b) middle @@ (b --> c) middle @@ finish)))
    in
    failwith "unexpected (test3)"
  with UnguardedLoopSeq ->
    print_endline "  exception correctly occurred (test3)\n"

(* (!): Future work: We can forcibly merge them by catching the exception in mergeable.ml.
        In that case, C indefinitely blocks but this should be distinguished with the
        actual deadlock (i.e. it is "safe" in some weaker notion of progress property)
*)
