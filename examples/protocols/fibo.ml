open Mpst
open Mpst.Util
open Usecase_util

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
        (a, (a --> b) fibonacci @@ (b --> a) fibonacci @@ fib)
        (a, (a --> b) stop @@ finish))
