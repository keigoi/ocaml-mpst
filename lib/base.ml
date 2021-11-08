open Types

let compose_method m1 m2 =
  {
    make_obj = (fun v -> m1.make_obj (m2.make_obj v));
    call_obj = (fun obj -> m2.call_obj @@ m1.call_obj obj);
  }

let rec find_physeq : 'a. 'a list -> 'a -> bool =
 fun xs y ->
  match xs with
  | x :: xs -> if x == y then true else find_physeq xs y
  | [] -> false

let rec int_of_idx : type a b c d. (a, b, c, d) idx -> int = function
  | Zero -> 0
  | Succ l -> int_of_idx l + 1
