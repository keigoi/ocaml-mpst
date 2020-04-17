open Mpst
open Usecase_util

(** Sleeping Barber protocol:
 *  from A. Scalas and N. Yoshida, Lightweight Session Programming in Scala, ECOOP 2016 *)

  let haircut () =
    (customer --> barber) descr @@
      (barber --> customer) haircut @@
        (customer --> barber) pay @@
          finish

  let g () =
    fix (fun t ->
        choice_at shop (to_customer full_or_seat)
          (shop, (shop --> customer) full @@
                       t)
          (shop, (shop --> customer) seat @@
                   (barber --> shop) available @@
                     (shop --> customer) ready @@
                       (shop --> barber) (deleg_customer >: get_ty barber (haircut ()))   @@
                           t))
