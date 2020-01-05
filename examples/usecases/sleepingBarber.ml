open Mpst
open Usecase_util

(** Sleeping Barber protocol:
 *  from A. Scalas and N. Yoshida, Lightweight Session Programming in Scala, ECOOP 2016 *)

  let customer = {role_index=Zero; role_label={make_obj=(fun v -> object method role_Customer=v end); call_obj=(fun o->o#role_Customer)}}
  let shop = {role_index=Succ Zero; role_label={make_obj=(fun v -> object method role_Shop=v end); call_obj=(fun o->o#role_Shop)}}
  let barber = {role_index=Succ (Succ Zero); role_label={make_obj=(fun v -> object method role_Barber=v end); call_obj=(fun o->o#role_Barber)}}
  let to_customer m = to_ m customer customer customer
  let to_shop m = to_ m shop shop shop
  let to_barber m = to_ m barber barber barber

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
                       (shop --> barber) (deleg_customer >:prot barber (haircut ()))   @@
                           t))
