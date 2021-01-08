open Concur_shims
open Bench_util.Util
open Bench_util.Testbase
open Linocaml

let global_cnt = ref 0

module MakeDyn
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    open Mpst
    open Mpst.Util

    (* module NoStaticLinearityChecking = struct
     *   type 'a lin = 'a
     *   let mklin x = x
     * end
     * open Mpst.Global_combinators.Make(Mpst.Dyn_lin.NoCheck)(NoStaticLinearityChecking)
     * open Mpst.Shared.Make(Mpst.Dyn_lin.NoCheck)(NoStaticLinearityChecking) *)

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    (**
     * pre-allocated channels
     *)
    let sa_init, sb_init =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (get_ch a g), ref (get_ch b g)


    let setup _ =
      let g = gen_with_kinds [Med.medium;Med.medium;] prot in
      sa_init := get_ch a g;
      sb_init := get_ch b g

    let server_step param =
      let stored = ref !sb_init in
      fun () ->
      let rec loop n sb =
        if n = 0 then begin
            IO.return sb
          end else
          IO.bind (receive sb#role_A)
          @@ fun[@inline] (`ping(_,sb)) ->
             IO.bind (send sb#role_A#pong ())
             @@ fun[@inline] sb ->
                loop (n-1) sb
      in
      IO.bind (loop param !stored)
      @@ fun[@inline] sb ->
         stored := sb;
         IO.return_unit
        

    let client_step param =
      let stored = ref !sa_init in
      let payload = () in
      Core.Staged.stage
      @@ fun () ->
         let rec loop n sa =
           if n = 0 then begin
               IO.return sa
             end else
             IO.bind (send sa#role_B#ping payload)
               @@ fun[@inline] sa ->
                  IO.bind (receive sa#role_B)
                  @@ fun[@inline] (`pong(c,sa)) ->
                     loop (n-1) sa
         in
         IO.bind (loop param !stored)
         @@ fun[@inline] sa ->
            stored := sa;
            IO.return_unit
  end

  include MakeTestBase(Test)(Med)()


end[@@inline]

module MakeStatic
         (Med:MEDIUM)
         ()
       : TEST
  = struct

  module Test = struct
    open Mpst_lin
    open Mpst.Util
    open Linocaml
    open LinocamlStyle

    let prot =
      fix (fun t ->
          (a --> b) ping @@
            (b --> a) pong @@ t)

    let sa_init, sb_init =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      ref (raw_get_ch a g), ref (raw_get_ch b g)

    let (let*) = Linocaml.bind

    let s = Linocaml.Zero

    let setup _ =
      let g = raw_gen_with_kinds [Med.medium;Med.medium;] prot in
      sa_init := raw_get_ch a g;
      sb_init := raw_get_ch b g


    let server_step param =
      let store = ref !sb_init in
      Linocaml.run
        (fun[@inline] () ->
          let rec loop n =
            if n = 0 then
              get_lin s
            else
              Syntax.bind_lin (s <@ receive (fun[@inline] x->x#role_A))
              @@ Syntax.Internal._mkbind
              @@ fun[@inline] {__lin=`ping(_,__tmp)} ->
                 Syntax.bind_lin
                   {__m=(fun[@inline] pre ->
                      let pre = Syntax.lens_put' s __tmp pre in
                      (s <@ send (fun[@inline] x-> x#role_A#pong) ()).__m pre)}
                 @@ Syntax.Internal._mkbind
                 @@ fun[@inline] __tmp ->
                    Syntax.Internal._modify
                      (Syntax.lens_put' s __tmp)
                      (loop (n-1))
                 
          in
          Syntax.Internal._modify (Syntax.lens_put' s {__lin= !store})
          @@ Syntax.bind_lin (loop param)
          @@ Syntax.Internal._mkbind
          @@ fun[@inline] {__lin=__tmp} ->
             {__m=(fun[@inline] pre ->
                store := __tmp;
                IO.return (pre, {Linocaml.data=()})
             )}
        )

    let client_step param =
      let store = ref !sa_init in
      let payload = List.assoc param big_arrays in
      Core.Staged.stage
        (Linocaml.run
           (fun[@inline] () ->
             let rec loop n =
               if n = 0 then
                 get_lin s
               else
                 Syntax.bind_lin
                   (s <@ send (fun[@inline] x->x#role_B#ping) payload)
                 @@ Syntax.Internal._mkbind
                 @@ fun[@inline] __tmp ->
                    Syntax.bind_lin
                      {__m=(fun[@inline] pre ->
                         let pre = Syntax.lens_put' s __tmp pre in
                         (s <@ receive (fun[@inline] x->x#role_B)).__m pre)}
                    @@ Syntax.Internal._mkbind
                    @@ fun[@inline] {__lin=`pong({Linocaml.data=()},__tmp)} ->
                       Syntax.Internal._modify
                         (Syntax.lens_put' s __tmp)
                         (loop (n-1))
             in
             Syntax.Internal._modify (Syntax.lens_put' s {__lin= !store})
             @@ Syntax.bind_lin (loop param)
             @@ Syntax.Internal._mkbind
             @@ fun[@inline] {__lin=__tmp} ->
                {__m=(fun[@inline] pre ->
                   store := __tmp;
                   IO.return (pre, {data=()}))}
           ))

  end

  include MakeTestBase(Test)(Med)()
end[@@inline]
