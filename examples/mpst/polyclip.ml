
(* open Mpst_simple
 * 
 * let p = {lens=Zero;
 *          label={make_obj=(fun v->object method role_P=v end);
 *                 call_obj=(fun o->o#role_P)}}
 * let r = {lens=Succ Zero;
 *          label={make_obj=(fun v->object method role_R=v end);
 *                 call_obj=(fun o->o#role_R)}}
 * let c = {lens=Succ (Succ Zero);
 *          label={make_obj=(fun v->object method role_C=v end);
 *                 call_obj=(fun o->o#role_C)}}
 * 
 * let plane = {obj={make_obj=(fun v-> object method plane=v end);
 *                   call_obj=(fun o->o#plane)};
 *              var=(fun v -> `plane(v))}
 * let is_above = {obj={make_obj=(fun v-> object method is_above=v end);
 *                      call_obj=(fun o->o#is_above)};
 *                 var=(fun v -> `is_above(v))}
 * let both_in = {obj={make_obj=(fun v-> object method both_in=v end);
 *                     call_obj=(fun o->o#both_in)};
 *                var=(fun v -> `both_in(v))}
 * let both_out = {obj={make_obj=(fun v-> object method both_out=v end);
 *                      call_obj=(fun o->o#both_out)};
 *                 var=(fun v -> `both_out(v))}
 * let intersect = {obj={make_obj=(fun v-> object method intersect=v end);
 *                       call_obj=(fun o->o#intersect)};
 *                  var=(fun v -> `intersect(v))}
 * let res = {obj={make_obj=(fun v-> object method res=v end);
 *                 call_obj=(fun o->o#res)};
 *            var=(fun v -> `res(v))}
 * let secout = {obj={make_obj=(fun v-> object method secout=v end);
 *                    call_obj=(fun o->o#secout)};
 *               var=(fun v -> `secout(v))}
 * let secin = {obj={make_obj=(fun v-> object method secin=v end);
 *                   call_obj=(fun o->o#secin)};
 *              var=(fun v -> `secin(v))}
 * let close_ = {obj={make_obj=(fun v-> object method close=v end);
 *                    call_obj=(fun o->o#close)};
 *               var=(fun v -> `close(v))}
 * 
 * let isabove_or_close =
 *   {obj_merge=(fun l r -> object method is_above=l#is_above method close=r#close end);
 *    obj_splitL=(fun lr -> (lr :> <is_above : _>));
 *    obj_splitR=(fun lr -> (lr :> <close : _>));
 *   }
 * let bothin_or_bothout =
 *   {obj_merge=(fun l r -> object method both_in=l#both_in method both_out=r#both_out end);
 *    obj_splitL=(fun lr -> (lr :> <both_in : _>));
 *    obj_splitR=(fun lr -> (lr :> <both_out : _>));
 *   }
 * let secout_or_secin =
 *   {obj_merge=(fun l r -> object method secout=l#secout method secin=r#secin end);
 *    obj_splitL=(fun lr -> (lr :> <secout : _>));
 *    obj_splitR=(fun lr -> (lr :> <secin : _>));
 *   }
 * let bothin_bothout_or_intersect =
 *   {obj_merge=(fun l r -> object method both_in=l#both_in method both_out=l#both_out method intersect=r#intersect end);
 *    obj_splitL=(fun lr -> (lr :> <both_in : _; both_out : _>));
 *    obj_splitR=(fun lr -> (lr :> <intersect : _>));
 *   }
 * let to_r m =
 *   {obj_merge=(fun l r -> object method role_R=m.obj_merge l#role_R r#role_R end);
 *    obj_splitL=(fun lr -> object method role_R=m.obj_splitL lr#role_R end);
 *    obj_splitR=(fun lr -> object method role_R=m.obj_splitR lr#role_R end);
 *   }
 * let to_c m =
 *   {obj_merge=(fun l r -> object method role_C=m.obj_merge l#role_C r#role_C end);
 *    obj_splitL=(fun lr -> object method role_C=m.obj_splitL lr#role_C end);
 *    obj_splitR=(fun lr -> object method role_C=m.obj_splitR lr#role_C end);
 *   }
 * 
 * type op = Add | Sub | Mul | Div
 * let polyclip () =
 *   let rec loop =
 *     lazy begin
 *       choice_at p (to_r isabove_or_close)
 *       (p, (p --> r) is_above @@ (r --> p) res @@
 *           (p --> r) is_above @@ (r --> p) res @@
 *           choice_at p (to_c bothin_bothout_or_intersect)
 *             (p, choice_at p (to_c bothin_or_bothout)
 *                 (p, (p --> c) both_in @@
 *                     (c --> r) both_in @@ goto loop)
 *                 (p, (p --> c) both_out @@
 *                     (c --> r) both_out @@ goto loop))
 *             (p, (p --> c) intersect @@
 *                 (c --> r) intersect @@
 *                 (r --> p) res @@
 *                 choice_at p (to_c secout_or_secin)
 *                 (p, (p --> c) secout @@
 *                     goto loop)
 *                 (p, (p --> c) secin @@
 *                     goto loop)))
 *       (p, (p --> r) close_ @@
 *           (p --> c) close_ @@
 *           finish)
 *   end
 *   in
 *   (\* (p --> r) plane @@ *\)
 *   Lazy.force loop
 *   
 * let tPro (\* ep *\) rect planes =
 *   let g = polyclip () in
 *   let ep = get_ch p g in
 *   (\* let ep = send (ep#role_R#plane) rect in *\)
 *   let rec loop (\* ep *\) points =
 *     match points with
 *     | (v1,v2) :: ps ->
 *        let ep = send (ep#role_R#is_above) v1 in
 *        let `res(b1,ep) = Event.sync (ep#role_R) in
 *        let ep = send (ep#role_R#is_above) v2 in
 *        let `res(b2,ep) = Event.sync (ep#role_R) in
 *        if b1 && b2 then
 *          let ep = send (ep#role_C#both_in) v2 in
 *          let ep = send (ep#role_R#both_in) () in
 *          loop ps
 *          (\* loop ep ps *\)
 *        else if not (b1 || b2) then
 *          let ep = send (ep#role_C#both_out) () in
 *          let ep = send (ep#role_R#both_out) () in
 *          loop ps
 *          (\* loop ep ps *\)
 *        else
 *          let ep = send (ep#role_R#intersect) (v1, v2) in
 *          let `res(ep,i) = Event.sync ep#res in
 *          if not b2 then
 *            let ep = send (ep#role_P#secout) i in
 *            loop ps
 *            (\* loop ep ps *\)
 *          else
 *            let ep = send (ep#role_P#secin) (i, v2) in
 *            loop ps
 *            (\* loop ep ps *\)
 *     | [] ->
 *        let ep = send (ep#role_R#close) () in
 *        let ep = send (ep#role_C#close) () in
 *        close ep
 *   in
 *   loop planes
 *   (\* loop ep planes *\)
 * 
 * (\* let () =
 *  *   let g = polyclip () in
 *  *   let ep = get_ch p g
 *  *   (\\* and er = get_ch r g
 *  *    * and ec = get_ch c g *\\)
 *  *   in
 *  *   (\\* ignore @@ Thread.create tSrv2 es; *\\)
 *  *   tPro ep (0,0,0,0) [(0,0)] *\) *)
