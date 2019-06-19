open Base

type ('robj,'c,'a,'b,'xs,'ys) role =
  {role_label: ('robj,'c) method_;
   role_index: ('a,'b,'xs,'ys) Seq.lens}

(* type epenv = Local | Remote of ConnTable.t *)
type env = int list
type 'g t = Seq of (env -> 'g Seq.t)
let unseq_ = function
    Seq f -> f

let fix : type g. (g t -> g t) -> g t = fun f ->
  Seq (fun e ->
      let rec body =
        lazy (unseq_ (f (Seq (fun _ -> SeqRecVars [body]))) e)
      in
      (* A "fail-fast" approach to detect unguarded loops.
       * Seq.partial_force tries to fully evaluate unguarded recursion variables 
       * in the body.
       *)
      Seq.partial_force [body] (Lazy.force body))

let finish : ([`cons of close * 'a] as 'a) t =
  Seq (fun env ->
      SeqRepeat(0, (fun i ->
            let num =
              if i < List.length env then
                List.nth env i
              else 1
            in
            Mergeable.make_no_merge (List.init num (fun _ -> Close)))))

let gen g = unseq_ g []
let gen_with_param p g = unseq_ g p

let get_ep : ('x0, 'x1, 'ep, 'x2, 't Seq.t, 'x3) role -> 't Seq.t -> 'ep = fun r g ->
  let ep = Seq.get r.role_index g in
  match Mergeable.out ep with
  | [e] -> e
  | [] -> assert false
  | _ -> failwith "get_ep: there are more than one endpoints. use get_ep_list."

let get_ep_list : ('x0, 'x1, 'ep, 'x2, 't Seq.t, 'x3) role -> 't Seq.t -> 'ep list = fun r g ->
  let ep = Seq.get r.role_index g in
  Mergeable.out ep

module type LIN = sig
  type 'a lin
  val mklin : 'a -> 'a lin
  val unlin : 'a lin -> 'a
end

module type EVENT = sig
  type 'a event
  val guard : (unit -> 'a event) -> 'a event
  val choose : 'a event list -> 'a event
  val wrap : 'a event -> ('a -> 'b) -> 'b event
end
(* module LwtEvent = struct
 *   type 'a event = 'a Lwt.t
 *   let guard f = f () (\*XXX correct??*\)
 *   let choose = Lwt.choose
 *   let wrap e f = Lwt.map f e
 * end *)

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}


let rec toint : type a b c d. (a,b,c,d) Seq.lens -> int = function
  | Zero -> 0
  | Succ l -> toint l + 1

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, unit, (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, unit, 'g0_l Seq.t, 'g1 Seq.t) role * 'g0_l t ->
                (_, _, 'ep_r, unit, 'g0_r Seq.t, 'g1 Seq.t) role * 'g0_r t ->
                'g2 t
  = fun r merge (r',Seq g0left) (r'',Seq g0right) ->
  Seq (fun env ->
      let g0left, g0right = g0left env, g0right env in
      let epL, epR =
        Seq.get r'.role_index g0left,
        Seq.get r''.role_index g0right in
      let g1left, g1right =
        Seq.put r'.role_index g0left (Mergeable.make_no_merge [()]),
        Seq.put r''.role_index g0right (Mergeable.make_no_merge [()]) in
      let g1 = Seq.seq_merge g1left g1right in
      let ep = Mergeable.disjoint_merge merge epL epR
      in
      let g2 = Seq.put r.role_index g1 ep
      in
      g2)

module MakeGlobal(X:LIN) = struct

  let receive_one = function
    | [ch] -> Event.receive ch
    | _ -> assert false

  (* XXX a dumb implementation of receiving from multiple channels  *)
  let receive_list = function
    | ch::chs ->
       Event.wrap
         (Event.receive ch)
         (fun v ->
           v :: List.map (fun ch -> Event.sync (Event.receive ch)) chs)
    | [] -> failwith "no channel"

  let make_recv ~receive num rA lab (phss: _ Event.channel list ref list) epB =
    if num<=0 then begin
        failwith "make_recv: scatter/gather error: number of senders is <= 0"
      end;
    if List.length phss = 0 then begin
        failwith "make_recv: scatter/gather error: number of receivers is <= 0"
      end;
    assert (List.length !(List.hd phss) = num);
    let ev =
      List.init num
        (fun k -> fun once ->
                  Event.wrap
                    (Event.guard (fun () ->
                         LinFlag.use once;
                         let chs = List.map (fun phs -> List.nth !phs k) phss in
                         receive chs))
                    (fun v -> lab.var (v, X.mklin (List.nth (Mergeable.out epB) k))))
    in
    let hook =
      lazy begin
          let eps = Mergeable.out epB in
          if num <> List.length eps then
            failwith "make_recv: endpoint count inconsistency; use unseq_param for scatter/gather"
        end
    in
    Mergeable.wrap_obj rA.role_label
      (Mergeable.make_with_hook
         hook
         Local.merge_in
         ev)


  let send_one (a,b,(c,d)) = Local.Out (a,b,(c,d))
  let send_many (a,b,(c,d)) = Local.OutMany (a,b,(c,d))

  let make_send ~send num rB lab (phss: _ Event.channel list ref list) epA =
    if num<=0 then begin
        failwith "make_send: scatter/gather error: number of receivers is <= 0"
      end;
    if List.length phss = 0 then begin
        failwith "make_send: scatter/gather error: number of senders is <= 0"
      end;
    assert (List.length phss = num);
    let epA' =
      List.init num
        (fun k -> fun once ->
                  X.mklin (send (once,List.nth phss k,(k,epA))))
    in
    let hook =
      lazy begin
          let eps = Mergeable.out epA in
          if num <> List.length eps then
            failwith "make_send: endpoint count inconsistency; use unseq_param for scatter/gather"
        end
    in
    Mergeable.wrap_obj rB.role_label
      (Mergeable.wrap_obj lab.obj
         (Mergeable.make_with_hook
            hook
            (fun o1 o2 -> X.mklin (Local.merge_out (X.unlin o1) (X.unlin o2)))
            epA'))

  let a2b anum bnum ~send ~receive = fun rA rB label g0 ->
    let ch =
      List.init anum (fun _ ->
          ref @@ List.init bnum (fun _ -> Event.new_channel ()))
    in
    let epB = Seq.get rB.role_index g0 in
    let ev  = make_recv ~receive bnum rA label ch epB in
    let g1  = Seq.put rB.role_index g0 ev
    in
    let epA = Seq.get rA.role_index g1 in
    let obj = make_send ~send anum rB label ch epA in
    let g2  = Seq.put rA.role_index g1 obj
    in g2

  let ( --> ) : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Local.out X.lin, 'v * 'epB X.lin) label ->
                'g0 t -> 'g2 t
    = fun rA rB label (Seq g0) ->
    Seq (fun env -> a2b 1 1 ~send:send_one ~receive:receive_one rA rB label (g0 env))

  let scatter : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
                (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
                (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
                (< .. > as 'labelobj, [> ] as 'labelvar, ('v list * 'epA) Local.out X.lin, 'v * 'epB X.lin) label ->
                'g0 t -> 'g2 t
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env <= toint rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        let bnum = List.nth env (toint rB.role_index) in
        a2b 1 bnum ~send:send_many ~receive:receive_one rA rB label (g0 env))

  let gather : 'roleAobj 'labelvar 'epA 'roleBobj 'g1 'g2 'labelobj 'epB 'g0 'v.
               (< .. > as 'roleAobj, 'labelvar Local.inp, 'epA, 'roleBobj, 'g1 Seq.t, 'g2 Seq.t) role ->
               (< .. > as 'roleBobj, 'labelobj,     'epB, 'roleAobj, 'g0 Seq.t, 'g1 Seq.t) role ->
               (< .. > as 'labelobj, [> ] as 'labelvar, ('v one * 'epA) Local.out X.lin, 'v list * 'epB X.lin) label ->
    'g0 t -> 'g2 t
    = fun rA rB label (Seq g0) ->
    Seq (fun env ->
        if List.length env <= toint rB.role_index then begin
            failwith "use unseq_param [...] for scatter/gather"
          end;
        let anum = List.nth env (toint rA.role_index) in
        a2b anum 1 ~send:send_one ~receive:receive_list rA rB label (g0 env))
end

include MakeGlobal(struct type 'a lin = 'a let mklin x = x let unlin x = x end)
