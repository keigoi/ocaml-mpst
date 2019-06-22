open Base

module Make(EP:S.LIN_EP) = struct
module Mergeable = Mergeable.Make(EP)
module Seq = Seq.Make(EP)

type ('robj,'c,'a,'b,'xs,'ys) role =
  {role_label: ('robj,'c) method_;
   role_index: ('a,'b,'xs,'ys) Seq.lens}

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}

type 'k epkind = EpLocal | EpIPCProcess of 'k list Table.t list

type 'k prop = {multiplicity:int; epkind:'k epkind}

type 'k env = {props: 'k prop Table.t}

let epkind {props;_} l = (Table.get_or_create props l 1).epkind
let multiplicity {props;_} l = (Table.get_or_create props l 1).multiplicity

type ('k, 'g) t = Global of ('k env -> 'g Seq.t)
let unglobal_ = function
    Global f -> f

let fix : type e g. ((e,g) t -> (e,g) t) -> (e,g) t = fun f ->
  Global (fun e ->
      let rec body =
        lazy (unglobal_ (f (Global (fun _ -> SeqRecVars [body]))) e)
      in
      (* A "fail-fast" approach to detect unguarded loops.
       * Seq.partial_force tries to fully evaluate unguarded recursion variables
       * in the body.
       *)
      Seq.partial_force [body] (Lazy.force body))

let finish : 'e. ('e, [`cons of close * 'a] as 'a) t =
  Global (fun env ->
      SeqRepeat(0, (fun i ->
            let num = (Table.get_or_create_ env.props i 1).multiplicity
            in
            Mergeable.make_no_merge (List.init num (fun _ -> Close)))))

let gen_with_param p g = unglobal_ g p

let get_ep : ('x0, 'x1, 'ep, 'x2, 't Seq.t, 'x3) role -> 't Seq.t -> 'ep = fun r g ->
  let ep = Seq.get r.role_index g in
  match Mergeable.out ep with
  | [e] -> e
  | [] -> assert false
  | _ -> failwith "get_ep: there are more than one endpoints. use get_ep_list."

let get_ep_list : ('x0, 'x1, 'ep, 'x2, 't Seq.t, 'x3) role -> 't Seq.t -> 'ep list = fun r g ->
  let ep = Seq.get r.role_index g in
  Mergeable.out ep

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, unit, (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, unit, 'g0_l Seq.t, 'g1 Seq.t) role * ('e,'g0_l) t ->
                (_, _, 'ep_r, unit, 'g0_r Seq.t, 'g1 Seq.t) role * ('e,'g0_r) t ->
                ('e,'g2) t
  = fun r merge (r',Global g0left) (r'',Global g0right) ->
  Global (fun env ->
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
end
