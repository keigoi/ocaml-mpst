open Base

type ('robj,'c,'a,'b,'xs,'ys) role =
  {role_label: ('robj,'c) method_;
   role_index: ('a,'b,'xs,'ys) Seq.lens}

type ('la,'lb,'va,'vb) label =
  {obj: ('la, 'va) method_;
   var: 'vb -> 'lb}

module ConnTable = struct
  type 'k t =
    {mutable table:'k list option array;
     new_channel: unit -> 'k}

  let create f =
    {table=Array.make 0 None;
     new_channel=f}

  let extend t newsize =
    t.table <-
      Array.init newsize (fun i ->
          if i < Array.length t.table then
            t.table.(i)
          else
            None)

  let rec put t lens kts =
    let idx = Seq.int_of_lens lens in
    if idx < Array.length t.table then begin
        t.table.(idx) <- Some kts
      end else begin
        extend t (idx+1);
        put t lens kts
      end

  let rec get t lens =
    let idx = Seq.int_of_lens lens in
    match t.table.(idx) with
    | Some ks -> ks
    | None -> failwith "ConnTable: no entry"

  let rec get_or_create t lens cnt =
    let idx = Seq.int_of_lens lens in
    if idx < Array.length t.table then begin
      match t.table.(idx) with
      | Some ks ->
         ks
      | None ->
         let ks = List.init cnt (fun _ -> t.new_channel ()) in
         t.table.(idx) <- Some ks;
         ks
      end else begin
        extend t (idx+1);
        get_or_create t lens cnt
      end
end

type 'k kind = Local | IPCProcess of 'k ConnTable.t list

type 'k prop = {multiplicity:int; kind:'k kind}
type 'k env = {props: 'k prop list}
type ('k, 'g) t = Seq of ('k env -> 'g Seq.t)
let unseq_ = function
    Seq f -> f

let multiplicity {props} l =
  let i = Seq.int_of_lens l in
  if i < List.length props then
    (List.nth props i).multiplicity
  else
    1

let kind {props} l =
  let i = Seq.int_of_lens l in
  if i < List.length props then
    (List.nth props i).kind
  else
    Local

let fix : type e g. ((e,g) t -> (e,g) t) -> (e,g) t = fun f ->
  Seq (fun e ->
      let rec body =
        lazy (unseq_ (f (Seq (fun _ -> SeqRecVars [body]))) e)
      in
      (* A "fail-fast" approach to detect unguarded loops.
       * Seq.partial_force tries to fully evaluate unguarded recursion variables 
       * in the body.
       *)
      Seq.partial_force [body] (Lazy.force body))

let finish : 'e. ('e, [`cons of close * 'a] as 'a) t =
  Seq (fun env ->
      SeqRepeat(0, (fun i ->
            let num =
              if i < List.length env.props then
                (List.nth env.props i).multiplicity
              else 1
            in
            Mergeable.make_no_merge (List.init num (fun _ -> Close)))))

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

let choice_at : 'ep 'ep_l 'ep_r 'g0_l 'g0_r 'g1 'g2.
                  (_, _, unit, (< .. > as 'ep), 'g1 Seq.t, 'g2 Seq.t) role ->
                ('ep, < .. > as 'ep_l, < .. > as 'ep_r) obj_merge ->
                (_, _, 'ep_l, unit, 'g0_l Seq.t, 'g1 Seq.t) role * ('e,'g0_l) t ->
                (_, _, 'ep_r, unit, 'g0_r Seq.t, 'g1 Seq.t) role * ('e,'g0_r) t ->
                ('e,'g2) t
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
