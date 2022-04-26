open BasicCombinators
module Sessions = Hlist.Make (State)
open Sessions

type 's scatter = 's ActionScatter.scatter
type 's gather = 's ActionGather.gather
type 's many = 's ActionMany.many
type chan = DynChan.chan

let close () = ()

type chan_table = {
  process_count : (string, int) Hashtbl.t;
  table : (string * string, DynChan.chan list) Hashtbl.t;
}

type env_entry += Broadcast of chan_table

let () =
  BasicCombinators.register_default_env (fun () ->
      Broadcast { process_count = Hashtbl.create 42; table = Hashtbl.create 42 })

let get_env env =
  List.hd @@ List.filter_map (function Broadcast t -> Some t | _ -> None) env

let get_process_count (t : chan_table) role =
  Option.value ~default:1 (Hashtbl.find_opt t.process_count role)

let get_chan (t : chan_table) (key : string * string) =
  match Hashtbl.find_opt t.table key with
  | Some ch -> ch
  | None ->
      let from, to_ = key in
      let from_count = get_process_count t from in
      let to_count = get_process_count t to_ in
      let ch = List.init (from_count * to_count) (fun _i -> DynChan.make ()) in
      Hashtbl.add t.table key ch;
      ch

let (--<<) ra rb lab g env =
  let g = g env in
  let env = get_env env in
  let from_count = get_process_count env ra.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g
      (ActionMany.inps ~count:from_count ra.role_label lab.var keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionScatter.make_scatter rb.role_label lab.obj keys a)
  in
  g

let (-->>) ra rb lab g env =
  let g = g env in
  let env = get_env env in
  let to_count = get_process_count env ra.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g (ActionGather.make_gather ra.role_label lab.var keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionMany.outs ~count:to_count rb.role_label lab.obj keys a)
  in
  g

let get_many = ActionMany.get_many

let scatter = ActionScatter.scatter
let gather = ActionGather.gather
