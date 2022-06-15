open BasicCombinators
module Sessions = Hlist.Make (LinState)
open Sessions

type 's scatter = 's ActionScatter.scatter
type 's gather = 's ActionGather.gather
type ('v, 's) scatter_val = ('v, 's) ActionScatter.scatter_val
type ('v, 's) gather_val = ('v, 's) ActionGather.gather_val
type 's many = 's ActionMany.many

type chan_table = {
  process_count : (string, int) Hashtbl.t;
  table : (string * string, DynChan.chan list) Hashtbl.t;
}

type param += P : ((_, _, _, _, _, _) role * int) -> param

module MulticastEnv : EnvSpec with type entry = chan_table = struct
  type entry = chan_table
  type env_entry += E of entry

  let name = "multicast"

  let make_default () =
    { process_count = Hashtbl.create 42; table = Hashtbl.create 42 }

  let update param t =
    match param with
    | P (role, cnt) ->
        Hashtbl.replace t.process_count role.role_label.method_name cnt
    | _ -> ()
end

module Lookup = RegisterEnvSpec (MulticastEnv)

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

(* scatter *)
let ( -->@@ ) ra rb lab g env =
  let g = g env in
  let env = Lookup.lookup env in
  let to_count = get_process_count env rb.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g
      (ActionMany.make_branches ~count:to_count ra.role_label lab.var keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionScatter.make_scatter rb.role_label lab.obj keys a)
  in
  g

(** gather *)
let ( @@--> ) ra rb lab g env =
  let g = g env in
  let env = Lookup.lookup env in
  let from_count = get_process_count env ra.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g
      (ActionGather.make_gather ra.role_label lab.var keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionMany.make_selects ~count:from_count rb.role_label lab.obj keys a)
  in
  g

(* scatter (value) *)
let ( ==>@@ ) ra rb g env =
  let g = g env in
  let env = Lookup.lookup env in
  let to_count = get_process_count env rb.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g
      (ActionMany.make_inps ~count:to_count ra.role_label keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionScatter.make_scatter_val rb.role_label keys a)
  in
  g

(** gather (val) *)
let ( @@==> ) ra rb g env =
  let g = g env in
  let env = Lookup.lookup env in
  let from_count = get_process_count env ra.role_label.method_name in
  let chs =
    get_chan env (ra.role_label.method_name, rb.role_label.method_name)
  in
  let keys = List.map DynChan.new_name chs in
  let b = seq_get rb.role_index g in
  let g =
    seq_put rb.role_index g (ActionGather.make_gather_val ra.role_label keys b)
  in
  let a = seq_get ra.role_index g in
  let g =
    seq_put ra.role_index g
      (ActionMany.make_outs ~count:from_count rb.role_label keys a)
  in
  g

let many_at role g env =
  let g = g env in
  let env = Lookup.lookup env in
  let count = get_process_count env role.role_label.method_name in
  seq_put role.role_index g (ActionMany.units ~count)

let get_many = ActionMany.get_many
let scatter = ActionScatter.scatter
let gather = ActionGather.gather
let scatter_val = ActionScatter.scatter_val
let gather_val = ActionGather.gather_val
