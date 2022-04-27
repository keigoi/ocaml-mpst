open BasicCombinators
module Sessions = Hlist.Make (State)
open Sessions

type 's out = 's ActionOut.out
type 's inp = 's ActionInp.inp
type chan = DynChan.chan

let close () = ()

type chan_table = (string * string, DynChan.chan) Hashtbl.t

module UnicastEnv : EnvSpec with type entry = chan_table = struct
  type entry = chan_table
  type env_entry += E of entry

  let name = "unicast"
  let make_default () = Hashtbl.create 42
  let update _param _tbl = ()
end

module Lookup = RegisterEnvSpec (UnicastEnv)

let lookup (t : chan_table) (key : string * string) =
  match Hashtbl.find_opt t key with
  | Some ch -> ch
  | None ->
      let ch = DynChan.make () in
      Hashtbl.add t key ch;
      ch

let ( --> ) ra rb lab g env =
  let g = g env in
  let ch =
    lookup (Lookup.lookup env)
      (ra.role_label.method_name, rb.role_label.method_name)
  in
  let key = DynChan.new_name ch in
  let b = seq_get rb.role_index g in
  let g = seq_put rb.role_index g (ActionInp.make_inp ra.role_label lab.var key b) in
  let a = seq_get ra.role_index g in
  let g = seq_put ra.role_index g (ActionOut.make_out rb.role_label lab.obj key a) in
  g

let select = ActionOut.select
let branch = ActionInp.branch
