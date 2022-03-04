open BasicCombinators
module Sessions = Hlist.Make (State)
open Sessions

type 's out = 's ActionOut.out
type 's inp = 's ActionInp.inp
type chan = DynChan.chan

let close () = ()

type chan_table = (string * string, DynChan.chan) Hashtbl.t
type env_entry += Comm of chan_table

let () =
  BasicCombinators.register_default_env (fun () -> Comm (Hashtbl.create 42))

let get_env env =
  List.hd @@ List.filter_map (function Comm t -> Some t | _ -> None) env

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
    lookup (get_env env) (ra.role_label.method_name, rb.role_label.method_name)
  in
  let key = DynChan.new_name ch in
  let b = seq_get rb.role_index g in
  let g = seq_put rb.role_index g (ActionInp.inp ra.role_label lab.var key b) in
  let a = seq_get ra.role_index g in
  let g = seq_put ra.role_index g (ActionOut.out rb.role_label lab.obj key a) in
  g

let select = ActionOut.select
let branch = ActionInp.branch
