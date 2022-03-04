open GlobalCombinators
module Sessions = Hlist.Make (State)
open Sessions

module type C = sig
  type chan
  type 'a name
  type _ out
  type 'var inp

  val make : unit -> chan
  val new_name : chan -> 'a name
  val select : 'a out -> 'a
  val branch : 'a inp -> 'a

  val out :
    ('a, 'b) Rows.method_ ->
    ('b, 'c out) Rows.method_ ->
    int name ->
    'c State.t ->
    'a State.t

  val inp :
    ('a, 'b inp) Rows.method_ ->
    ('b, 'c) Rows.constr ->
    int name ->
    'c State.t ->
    'a State.t
end

module type S = sig
  type 's out
  type 's inp
  type chan

  val select : 'a out -> 'a
  val branch : 'a inp -> 'a
  val close : unit -> unit

  val ( --> ) :
    ('a, 'b, 'c, 'd, 'e, 'f inp) role ->
    ('g, 'e, 'h, 'c, 'b, 'i) role ->
    ('i, 'a out, 'f, 'g) label ->
    'h global ->
    'd global
end

module Make (Chan : C) = struct
  type 's out = 's Chan.out
  type 's inp = 's Chan.inp

  let select = Chan.select
  let branch = Chan.branch
  let close () = ()

  type chan_table = (string * string, Chan.chan) Hashtbl.t
  type env_entry += Comm of chan_table

  let () =
    GlobalCombinators.register_default_env (fun () -> Comm (Hashtbl.create 42))

  let get_env env =
    List.hd @@ List.filter_map (function Comm t -> Some t | _ -> None) env

  let lookup (t : chan_table) (key : string * string) =
    match Hashtbl.find_opt t key with
    | Some ch -> ch
    | None ->
        let ch = Chan.make () in
        Hashtbl.add t key ch;
        ch

  let ( --> ) ra rb lab g env =
    let g = g env in
    let ch =
      lookup (get_env env) (ra.role_label.method_name, rb.role_label.method_name)
    in
    let key = Chan.new_name ch in
    let b = seq_get rb.role_index g in
    let g = seq_put rb.role_index g (Chan.inp ra.role_label lab.var key b) in
    let a = seq_get ra.role_index g in
    let g = seq_put ra.role_index g (Chan.out rb.role_label lab.obj key a) in
    g
end

module Sync = struct
  include Chvec.Sync
  include Make (Chvec.Sync)
end

module Async = struct
  include Chvec.Async
  include Make (Chvec.Async)
end

include Async
