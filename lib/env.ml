open Base

type epkind =
    EpLocal
  (* | EpDpipe of Dpipe.dpipe list Table.t list
   * | EpUntyped of (Base.tag * Obj.t) EV.channel list Table.t list *)

type role_metainfo =
  {rm_index: int;
   rm_kind: epkind;
   rm_size: int}

type t = {metainfo: role_metainfo Table.t; default:int -> epkind}

let rm_size {metainfo;_} idx =
  option ~dflt:1 ~f:(fun x -> x.rm_size) (Table.get_opt metainfo idx)

let rm_kind {metainfo;default} idx cnt =
  match Table.get_opt metainfo idx with
  | Some p -> p.rm_kind
  | None ->
     let kind = default cnt in
     let p = {rm_index=idx; rm_size=cnt; rm_kind=kind} in
     Table.put metainfo idx p;
     kind

let make_metainfo ?size env role =
  let rm_index = int_of_idx role.role_index in
  let rm_size = of_option size ~dflt:(rm_size env rm_index) in
  let rm_kind = rm_kind env rm_index rm_size in
  {rm_index; rm_kind; rm_size}
