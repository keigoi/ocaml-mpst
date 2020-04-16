open Base

type epkind =
    EpLocal
  | EpDpipe of Dpipe.t list Table.t list
  | EpUntyped of (Untyped.tag * Obj.t) Untyped_stream.t list Table.t list

type role_metainfo =
  {rm_index: int;
   rm_kind: epkind;
   rm_size: int}

type t =
  {metainfo: role_metainfo Table.t;
   default:int -> epkind}

let metainfo env idx cnt =
  let kind = env.default cnt in
  let info = {rm_index=idx; rm_size=cnt; rm_kind=kind} in
  Table.put env.metainfo idx info;
  info
  

let rm_size {metainfo;_} idx =
  option ~dflt:1 ~f:(fun x -> x.rm_size) (Table.get_opt metainfo idx)

let rm_kind env idx cnt =
  match Table.get_opt env.metainfo idx with
  | Some p -> p.rm_kind
  | None ->
     let info = metainfo env idx cnt in
     info.rm_kind

     
let make_metainfo ?size env role =
  let rm_index = int_of_idx role.role_index in
  let rm_size = of_option size ~dflt:(rm_size env rm_index) in
  let rm_kind = rm_kind env rm_index rm_size in
  {rm_index; rm_kind; rm_size}


let get_or_add_channels
      ~newch
      src_tables
      _src_role
      other_role
      other_count =
  let make_func () =
    List.init other_count (fun _ -> newch ())
  in
  List.map
    (fun t -> Table.get_or_add t other_role make_func)
    src_tables

let update_tables tables dest chss =
  List.iter2
    (fun t ch -> Table.put t dest ch)
    tables
    chss

let rec transpose : 'a list list -> 'a list list = fun xss ->
  match xss with
  | [] -> []
  | []::_ -> []
  | xss ->
     let hds, tls =
       List.map (fun xs -> List.hd xs, List.tl xs) xss |> List.split
     in
     hds :: transpose tls

let flip_all flip chss =
  List.map (List.map flip) @@ transpose chss

let generate_channels
      ~newch ~flipch ~tablefun ~from_info ~to_info =
  match tablefun from_info.rm_kind, tablefun to_info.rm_kind with
  | Some from_table, to_table_opt ->
     let chss =
       get_or_add_channels
         ~newch
         from_table
         from_info.rm_index
         to_info.rm_index
         to_info.rm_size
     in
     begin match to_table_opt with
     | Some to_table ->
        update_tables
          to_table
          from_info.rm_index
          (flip_all flipch chss)
     | None -> ()
     end;
     chss
  | None, Some table ->
     let chss =
       get_or_add_channels
         ~newch
         table
         to_info.rm_index
         from_info.rm_index
         from_info.rm_size
     in
     flip_all flipch chss
  | _ -> assert false
    
