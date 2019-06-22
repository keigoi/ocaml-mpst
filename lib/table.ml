type 'k t =
  {mutable table:'k option array}

let create () =
  {table=Array.make 0 None}

let create_from xs =
  {table=Array.init (List.length xs) (fun i -> Some(List.nth xs i))}

let extend t newsize =
  t.table <-
    Array.init newsize (fun i ->
        if i < Array.length t.table then
          t.table.(i)
        else
          None)

let rec put t idx kts =
  let idx = idx in
  if idx < Array.length t.table then begin
      t.table.(idx) <- Some kts
    end else begin
      extend t (idx+1);
      put t idx kts
    end

let get t idx =
  match t.table.(idx) with
  | Some ks -> ks
  | None -> failwith "ConnTable: no entry"

let get_opt t idx =
  if idx < Array.length t.table then
    t.table.(idx)
  else
    None

let rec get_or_add t idx default =
  if idx < Array.length t.table then begin
      match t.table.(idx) with
      | Some ks ->
         ks
      | None ->
         let k = default () in
         t.table.(idx) <- Some k;
         k
    end else begin
      extend t (idx+1);
      get_or_add t idx default
    end

let size t =
  Array.length t.table
