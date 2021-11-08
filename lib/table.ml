type 'k t = { mutable table : 'k option array }

let create () = { table = Array.make 0 None }

let create_from xs =
  { table = Array.init (List.length xs) (fun i -> Some (List.nth xs i)) }

let extend t newsize =
  t.table <-
    Array.init newsize (fun i ->
        if i < Array.length t.table then t.table.(i) else None)

let rec put t idx kts =
  let idx = idx in
  if idx < Array.length t.table then t.table.(idx) <- Some kts
  else (
    extend t (idx + 1);
    put t idx kts)

let get t idx =
  match t.table.(idx) with
  | Some ks -> ks
  | None -> failwith "ConnTable: no entry"

let get_opt t idx = if idx < Array.length t.table then t.table.(idx) else None

let rec get_or_add t idx default =
  if idx < Array.length t.table then (
    match t.table.(idx) with
    | Some ks -> ks
    | None ->
        let k = default () in
        t.table.(idx) <- Some k;
        k)
  else (
    extend t (idx + 1);
    get_or_add t idx default)

let size t = Array.length t.table

let to_list t =
  List.fold_left
    (fun xs -> function Some x -> x :: xs | None -> xs)
    [] (Array.to_list t.table)
