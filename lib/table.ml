type 'k t =
  {mutable table:'k option array;
   default: int -> 'k}

let create f =
  {table=Array.make 0 None;
   default=f}

let create_with f xs =
  {table=Array.init (List.length xs) (fun i -> Some(List.nth xs i));
   default=f}

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

let rec get_or_create_ t idx param =
  if idx < Array.length t.table then begin
      match t.table.(idx) with
      | Some ks ->
         ks
      | None ->
         let k = t.default param in
         t.table.(idx) <- Some k;
         k
    end else begin
      extend t (idx+1);
      get_or_create_ t idx param
    end

let get_or_create t lens param =
  let idx = Seq.int_of_lens lens in
  get_or_create_ t idx param

let size t =
  Array.length t.table
