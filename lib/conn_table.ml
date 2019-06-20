type 'k t =
  {mutable table:'k list option array;
   new_channel: unit -> 'k}

let create f =
  {table=Array.make 0 None;
   new_channel=f}

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

let rec get_or_create t lens cnt =
  let idx = Seq.int_of_lens lens in
  if idx < Array.length t.table then begin
      match t.table.(idx) with
      | Some ks ->
         ks
      | None ->
         let ks = List.init cnt (fun _ -> t.new_channel ()) in
         t.table.(idx) <- Some ks;
         ks
    end else begin
      extend t (idx+1);
      get_or_create t lens cnt
    end
