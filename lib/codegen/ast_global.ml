
type kind = Comm | Scatter | Gather | SelectOne | OfferOne
type global =
  | Seq of kind * string * string * string * global
  | Choice of string * global * global
  | Guard of global lazy_t
  | Goto of string
  | Finish

let newvar =
  let r = ref 0 in
  fun () ->
  let i = !r in
  r := !r + 1;
  "g" ^ string_of_int i

let assq_opt x xs =
  try
    Some (List.assq x xs)
  with
    Not_found ->
     None

let make_protocol_env g =
  let rec check_cycle_then_traverse acc (g : global) =
    match assq_opt g acc with
    | Some(touched,varname,_) ->
       touched := true;
       Goto varname, acc
    | None ->
       begin match g with
       | (Goto _ | Finish) -> g, acc
       | _ -> (* make an entry *)
          let result = ref g in
          let touched = ref (if acc=[] then true else false) in
          let acc = (g, (touched, newvar (), result))::acc in
          let g, acc = traverse acc g in
          result := g;
          g, acc
       end
  and traverse acc (g : global) =
    match g with
    | Seq(k,f,t,l,cont) ->
       let g, acc = check_cycle_then_traverse acc cont in
       Seq(k,f,t,l,g), acc
    | Choice(r,cont1,cont2) ->
       let g1, acc = check_cycle_then_traverse acc cont1 in
       let g2, acc = check_cycle_then_traverse acc cont2 in
       Choice(r,g1,g2), acc
    | Guard g ->
       check_cycle_then_traverse acc (Lazy.force g)
    | Goto var ->
       Goto var, acc
    | Finish ->
       Finish, acc
  in
  let _, acc = check_cycle_then_traverse [] g in
  List.fold_left
    (fun xs (_,(touched,varname,result)) ->
      if !touched then (!result,varname)::xs else xs)
    [] acc

let ( --> )  rA rB label p0 =
  Seq(Comm,rA,rB,label,p0)

let choice_at r p0 p1 =
  Choice(r, p0, p1)

let fix f =
  let rec loop = Guard (lazy (f loop)) in
  loop

let finish = Finish

