
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
  "G" ^ string_of_int i

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

let mktab cnt =
  let rec loop acc cnt =
    if cnt = 0 then
      acc
    else
      loop (acc ^ "  ") (cnt-1)
  in
  loop "" cnt

let add x xs =
  if List.mem x xs then xs else x::xs 

let rec add_roles acc = function
  | Seq(_,f,t,_,cont) ->
     let acc = acc |> add f |> add t in
     add_roles acc cont
  | Choice(r,cont1,cont2) ->
     let acc = add r acc in
     let acc = add_roles acc cont1 in
     add_roles acc cont2
  | Guard _ -> acc
  | Goto _ -> acc
  | Finish -> acc
    
let pr_roles roles =
  String.concat ", " @@ List.map (fun s -> "role "^s) roles

let pr_global env roles g =
  let check g =
    match assq_opt g env with
    | Some(var) -> Goto var
    | None -> g
  in
  let comm k f t =
    let str f t = "from " ^ f ^ " to " ^ t in
    match k with
    | Comm -> str f t
    | Scatter -> str f (t ^ "[1,K]")
    | Gather -> str (f ^ "[1,K]") t
    | SelectOne -> str f (t^"[?]")
    | OfferOne -> str (f^"[?]") t
  in
  let rec loop acc tab roles g =
    match g with
    | Seq(k,f,t,l,cont) ->
       let acc = acc ^ mktab tab ^ l ^ "() " ^ comm k f t ^ ";\n" in
       loop acc tab roles (check cont)
    | Choice(r,cont1,cont2) ->
       let str1 = loop "" (tab+1) roles (check cont1) in
       let str2 = loop "" (tab+1) roles (check cont2) in
       acc
       ^ mktab tab ^ "choice at " ^ r ^ " {\n"
       ^ str1
       ^ mktab tab ^ "} or {\n"
       ^ str2
       ^ mktab tab ^ "}\n"
    | Goto var ->
       acc
       ^ mktab tab ^ "do "^ var ^ "(" ^ String.concat ", " roles ^  ");\n"
    | Finish ->
       acc ^ ""
    | Guard _ ->
       failwith "impossible: pr: Guard encountered"
  in
  loop "" 1 roles g

let print_global g =
  let env = make_protocol_env g in
  let rs = List.fold_left (fun rs (g,_) -> add_roles rs g) [] env in
  let rs = List.rev rs in
  print_endline @@ "module OCamlMPST;";
  List.iter (fun (g, var) ->
      print_endline @@
        "global protocol " ^ var ^ "(" ^ pr_roles rs ^ ") {\n"
        ^ pr_global env rs g
        ^ "}\n";
    ) env
  
