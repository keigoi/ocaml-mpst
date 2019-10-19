
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

class virtual syntax =
object
    val mutable tab = 0
    method mktab =
      mktab tab
    method set_tab i =
      tab <- i
    method virtual preamble : string list -> string list -> string
    method virtual comm : string -> kind -> string -> string -> string
    method virtual choice : string -> string -> string -> string
    method virtual goto : string -> string
    method virtual finish : string
    method virtual decl : string -> string -> string
end
  
let scribble roles =
  object (self)
    inherit syntax
    method preamble _ _ =
      "module OCamlMPST;\n"
    method comm l k f t =
      let str f t = self#mktab ^ l ^ "() " ^ "from " ^ f ^ " to " ^ t ^ ";\n" in
      match k with
      | Comm -> str f t
      | Scatter -> str f (t ^ "[1,K]")
      | Gather -> str (f ^ "[1,K]") t
      | SelectOne -> str f (t^"[?]")
      | OfferOne -> str (f^"[?]") t
    method choice r cont1 cont2 =
      self#mktab ^ "choice at " ^ r ^ " {\n"
      ^ cont1
      ^ self#mktab ^ "} or {\n"
      ^ cont2
      ^ self#mktab ^ "}\n"
    method goto var =
      self#mktab ^ "do "^ var ^ "(" ^ String.concat ", " roles ^  ");\n"
    method finish =
      ""
    method decl var body =
        "global protocol " ^ var ^ "(" ^ pr_roles roles ^ ") {\n"
        ^ body
        ^ "}\n"
  end

let rec peano i =
  if i = 0 then
    "Zero"
  else
    "Succ (" ^ peano (i-1) ^ ")"
  
let declare_role i r =
  "let " ^ r ^ " =\n" ^
  "  {role_index="^ peano i ^";\n"^
  "   role_label={make_obj=(fun v -> object method role_"^r^"=v end);\n" ^
  "               call_obj=(fun o -> o#role_"^r^")}}\n"
let declare_label l =
  "let " ^ l ^ " =\n" ^
  "  {obj={make_obj=(fun v -> object method role_"^l^"=v end);\n" ^
  "        call_obj=(fun o -> o#role_"^l^")};\n" ^
  "   var=(fun v -> `"^l^"(v))}\n"
let ocamlmpst roles =
  object (self)
    inherit syntax
    method preamble roles labels =
      String.concat "" @@
        List.mapi declare_role roles @ List.map declare_label labels
    method comm l k f t =
      match k with
      | Comm -> self#mktab ^ "(" ^ f ^ "-->" ^ t ^ ") " ^ l ^ " @@\n"
      | Scatter -> self#mktab ^ "(scatter " ^ f ^ " " ^ t ^ ") " ^ l ^ " @@\n"
      | Gather -> self#mktab ^ "(gather " ^ f ^ " " ^ t ^ ") " ^ l ^ " @@\n"
      | _ -> failwith ""
    method choice r cont1 cont2 =
      self#mktab ^ "choice_at " ^ r ^ "\n"
      ^ self#mktab ^ "(" ^ r ^ ",\n"
      ^ cont1
      ^ self#mktab ^ ")\n"
      ^ self#mktab ^ "(" ^ r ^ ",\n"
      ^ cont2
      ^ self#mktab ^ ")\n"
    method goto var =
      self#mktab ^ var ^ "\n"
    method finish =
      self#mktab ^ "finish\n"
    method decl var body =
        "let " ^ var ^ " = fix (fun " ^ var ^ "->\n"
        ^ body
        ^ ")\n"
  end

let pr_global syn env roles g =
  let check g =
    match assq_opt g env with
    | Some(var) -> Goto var
    | None -> g
  in
  let rec loop acc tab roles g =
    syn#set_tab tab;
    match g with
    | Seq(k,f,t,l,cont) ->
       let acc = acc ^ syn#comm l k f t in
       loop acc tab roles (check cont)
    | Choice(r,cont1,cont2) ->
       let str1 = loop "" (tab+1) roles (check cont1) in
       let str2 = loop "" (tab+1) roles (check cont2) in
       acc ^ syn#choice r str1 str2
    | Goto var ->
       acc
       ^ syn#goto var
    | Finish ->
       acc ^ syn#finish
    | Guard _ ->
       failwith "impossible: pr: Guard encountered"
  in
  loop "" 1 roles g

let string_of_global syn g =
  let env = make_protocol_env g in
  let rs = List.fold_left (fun rs (g,_) -> add_roles rs g) [] env in
  let rs = List.rev rs in
  let syn = syn rs in
  let body_all =
  List.fold_left (fun str (g, var) ->
      str ^ syn#decl var (pr_global syn env rs g)
    ) "" env
  in
  syn#preamble rs [] ^ body_all

let print_global syn g =
  print_endline (string_of_global syn g)

let ( --> )  rA rB label p0 =
  Seq(Comm,rA,rB,label,p0)

let choice_at r p0 p1 =
  Choice(r, p0, p1)

let fix f =
  let rec loop = Guard (lazy (f loop)) in
  loop

let finish = Finish

