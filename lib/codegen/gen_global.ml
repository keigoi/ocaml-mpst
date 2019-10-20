open Ast_global
open Epp

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

let add_all xs xss =
  List.fold_right add xs xss

class virtual syntax =
object
  val mutable tab = 0
  val mutable roles = []
  method mktab =
    mktab tab
  method set_tab i =
    tab <- i
  method set_roles (rs:string list) =
    roles <- rs
  method virtual preamble : string list -> string list -> (string list * string list) list -> string
  method virtual comm : string -> kind -> string -> string -> string -> string
  method virtual choice : string -> string -> string -> (string * string list * string list) -> string
  method virtual goto : string -> string
  method virtual finish : string
  method virtual decl : string -> string -> string
end

let pr_global (syn:syntax) env roles g =
  let check g =
    match assq_opt g env with
    | Some(var) -> Goto var
    | None -> g
  in
  let merges = ref [] in
  let add_merge ls rs =
    merges := add (ls,rs) !merges
  in
  let rec loop acc tab roles g =
    syn#set_tab tab;
    match g with
    | Seq(k,f,t,l,cont) ->
       let cont = loop "" tab roles (check cont) in
       acc ^ syn#comm l k f t cont
    | Choice(r,cont1,cont2) ->
       let cl,cr,_ = Epp.gen_choice r cont1 cont2 in
       let dest,br1,br2 = Epp.out_merge_ cl cr in
       let left_labels = List.map fst br1 in
       let right_labels = List.map fst br2 in
       let str1 = loop "" (tab+1) roles (check cont1) in
       let str2 = loop "" (tab+1) roles (check cont2) in
       syn#set_tab tab;
       add_merge left_labels right_labels;
       acc ^ syn#choice r str1 str2 (dest,left_labels,right_labels)
    | Goto var ->
       acc
       ^ syn#goto var
    | Finish ->
       acc ^ syn#finish
    | Guard _ ->
       failwith "impossible: pr: Guard encountered"
  in
  let s = loop "" 1 roles g in
  (s, !merges)

let pr_roles roles =
  String.concat ", " @@ List.map (fun s -> "role "^s) roles
  
class scribble =
  object (self)
    inherit syntax
    method preamble _ _ _ =
      "module OCamlMPST;\n"
    method comm l k f t cont =
      let str f t =
        self#mktab ^ l ^ "() " ^ "from " ^ f ^ " to " ^ t ^ ";\n"
        ^ cont
      in
      match k with
      | Comm -> str f t
      | Scatter -> str f (t ^ "[1,K]")
      | Gather -> str (f ^ "[1,K]") t
      | SelectOne -> str f (t^"[?]")
      | OfferOne -> str (f^"[?]") t
    method choice r cont1 cont2 _ =
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
    "(Succ " ^ peano (i-1) ^ ")"
  
let declare_role i r =
  "let " ^ r ^ " =\n" ^
  "  {role_index="^ peano i ^";\n"^
  "   role_label={make_obj=(fun v -> object method role_"^r^"=v end);\n" ^
  "               call_obj=(fun o -> o#role_"^r^")}}\n" ^
  "let to_" ^ r ^ " m =\n" ^
  "  {disj_merge=(fun l r -> object method role_"^r^"=m.disj_merge l#role_"^r^" r#role_"^r^" end);\n" ^
  "   disj_splitL=(fun lr -> object method role_"^r^"=m.disj_splitL lr#role_"^r^" end);\n" ^
  "   disj_splitR=(fun lr -> object method role_"^r^"=m.disj_splitR lr#role_"^r^" end);\n" ^
  "  }\n"

let declare_label l =
  "let " ^ l ^ " =\n" ^
  "  {obj={make_obj=(fun v -> object method "^l^"=v end);\n" ^
  "        call_obj=(fun o -> o#"^l^")};\n" ^
  "   var=(fun v -> `"^l^"(v))}\n"

  
let declare_merge ls rs =
  let method_ v m = "method "^m^"="^v^"#"^m in
  let methods v ms =
    String.concat " " (List.map (method_ v) ms)
  in
  let methodtype m = m ^ " : _" in
  let methodtypes ms =
    String.concat "; " (List.map methodtype ms)
  in
  let name = String.concat "" ls ^ "_" ^ String.concat "" rs in
"let "^name^" = \n"^
"  {disj_merge=(fun l r -> object " ^ methods "l" ls ^ " " ^ methods "r" rs ^ " end);\n" ^
"   disj_splitL=(fun lr -> (lr :> <"^methodtypes ls^">));\n" ^
"   disj_splitR=(fun lr -> (lr :> <"^methodtypes rs^">));\n" ^
"  }\n"

class ocamlmpst =
  object (self)
    inherit syntax
    method preamble roles labels merges =
      (* "open Mpst;;\n" ^ *)
      String.concat "" @@
        List.mapi declare_role roles
        @ List.map declare_label labels
        @ List.map (fun (l,r) -> declare_merge l r) merges
    method comm l k f t cont =
      match k with
      | Comm ->
         "(" ^ f ^ " --> " ^ t ^ ") " ^ l ^ " @@\n"
         ^ self#mktab ^ cont
      | Scatter ->
         "(scatter " ^ f ^ " " ^ t ^ ") " ^ l ^ " @@\n"
         ^ self#mktab ^ cont
      | Gather ->
         self#mktab ^ "(gather " ^ f ^ " " ^ t ^ ") " ^ l ^ " @@\n"
         ^ self#mktab ^ cont
      | _ -> failwith ""
    method choice r cont1 cont2 (dest,left_lab,right_lab) =
      "choice_at " ^ r ^
        " (to_" ^ dest ^ " " ^
          String.concat "" left_lab ^ "_" ^ String.concat "" right_lab ^ ")"
      ^ "\n" ^ self#mktab ^ "(" ^ r ^ ",\n" ^ self#mktab ^ "  " ^ cont1 ^ ")"
      ^ "\n" ^ self#mktab ^ "(" ^ r ^ ",\n" ^ self#mktab ^ "  " ^ cont2 ^ ")"
    method goto var =
      var
    method finish =
      "finish"
    method decl var body =
        "let " ^ var ^ " = fix (fun " ^ var ^ "->\n"
        ^ self#mktab ^ body
        ^ ")\n"
  end

let rec get_roles = function
  | Seq(_,f,t,_,cont) ->
     add f @@ add t @@ get_roles cont
  | Choice(r,cont1,cont2) ->
     add r (add_all (get_roles cont1) (get_roles cont2))
  | Guard _ -> []
  | Goto _ -> []
  | Finish -> []

let rec get_labels = function
  | Seq(_,_,_,l,cont) ->
     add l @@ get_labels cont
  | Choice(_,cont1,cont2) ->
     add_all (get_labels cont1) (get_labels cont2)
  | Guard _ -> []
  | Goto _ -> []
  | Finish -> []

let string_of_global (syn:syntax) g =
  let env = make_protocol_env g in
  let gs = List.map fst env in
  let roles = List.concat (List.map get_roles gs) in
  let labels = List.concat (List.map get_labels gs) in
  syn#set_roles roles;
  let body_all, merges =
    List.fold_left (fun (str,merges) (g, var) ->
        let body,merge = pr_global syn env roles g in
        (str ^ syn#decl var body, add_all merge merges)
      ) ("",[]) env
  in
  syn#preamble roles labels merges ^ body_all

let print_global syn g =
  print_endline (string_of_global syn g)
