open Codegen

let preamble =
"open Bench_util.Util\n" ^
"open Bench_util.Testbase\n" ^
"open Mpst.M\n" ^
"open Mpst.M.Base\n" ^
"module ML = Mpst_lwt.M\n" ^
"\n" ^
"let () = Random.self_init ()\n" ^
"\n" ^
"let from_some = function\n" ^
"    Some v -> v\n" ^
"  | None -> assert false\n" ^
"\n" ^
"module MakeDyn\n" ^
"         (EP:Mpst.S.ENDPOINTS)\n" ^
"         (M:PERIPHERAL)\n" ^
"         (Med:MEDIUM)\n" ^
"         ()\n" ^
"       : TEST\n" ^
"  = struct\n" ^
"\n" ^
"\n" ^
"  module Test = struct\n" ^
"    type +'a monad = 'a M.t\n" ^
"\n" ^
"    module Local = Local.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)\n" ^
"    module Global = Global.Make(EP)(Mpst.Lin.NoCheck)(M)(M.Event)(M.Serial)\n" ^
"    module Util = Util.Make(EP)\n" ^
"\n" ^
"    open Global\n" ^
"    open Local\n" ^
"    open Util\n" ^
"\n" ^
"    let (let/) = M.bind\n\n"

let rec outcase tab dest (lab,cont) =
  mktab tab ^
  "let/ s = send s#role_" ^ dest ^ "#" ^ lab ^ " () in\n" ^
  gen_body_code tab cont
and makeout tab dest br =
  match br with
  | [] -> assert false
  | [(lab,cont)] ->
       outcase tab dest (lab,cont)
  | _ ->
     mktab tab ^
       "if Random.bool () then begin\n" ^
           String.concat
             ("\n" ^ mktab tab ^ "end else if Random.bool () then begin\n")
             (List.map (outcase (tab+1) dest) br) ^
             (mktab tab ^ "\nend")
and makeinp tab dest br =
  match br with
  | [] -> assert false
  | [(lab,cont)] ->
     mktab tab ^
       "let/ `"^lab^"((),s) = receive s#role_" ^ dest ^ " in\n" ^
         gen_body_code tab cont
  | _ ->
     mktab tab ^
       "let/ lab = receive s#role_" ^ dest ^ " in\n" ^
         mktab tab ^
           "match lab with\n" ^
             String.concat "\n" (List.map (inpcase (tab+1)) br)
and inpcase tab (lab,cont) =
  mktab tab ^
    "| `" ^ lab ^ "((), s) ->\n" ^
      gen_body_code (tab+1) cont
and gen_body_code tab = function
  | Out(dest,br) ->
     makeout tab dest br
  | Inp(dest,br) ->
     makeinp tab dest br
  | Close ->
     mktab tab ^ "close s"
  | Loop var ->
     mktab tab ^ var ^ "s"

let gen_local_code tab (var,body) =
  var ^ " s = \n" ^ gen_body_code (tab+1) body

let assoc_all k ls =
  List.map snd (List.filter (fun (k',_) -> k=k') ls)

let gen_role_code tab r ls =
  let fstvar = fst (List.hd ls) in
  let codes = List.map (gen_local_code (tab+1)) ls in
  let code = 
    "let rec " ^ String.concat "\nand " codes
  in
  "let start_" ^ r ^ " s = \n" ^
    code ^ "\nin " ^ fstvar ^ " s;;"

let gen_code modname g =
  let roles = get_roles g in
  let env = make_protocol_env g in
  let rs = List.map (fun (g, var) -> (var, fst (Epp.genlocals g))) env in
  let rs =
    List.concat @@
    List.map (fun (var, ls) ->
          (List.map (fun (role,l) -> (role,(var,l))) ls)
      ) rs
  in
  let code =
    List.map (fun r ->
        let ls = assoc_all r rs in
        gen_role_code 1 r ls) roles
  in
  "module " ^ modname ^ " = struct\n" ^
    String.concat "\n\n" code ^
  "\nend\n"
  
let gentest modname g =
  let gl = string_of_global (new ocamlmpst) g in
  let ls = gen_code modname g in
  gl ^ "\n" ^ ls

let gentests params gf =
  preamble ^
    String.concat "\n\n" (List.map (fun p -> gentest ("P"^string_of_int p) (gf p)) params) ^
      "\n\nend\nend\n"
      
