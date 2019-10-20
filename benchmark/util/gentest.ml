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
(* "       : TEST\n" ^ *)
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

let preamble_static =
"module MakeStatic\n" ^
"         (M:PERIPHERAL_LIN)\n" ^
"         (Med:MEDIUM)\n" ^
"         ()\n" ^
(* "       : TEST\n" ^ *)
"  = struct\n" ^
"  module Test = struct\n" ^
"    module Local = Mpst_monad.Local_monad.Make(M)(M.Event)(M.Linocaml)\n" ^
"    module Global = Mpst_monad.Global_monad.Make(M)(M.Event)(M.Serial)(M.Linocaml)\n" ^
"    module Util = Util.Make(Mpst_monad.Linocaml_lin.EP)\n" ^
"    open Global\n" ^
"    open Local\n" ^
"    open Util\n" ^
"    let (let/) = M.Linocaml.(>>=)\n" ^
"    let s = Linocaml.Zero\n" ^
"     open M.Linocaml\n"

let beforeloop =
  "let/ () = put_linval s (from_some !sb_stored) in\n"
let afterloop =    
"{__m=(fun[@inline] pre ->\n" ^
"  sb_stored := Some ((Linocaml.lens_get s pre).__lin);\n" ^
"  M.return (Linocaml.lens_put s pre (), {Linocaml.data=()})\n" ^
")}\n"

module Dynamic = struct  
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
       mktab tab ^ var ^ " s"
end

module Static = struct
  let rec outcase tab dest (lab,cont) =
    mktab tab ^
      "let%lin #s = s <@ send (fun x-> x#role_" ^ dest ^ "#" ^ lab ^ ") () in\n" ^
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
         "let%lin `"^lab^"((),#s) = s <@ receive (fun x -> x#role_" ^ dest ^ ") in\n" ^
           gen_body_code tab cont
    | _ ->
       mktab tab ^
         "match%lin s <@ receive (fun x -> x#role_" ^ dest ^ ") with\n" ^
           mktab tab ^
               String.concat "\n" (List.map (inpcase (tab+1)) br)
  and inpcase tab (lab,cont) =
    mktab tab ^
      "| `" ^ lab ^ "((), #s) ->\n" ^
        gen_body_code (tab+1) cont
  and gen_body_code tab = function
    | Out(dest,br) ->
       makeout tab dest br
    | Inp(dest,br) ->
       makeinp tab dest br
    | Close ->
       mktab tab ^ "s <@ close"
    | Loop var ->
       mktab tab ^ var ^ "()"
end

let gen_local_code tab is_static (var,body) =
  if is_static then
    var ^ " () = \n" ^ Static.gen_body_code (tab+1) body
  else
    var ^ " s = \n" ^ Dynamic.gen_body_code (tab+1) body

let assoc_all k ls =
  List.map snd (List.filter (fun (k',_) -> k=k') ls)

let gen_role_code tab is_static r ls =
  let fstvar = fst (List.hd ls) in
  let codes = List.map (gen_local_code (tab+1) is_static) ls in
  let code = 
    "let rec " ^ String.concat "\nand " codes
  in
  let arg = if is_static then "()" else "s" in
  "let start_" ^ r ^ " "^arg^" = \n" ^
    code ^ "\nin " ^ fstvar ^ " "^arg^";;"


let gen_code is_static modname g =
  let env = make_protocol_env g in
  let roles = get_roles g in
  let env =
    List.map (fun (g, var) ->
        (var, List.map (fun r -> (r, Epp.get r (Epp.genlocals g))) roles)
      ) env
  in
  let env =
    List.concat @@
      List.map (fun (var, ls) ->
          (List.map (fun (role,l) -> (role,(var, l))) ls)
      ) env
  in
  let code =
    List.map (fun r ->
        let ls = assoc_all r env in
        gen_role_code 1 is_static r ls) roles
  in
  "module " ^ modname ^ " = struct\n" ^
    String.concat "\n\n" code ^
  "\nend\n"
  
let gentest modname g =
  let gl = string_of_global (new ocamlmpst) g in
  let lsdyn = gen_code false modname g in
  let lsstatic = gen_code true modname g in
  (gl, (lsdyn, lsstatic))

let gentests params gf =
  let codes =
    List.map (fun p -> gentest ("P"^string_of_int p) (gf p)) params
  in
  let gs, ls = List.split codes in
  let lsdyn, lsstatic = List.split ls in
  let cat = String.concat "\n" in
  preamble ^
    cat gs ^ "\n" ^ cat lsdyn ^
      "\n\nend\nend\n" ^
      preamble_static ^
        cat gs ^ "\n" ^ cat lsstatic ^
      "\n\nend\nend"
