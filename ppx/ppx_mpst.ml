open Parsetree

let rec peano_natural ~loc (i : int) =
  if i = 0 then [%expr Zero] else [%expr Succ [%e peano_natural ~loc (i - 1)]]

let label_expr ~loc (str : string) =
  [%expr
    {
      obj = [%e Ppx_rows.method_expr ~loc str];
      var = [%e Ppx_rows.constr_expr ~loc str];
    }]

let role_expr ~loc (idx : int) (str : string) =
  [%expr
    {
      role_label = [%e Ppx_rows.method_expr ~loc str];
      role_index = [%e peano_natural ~loc idx];
    }]

let let_ ~loc (strloc : string Location.loc) expr =
  let ident = Ast_helper.Pat.var ~loc:strloc.loc strloc in
  [%stri let [%p ident] = [%e expr]]

let declare_labels ~loc strlocs : Parsetree.module_expr =
  Ast_helper.(
    Mod.structure ~loc
      (List.map
         (fun strloc -> let_ ~loc strloc (label_expr ~loc strloc.txt))
         strlocs))

let declare_roles ~loc ?prefix strlocs : Parsetree.module_expr =
  Ast_helper.(
    let rolename strloc =
      match prefix with
      | Some prefix ->
          prefix ^ Stdlib.String.capitalize_ascii strloc.Location.txt
      | None -> strloc.txt
    in
    Mod.structure ~loc
      (List.mapi
         (fun i strloc -> let_ ~loc strloc (role_expr ~loc i (rolename strloc)))
         strlocs))

let payload_ident_tuple =
  let open Ppxlib.Ast_pattern in
  pstr
    (pstr_eval
       (alt
          (pexp_tuple @@ many (pexp_ident (lident __')))
          (map ~f:(fun f x -> f [ x ]) @@ pexp_ident (lident __')))
       nil
    ^:: nil)

let labels =
  let open Ppxlib in
  Extension.declare "declare_labels" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ lablocs ->
      [%stri include [%m declare_labels ~loc lablocs]])

let roles =
  let open Ppxlib in
  Extension.declare "declare_roles" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ lablocs ->
      [%stri include [%m declare_roles ~loc lablocs]])

let roles_prefixed =
  let open Ppxlib in
  Extension.declare "declare_roles_prefixed" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ lablocs ->
      [%stri include [%m declare_roles ~loc ~prefix:"role_" lablocs]])

let () =
  Ppxlib.Driver.register_transformation
    ~extensions:[ labels; roles; roles_prefixed ]
    "ppx_mpst"
