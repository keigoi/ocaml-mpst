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

let disj_expr ~loc (left_methods : string Location.loc list)
    (right_methods : string Location.loc list) : expression =
  let open Ast_helper in
  let method_ origin (name : string Location.loc) =
    let name = Location.mknoloc name.txt in
    Cf.method_ ~loc name Public @@ Cf.concrete Fresh @@ Exp.send origin name
  in
  let concat_body (l, ls) (r, rs) =
    Exp.object_ ~loc
    @@ Cstr.mk (Pat.any ()) (List.map (method_ l) ls @ List.map (method_ r) rs)
  in
  let method_type (name : string Location.loc) = Of.tag name (Typ.any ()) in
  let split_type names = Typ.object_ ~loc (List.map method_type names) Closed in
  [%expr
    {
      disj_concat =
        (fun l r ->
          [%e concat_body ([%expr l], left_methods) ([%expr r], right_methods)]);
      disj_splitL = (fun lr -> (lr :> [%t split_type left_methods]));
      disj_splitR = (fun lr -> (lr :> [%t split_type right_methods]));
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

let declare_roles ~loc strlocs : Parsetree.module_expr =
  Ast_helper.(
    Mod.structure ~loc
      (List.mapi
         (fun i strloc ->
           let_ ~loc strloc
             (role_expr ~loc i
                ("role_" ^ Stdlib.String.capitalize_ascii strloc.txt)))
         strlocs))

let payload_ident_tuple =
  let open Ppxlib.Ast_pattern in
  pstr (pstr_eval (pexp_tuple @@ many (pexp_ident (lident __'))) nil ^:: nil)

let labels =
  let open Ppxlib in
  Extension.declare "declare_labels" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_labels ~loc strlocs]])

let roles =
  let open Ppxlib in
  Extension.declare "declare_roles" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_roles ~loc strlocs]])

let roles_prefixed =
  let open Ppxlib in
  Extension.declare "declare_roles_prefixed" Extension.Context.Structure_item
    payload_ident_tuple (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_roles ~loc strlocs]])

let disj =
  let open Ppxlib in
  Extension.declare "disj" Extension.Context.Expression
    Ast_pattern.(
      pstr
      @@ pstr_eval
           (pexp_tuple
              ((elist (pexp_ident (lident __'))
               ||| map1 ~f:(fun x -> [ x ]) (pexp_ident (lident __')))
              ^:: (elist (pexp_ident (lident __'))
                  ||| map1 ~f:(fun x -> [ x ]) (pexp_ident (lident __')))
              ^:: nil))
           nil
      ^:: nil)
    (fun ~loc ~path:_ ls rs -> disj_expr ~loc ls rs)

let () =
  Ppxlib.Driver.register_transformation
    ~extensions:[ labels; roles; roles_prefixed; disj ]
    "ppx_mpst"
