open Parsetree

let make_choice_at ~loc role alt1 alt2 alts =
  Ast_helper.(
    let role = Exp.ident role in
    let choice e1 e2 =
      [%expr
        choice_at [%e role] [%HOLE] ([%e role], [%e e1]) ([%e role], [%e e2])]
    in
    List.fold_left (fun exp alt -> choice exp alt) (choice alt1 alt2) alts)

let choice =
  let open Ppxlib in
  Extension.declare "choice_at" Extension.Context.Expression
    Ast_pattern.(
      pstr
      @@ pstr_eval
           (alt
              (pexp_apply (pexp_ident __')
                 (pair nolabel __ ^:: pair nolabel __ ^:: many (pair nolabel __)))
              (pexp_apply (pexp_ident __')
                 (pair nolabel (pexp_tuple (__ ^:: __ ^:: many __)) ^:: nil)))
           nil
      ^:: nil)
    (fun ~loc ~path:_ role alt1 alt2 alts ->
      make_choice_at ~loc role alt1 alt2 alts)

let () =
  Ppxlib.Driver.register_transformation ~extensions:[ choice ] "ppx_mpst_ty"
