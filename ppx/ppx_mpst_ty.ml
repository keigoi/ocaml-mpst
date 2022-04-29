open Parsetree

let fillup_hole self (super : Untypeast.mapper) attr
    (texp : Typedtree.expression) =
  match attr with
  | { Parsetree.attr_name = { txt = "HOLE"; _ }; attr_loc = loc; _ } -> begin
      try Concat.fill_hole ~loc texp
      with Concat.Pending (_, _) -> super.expr self texp
    end
  | _ -> super.expr self texp

let untyper =
  let super = Untypeast.default_mapper in
  {
    Untypeast.default_mapper with
    expr =
      (fun self (texp : Typedtree.expression) ->
        match (texp.exp_attributes, texp.exp_extra) with
        | attr :: _, _ -> fillup_hole self super attr texp
        | _, (_, _, attr :: _) :: _ -> fillup_hole super self attr texp
        | _ -> super.expr self texp);
  }

let rec loop_typer_untyper str =
  Compmisc.init_path ();
  let env = Compmisc.initial_env () in
  let tstr, _, _, _ = Typemod.type_structure env str in
  let untypstr = untyper.structure untyper tstr in
  if str = untypstr then untypstr else loop_typer_untyper untypstr

let make_hole =
  let cnt = ref 0 in
  fun ~loc ->
    cnt := !cnt + 1;
    let typ = Ast_helper.Typ.var @@ "mpst_tmp_" ^ string_of_int !cnt in
    let exp = [%expr (assert false : [%t typ])] in
    {
      exp with
      pexp_attributes =
        [
          {
            attr_name = { txt = "HOLE"; loc = Location.none };
            attr_loc = loc;
            attr_payload = PStr [];
          };
        ];
    }

let replace_hashhash str =
  let obj =
    object (this)
      inherit Ppxlib.Ast_traverse.map as super

      method! expression exp =
        match exp.pexp_desc with
        | Pexp_apply
            ( {
                pexp_desc = Pexp_ident { txt = Lident "##"; _ };
                pexp_loc = loc_hole;
                _;
              },
              [ (_, arg1); (_, arg2) ] ) ->
            let loc = loc_hole in
            Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
              (this#expression arg1)
              [ (Nolabel, make_hole ~loc); (Nolabel, this#expression arg2) ]
        | _ -> super#expression exp
    end
  in
  obj#structure str

let transform str = loop_typer_untyper @@ replace_hashhash str

let make_choice_at ~loc role alt1 alt2 alts =
  Ast_helper.(
    let role = Exp.ident role in
    let choice e1 e2 =
      [%expr
        choice_at [%e role] [%HOLE] ([%e role], [%e e1]) ([%e role], [%e e2])]
    in
    List.fold_left (fun exp alt -> choice exp alt) (choice alt1 alt2) alts)

let hole =
  Ppxlib.Extension.declare "HOLE" Ppxlib.Extension.Context.expression
    Ppxlib.Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> make_hole ~loc)

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
  Ppxlib.Driver.register_transformation ~extensions:[ hole; choice ]
    ~instrument:(Ppxlib.Driver.Instrument.make ~position:After transform)
    "ppx_mpst_ty"
