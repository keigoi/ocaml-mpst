open Parsetree
open Ast_helper

exception Pending of Location.t * string

let error loc (s : string) = Location.raise_errorf ~loc "%s" s

let field_types_otyp ~loc = function
  | Outcometree.Otyp_object (flds, None) -> flds
  | typ ->
      raise
        (Pending
           ( loc,
             Format.asprintf "not applicable for object concatenation: %a"
               !Ocaml_common.Oprint.out_type
               typ ))

let typeconstr_name typ =
  match Printtyp.tree_of_typexp false typ with
  | Outcometree.Otyp_constr (ident, _) -> ident
  | _ -> assert false

let make_method ?loc fld exp =
  Ast_helper.Cf.method_ ?loc (Location.mknoloc fld) Public
    (Cfk_concrete (Fresh, exp))

let make_call ?loc exp fld = Ast_helper.Exp.send ?loc exp (Location.mknoloc fld)

let make_object ?loc0 flds =
  let loc = Location.none in
  Exp.object_ ?loc:loc0 @@ Ast_helper.Cstr.mk [%pat? _] flds

let make_field ?loc var (fld, _typ) =
  make_method ?loc fld (make_call ?loc var fld)

let generate_projection ~loc ?from ?onto vars_typs =
  let rec loop fld_nests vars_typs =
    let vars_flds =
      List.map (fun (domvar, typ) -> (domvar, field_types_otyp ~loc typ)) vars_typs
    in
    let all_flds =
      let flds =
        match onto with
        | Some onto ->
            List.concat_map
              (function
                | var, flds when var = onto -> List.map fst flds | _ -> [])
              vars_flds
        | None -> List.concat_map (fun (_, flds) -> List.map fst flds) vars_flds
      in
      List.sort_uniq String.compare @@ flds
    in
    let make_field fld =
      let nests = fld :: fld_nests in
      match
        List.concat_map
          (fun (var, inner_flds_typs) ->
            match List.assoc_opt fld inner_flds_typs with
            | Some typ -> [ (var, typ) ]
            | None -> [])
          vars_flds
      with
      | [ (var, _) ] ->
          let var = Option.value ~default:var from in
          let var = Exp.ident (Location.mknoloc @@ Longident.Lident var) in
          (* field has no duplicate. just project onto it *)
          make_method fld
            (List.fold_left (fun exp f -> make_call exp f) var @@ List.rev nests)
      | ents ->
          (* field has duplicates. try to separate it further *)
          make_method fld (loop nests ents)
    in
    Ast_helper.Exp.object_
    @@ Ast_helper.Cstr.mk [%pat? _]
    @@ List.map make_field all_flds
  in
  loop [] vars_typs

let concatenation ~loc typs = generate_projection ~loc typs
let projection ~loc onto typs = generate_projection ~loc ~from:"lr" ~onto typs

let rec abbrev_paths = function
  | Types.Mcons (_, path, _, _, xs) -> path :: abbrev_paths xs
  | Mnil -> []
  | Mlink xs -> abbrev_paths !xs

let fill_hole ~loc (holeexp : Typedtree.expression) =
  let typ = Ctype.repr (Ctype.expand_head holeexp.exp_env holeexp.exp_type) in
  let _lr, l, r =
    (* extract inferred type args of ('lr,'l,'r) Rows.disj *)
    match typ.Types.desc with
    | Types.Tconstr (_, [ lr; l; r ], _) -> (lr, l, r)
    | _ ->
        error loc
          (Format.asprintf "Bad hole type:%a" Ocaml_common.Printtyp.type_expr
             typ)
  in
  let l_typ, r_typ =
    (Printtyp.tree_of_typexp false l, Printtyp.tree_of_typexp false r)
  in
  let typs = [ ("l", l_typ); ("r", r_typ) ] in
  let expr =
    try
      [%expr
        {
          disj_concat = (fun l r -> [%e concatenation ~loc typs]);
          disj_splitL = (fun lr -> [%e projection ~loc "l" typs]);
          disj_splitR = (fun lr -> [%e projection ~loc "r" typs]);
        }]
    with (Location.Error _) as e ->
      print_endline
      @@ Format.asprintf "error parsing types: %a\n" !Oprint.out_type
           (Printtyp.tree_of_typexp false typ);
      raise e
  in
  expr
