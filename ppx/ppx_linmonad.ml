open Migrate_parsetree.Ast_405
open Migrate_parsetree.Ast_405.Asttypes
open Migrate_parsetree.Ast_405.Parsetree
open Migrate_parsetree.Ast_405.Ast_helper
open Ast_convenience_405

let newname =
  let r = ref 0 in
  fun prefix ->
  let i = !r in
  r := i + 1;
  Printf.sprintf "__ppx_linocaml_%s_%d" prefix i

let root_module = ref "Syntax"

let monad_bind_data ~loc =
  evar ~loc (!root_module ^ ".bind")

let monad_bind_lin ~loc =
  evar ~loc (!root_module ^ ".bind_lin")

let monad_return_lin ~loc =
  evar ~loc (!root_module ^ ".return_lin")

let get_lin ~loc =
  evar ~loc (!root_module ^ ".get_lin")

let put_linval ~loc =
  evar ~loc (!root_module ^ ".put_linval")

let mkbindfun ~loc ?attrs expr =
  let fieldname = {loc; txt = Longident.parse (!root_module ^ ".__call")} in
  Exp.record ~loc ?attrs [(fieldname, expr)] None

let mkdataconstr ~loc ?attrs expr =
  let fieldname = {loc; txt = Longident.parse (!root_module ^ ".data")} in
  Exp.record ~loc ?attrs [(fieldname, expr)] None

let mklinpat ~loc pat =
  precord ~loc ~closed:Closed [(!root_module ^ ".__lindata", pat)]

let mkdatapat ~loc pat =
  precord ~loc ~closed:Closed [(!root_module ^ ".data", pat)]

let error ~loc (s:string) =
  Location.raise_errorf ~loc "%s" s

let add_putval es expr =
  let insert_put_lin lensvar newvar =
    app ~loc:lensvar.loc
      (put_linval ~loc:lensvar.loc)
      [Exp.ident ~loc:lensvar.loc lensvar;
       evar ~loc:lensvar.loc newvar]
  in
  List.fold_right (fun (lensvar, newvar) expr ->
      app ~loc:lensvar.loc
        (monad_bind_data ~loc:lensvar.loc)
        [insert_put_lin lensvar newvar; lam (punit ()) expr]) es expr

let add_takeval lensbinds expr =
  List.fold_right
    (fun (v,lens) expr ->
      let loc = lens.pexp_loc in
      app
        (monad_bind_lin ~loc)
        [app ~loc (get_lin ~loc) [lens];
         mkbindfun ~loc (lam ~loc (pvar v) expr)])
    lensbinds expr

(**
 * The main function converting lens-patterns
 *)
let convert_pattern (p : pattern) : pattern * (Longident.t Location.loc * string) list =
  let lin_vars = ref [] in
  let replace_linpat ({loc; _} as linvar) =
    let newvar = newname "match" in
    lin_vars := (linvar,newvar) :: !lin_vars;
    mklinpat ~loc (pvar ~loc newvar)
  and wrap_datapat ({ppat_loc; _} as pat) =
    mkdatapat ~loc:ppat_loc pat
  in
  let rec traverse ({ppat_desc; _} as patouter) =
    match ppat_desc with
    | Ppat_type lidloc -> replace_linpat lidloc
    (* #tconst *)
    | Ppat_any -> wrap_datapat patouter
    (* _ *)
    | Ppat_var _ -> wrap_datapat patouter
    (* x *)

    | Ppat_constant _ (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
      | Ppat_interval (_,_) (* 'a'..'z' *)
      | Ppat_construct (_,None) (* constructor without payloads*)
      | Ppat_variant (_, None) -> (* polymorphic variant constructor without payloads*)
       patouter

    | Ppat_tuple pats -> {patouter with ppat_desc=Ppat_tuple(List.map traverse pats)}
    (* (P1, ..., Pn)

           Invariant: n >= 2
     *)
    | Ppat_construct (lidloc,Some(pat)) -> {patouter with ppat_desc=Ppat_construct(lidloc,Some(traverse pat))}
    (* C                None
           C P              Some P
           C (P1, ..., Pn)  Some (Ppat_tuple [P1; ...; Pn])
     *)
    | Ppat_variant (lab,Some(pat)) -> {patouter with ppat_desc=Ppat_variant(lab,Some(traverse pat))}
    (* `A             (None)
           `A P           (Some P)
     *)
                                    
    | Ppat_record ([({txt=id; _}, _)], Closed) when ["data"]=Longident.flatten id ->
       patouter (* do nothing inside {data=pat} *)
       
    | Ppat_record (recpats, Closed) ->
       {patouter with
         ppat_desc=Ppat_record(List.map (fun (field,pat) -> (field,traverse pat)) recpats, Closed)
       }
    (* { l1=P1; ...; ln=Pn }     (flag = Closed)
           { l1=P1; ...; ln=Pn; _}   (flag = Open)

           Invariant: n > 0
     *)
    | Ppat_array pats -> {patouter with ppat_desc=Ppat_array (List.map traverse pats)}
    (* [| P1; ...; Pn |] *)
    | Ppat_constraint (pat,typ)  -> {patouter with ppat_desc=Ppat_constraint(traverse pat,typ)}
    (* (P : T) *)
    | Ppat_lazy pat -> {patouter with ppat_desc=Ppat_lazy(traverse pat)}

    | Ppat_alias (_, tvarloc) ->
       error ~loc:tvarloc.loc "as-pattern is forbidden at %lin match" (* TODO relax this *)
    (* {patouter with ppat_desc=Ppat_alias(traverse pat,tvarloc)} *)
    (* P as 'a *)

    | Ppat_record (_, Open)
      | Ppat_or (_,_) | Ppat_unpack _
      | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
       error ~loc:patouter.ppat_loc "%lin cannot handle this pattern"
  in
  let p = traverse p in
  p, List.rev !lin_vars


let rec is_linpat {ppat_desc;ppat_loc; _} =
  match ppat_desc with
  | Ppat_type _ -> true
  | Ppat_alias (pat,_) -> is_linpat pat
  | Ppat_constraint (pat,_)  -> is_linpat pat
  | Ppat_any | Ppat_var _
    | Ppat_constant _ | Ppat_interval (_,_)
    | Ppat_tuple _ | Ppat_construct (_,_)
    | Ppat_variant (_,_) | Ppat_record (_, _)
    | Ppat_array _ | Ppat_lazy _ -> false
  | Ppat_or (_,_) | Ppat_unpack _
    | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
     error ~loc:ppat_loc "%lin cannot handle this pattern"

(* (#p, #q, x) ==> (__tmp1, __tmp2, x), [(#p, "__tmp1"), (#q, "__tmp2")] *)
let lin_pattern oldpat : pattern * (Longident.t Location.loc * string) list=
  let wrap ({ppat_loc; _} as oldpat) =
    let newpat, lin_vars = convert_pattern oldpat in
    let newpat =
      if is_linpat oldpat then
        newpat (* not to duplicate Lin pattern *)
      else
        mklinpat ~loc:ppat_loc newpat
    in
    newpat, lin_vars
  in
  let newpat,lin_vars = wrap oldpat in
  newpat, lin_vars

let make_lin_match_case ({pc_lhs;pc_rhs; _} as case) =
  let newpat, linvars = lin_pattern pc_lhs in
  {case with pc_lhs=newpat; pc_rhs=add_putval linvars pc_rhs}

let rec linval ({pexp_desc;pexp_loc;pexp_attributes} as outer) =
  match pexp_desc with
  | Pexp_ident _ | Pexp_constant _
    | Pexp_construct (_,None)
    | Pexp_variant (_,None) ->
     outer, []

  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident"!!"; _};_} , [(Nolabel,lens)]) ->
     let newvar = newname "linval" in
     evar ~loc:pexp_loc newvar, [(newvar,lens)]

  | Pexp_tuple (exprs) ->
     let exprs, bindings = List.split (List.map linval exprs) in
     {pexp_desc=Pexp_tuple(exprs);pexp_loc;pexp_attributes}, List.concat bindings

  | Pexp_construct (lid,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_construct(lid,Some(expr));pexp_loc;pexp_attributes}, binding
  | Pexp_variant (lab,Some(expr)) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_variant(lab,Some(expr));pexp_loc;pexp_attributes}, binding

  (* {data=exp} --> {Syntax.data=exp} (do we actually need this?) *)
  | Pexp_record ([({txt=id; _}, expr)],None) when ["data"]=Longident.flatten id ->
     mkdataconstr ~loc:pexp_loc ~attrs:pexp_attributes expr, []
    
  | Pexp_record (fields,expropt) ->
     let fields, bindings =
       List.split (List.map (fun (lid,expr) -> let e,b = linval expr in (lid,e),b) fields)
     in
     let bindings = List.concat bindings in
     let expropt, bindings =
       match expropt with
       | Some expr ->
          let expr, binding = linval expr in
          Some expr, binding @ bindings
       | None -> None, bindings
     in
     {pexp_desc=Pexp_record(fields,expropt);pexp_loc;pexp_attributes}, bindings
  | Pexp_array (exprs) ->
     let exprs, bindings =
       List.split (List.map linval exprs)
     in
     {pexp_desc=Pexp_array(exprs);pexp_loc;pexp_attributes}, List.concat bindings
  | Pexp_constraint (expr,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_constraint(expr,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_coerce (expr,typopt,typ) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_coerce(expr,typopt,typ);pexp_loc;pexp_attributes}, binding
  | Pexp_lazy expr ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_lazy(expr);pexp_loc;pexp_attributes}, binding
  | Pexp_open (oflag,lid,expr) ->
     let expr, binding = linval expr
     in
     {pexp_desc=Pexp_open(oflag,lid,expr);pexp_loc;pexp_attributes}, binding
  | Pexp_apply (expr,exprs) ->
     let expr, binding = linval expr in
     let exprs, bindings =
       List.split @@
         List.map
           (fun (lab,expr) -> let expr,binding = linval expr in (lab,expr),binding)
           exprs
     in
     begin match binding @ List.concat bindings with
     | [] -> {pexp_desc=Pexp_apply(expr,exprs);pexp_loc;pexp_attributes}, []
     | _ ->
        error ~loc:pexp_loc "function call inside %linval cannot contain slot references (!! slotname)"
     end
  | Pexp_object ({pcstr_self={ppat_desc=Ppat_any; _}; pcstr_fields=fields} as o) ->
     let new_fields, bindings =
       List.split @@ List.map
                       (function
                        | ({pcf_desc=Pcf_method (name,Public,Cfk_concrete(fl,expr)); _} as f) ->
                           let new_expr, binding = linval expr in
                           {f with pcf_desc=Pcf_method(name,Public,Cfk_concrete(fl,new_expr))}, binding
                        | _ ->
                           error ~loc:pexp_loc "object can only contain public method")
                       fields
     in
     {pexp_desc=Pexp_object({o with pcstr_fields=new_fields});pexp_loc;pexp_attributes},
     List.concat bindings
  | Pexp_object _ ->
     error ~loc:pexp_loc "object in linval can't refer to itself"
  | Pexp_poly (expr,None) ->
     let expr, binding = linval expr in
     {pexp_desc=Pexp_poly(expr,None);pexp_loc;pexp_attributes}, binding
  | Pexp_poly (_,_) ->
     error ~loc:pexp_loc "object method can not have type ascription"
  | Pexp_let (_,_,_) | Pexp_function _
    | Pexp_fun (_,_,_,_) | Pexp_match (_,_) | Pexp_try (_,_)
    | Pexp_field (_,_) | Pexp_setfield (_,_,_) | Pexp_ifthenelse (_,_,_)
    | Pexp_sequence (_,_) | Pexp_while (_,_) | Pexp_for (_,_,_,_,_)
    | Pexp_send (_,_) | Pexp_new _ | Pexp_setinstvar (_,_) | Pexp_override _
    | Pexp_letmodule (_,_,_) | Pexp_assert _ | Pexp_newtype (_,_)
    | Pexp_pack _ | Pexp_extension _
    | Pexp_unreachable | Pexp_letexception _
    -> error ~loc:pexp_loc "%linval can only contain values"

let expression_mapper id mapper exp attrs =
  let attrs = attrs @ exp.pexp_attributes in
  let loc=exp.pexp_loc in
  let process_inner expr = mapper.Ast_mapper.expr mapper expr
  in
  match id, exp.pexp_desc with
  (* let%lin *)
  | "lin", Pexp_let (Nonrecursive, vbls, expr) ->
     let conv_binding ({pvb_pat; _} as vb) =
       let newpat, lensbinds = lin_pattern pvb_pat in
       {vb with pvb_pat=newpat}, lensbinds
     in
     let new_vbls, lensbinds = List.split (List.map conv_binding vbls) in
     let new_expr = add_putval (List.concat lensbinds) expr in
     let make_bind {pvb_pat; pvb_expr; pvb_loc; pvb_attributes} expr =
       let loc = pvb_loc in
       app ~loc
         (monad_bind_lin ~loc)
         [pvb_expr;
          mkbindfun ~loc ~attrs:pvb_attributes (lam ~loc pvb_pat expr)]
     in
     let new_expr = List.fold_right make_bind new_vbls new_expr in
     let new_expr = {new_expr with pexp_attributes=attrs @ new_expr.pexp_attributes} in
     Some (process_inner new_expr)

  (* match%lin *)
  | "lin", Pexp_match(matched, cases) ->
     let new_cases = List.map make_lin_match_case cases in
     let new_expr =
       app ~loc ~attrs
         (monad_bind_lin ~loc)
         [matched;
          mkbindfun ~loc (Exp.function_ ~loc new_cases)]
     in
     Some (process_inner new_expr)

  | "lin", Pexp_function(cases) ->
     let cases = List.map make_lin_match_case cases in
     let new_expr = mkbindfun ~loc ~attrs (Exp.function_ ~loc cases)
     in
     Some (process_inner new_expr)

  | "lin", Pexp_fun(Nolabel,None,pat,expr) ->
     let newpat, linvars = lin_pattern pat in
     let newexpr = add_putval linvars expr in
     let new_expr =
       mkbindfun ~loc ~attrs (Exp.fun_ ~loc Nolabel None newpat newexpr)
     in
     Some (process_inner new_expr)

  | "lin", _ ->
     error ~loc "Invalid content for extension %lin; it must be \"let%lin slotname = ..\" OR \"match%lin slotname with ..\""

  | "linret", expr ->
     let new_exp, lensbinds = linval {pexp_desc=expr;pexp_loc=loc;pexp_attributes=attrs} in
     let new_exp = app (monad_return_lin ~loc) [new_exp] in
     let new_exp = add_takeval lensbinds new_exp in
     Some({new_exp with pexp_attributes=attrs @ new_exp.pexp_attributes})

  | _ -> None

let mapper_fun _ _ =
  let open Ast_mapper in
  let expr mapper outer =
    match outer with
    (* expand macros like [%xxx e], let%xxx and so on*)
    | {pexp_desc=Pexp_extension({ txt = id; _ },
                                PStr([{pstr_desc=Pstr_eval(exp,exp_attrs); _}]));
       pexp_attributes=outer_attrs;
       _} ->
       begin match expression_mapper id mapper exp (exp_attrs @ outer_attrs) with
       | Some exp -> exp
       | None -> default_mapper.expr mapper outer
       end
    | _ -> default_mapper.expr mapper outer
  in
  {default_mapper with expr}

open Migrate_parsetree
let migration =
  Versions.migrate Versions.ocaml_405 Versions.ocaml_current

let () =
  Driver.register
    ~name:"ppx_linmonad"
    Versions.ocaml_405
    mapper_fun
