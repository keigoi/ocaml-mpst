(* TODO: replace "failwith" with proper error-handling *)
open Asttypes
open Longident
open Parsetree
open Ast_helper

let may_tuple ?loc tup = function
  | [] -> None
  | [x] -> Some x
  | l -> Some (tup ?loc ?attrs:None l)

let lid ?(loc = !default_loc) ~quals s =
  match Longident.unflatten (quals @ [s]) with
  | Some s -> Location.mkloc s loc
  | None -> assert false
let app ?loc ?attrs f l =
  if l = []
  then f
  else Exp.apply ?loc ?attrs f (List.map (fun a -> Nolabel, a) l)
let lam ?loc ?(attrs=[]) ?(label = Nolabel) ?default pat exp =
  Exp.fun_ ?loc ~attrs:attrs label default pat exp

let evar ?loc ?attrs ~quals id =
  Exp.ident ?loc ?attrs (lid ?loc ~quals id)
let record ?loc ?attrs ?over ?(quals=[]) l =
  Exp.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.pexp_loc ~quals s, e)) l) over
let constr ?loc ?attrs ~quals s args = Exp.construct ?loc ?attrs (lid ?loc ~quals s) (may_tuple ?loc Exp.tuple args)
let precord ?loc ?attrs ?(closed = Open) ~quals l =
  Pat.record ?loc ?attrs (List.map (fun (s, e) -> (lid ~loc:e.ppat_loc ~quals s, e)) l) closed
let tconstr ?loc ?attrs ~quals c l = Typ.constr ?loc ?attrs (lid ?loc ~quals c) l
let unit ?loc ?attrs () = constr ?loc ?attrs "()" []

let pconstr ?loc ?attrs s args =
  Pat.construct ?loc ?attrs (lid ?loc ~quals:[] s) (may_tuple ?loc Pat.tuple args)
let punit ?loc ?attrs () =
  pconstr ?loc ?attrs "()" []
let pvar ?(loc = !default_loc) ?attrs s =
  Pat.var ~loc ?attrs (Location.mkloc s loc)

let newname =
  let r = ref 0 in
  fun prefix ->
    let i = !r in
    r := i + 1;
  Printf.sprintf "__ppx_linocaml_%s_%d" prefix i

let root_module = ref "Syntax"

let longident ?(loc= !default_loc) ~quals id = evar ~loc ~quals id

let monad_bind_data () =
  longident ~quals:[!root_module] "bind_data"

let monad_bind_lin () =
  longident ~quals:[!root_module] "bind_lin"

let monad_return_lin () =
  longident ~quals:[!root_module] "return_lin"

let get_lin () =
  longident ~quals:[!root_module] "get_lin"

let put_linval () =
  longident ~quals:[!root_module] "put_linval"

let lens_put' () =
  longident ~quals:[!root_module] "lens_put'"

let mkbind () =
  longident ~quals:[!root_module; "Internal"] "_mkbind"

let modify () =
  longident ~quals:[!root_module; "Internal"] "_modify"

let error loc (s:string) =
  Location.raise_errorf ~loc "%s" s


let add_putval es expr =
  let insert_expr (linvar, newvar) =
    app (modify ()) [app (lens_put' ()) [Exp.ident ~loc:linvar.loc linvar; evar ~loc:linvar.loc ~quals:[] newvar]]
  in
  List.fold_right (fun e expr ->
      app
        (insert_expr e)
        [expr]) es expr

let add_takeval es expr =
  List.fold_right
    (fun (v,slot) expr ->
      app
        (monad_bind_lin ())
        [app (get_lin ()) [slot]; app (mkbind ()) [lam (pvar v) expr]])
    es expr

(**
 * The main function converting lens-patterns
 *)
let convert_pattern (p : pattern) : pattern * (Longident.t Location.loc * string) list =
  let lin_vars = ref [] in
  let replace_linpat ({loc; _} as linvar) =
    let newvar = newname "match" in
    lin_vars := (linvar,newvar) :: !lin_vars;
    precord ~loc ~quals:["Linocaml"] [("__lin", pvar ~loc newvar)]
  and wrap_datapat ({ppat_loc=loc; _} as pat) =
    precord ~loc ~quals:["Linocaml"][("data", pat)]
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
  | Ppat_record ([({txt=Lident"data";_}, _)], Closed) as p ->
     {patouter with
       ppat_desc=p
     }
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
     error tvarloc.loc "as-pattern is forbidden at %lin match" (* TODO relax this *)
     (* {patouter with ppat_desc=Ppat_alias(traverse pat,tvarloc)} *)
        (* P as 'a *)

  | Ppat_record (_, Open)
  | Ppat_or (_,_) | Ppat_unpack _
  | Ppat_exception _ | Ppat_extension _ | Ppat_open _ ->
       error patouter.ppat_loc "%lin cannot handle this pattern"
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
     error ppat_loc "%lin cannot handle this pattern"

(* (#p, #q, x) ==> (__tmp1, __tmp2, x), [(#p, "__tmp1"), (#q, "__tmp2")] *)
let lin_pattern oldpat : pattern * (Longident.t Location.loc * string) list=
  let wrap ({ppat_loc; _} as oldpat) =
    let newpat, lin_vars = convert_pattern oldpat in
    let newpat =
      if is_linpat oldpat then
        newpat (* not to duplicate Lin pattern *)
      else
        precord ~loc:ppat_loc ~quals:["Linocaml"] [("__lin", newpat)]
    in
    newpat, lin_vars
  in
  let newpat,lin_vars = wrap oldpat in
  newpat, lin_vars

let make_lin_match_case ({pc_lhs=pat;pc_rhs=expr; _} as case) =
  let newpat, linvars = lin_pattern pat in
  {case with pc_lhs=newpat; pc_rhs=add_putval linvars expr}

let rec linval ({pexp_desc;pexp_loc;_(*pexp_loc_stack*)} as outer) =
  match pexp_desc with
  | Pexp_ident _ | Pexp_constant _
  | Pexp_construct (_,None)
  | Pexp_variant (_,None) ->
     outer, []

  | Pexp_apply ({pexp_desc=Pexp_ident {txt=Lident"!!"; _};_} , [(Nolabel,exp)]) ->
     let newvar = newname "linval" in
     longident ~loc:pexp_loc ~quals:[""] newvar, [(newvar,exp)]

  | Pexp_tuple (exprs) ->
    let exprs, bindings = List.split (List.map linval exprs) in
    {outer with pexp_desc=Pexp_tuple(exprs);pexp_loc}, List.concat bindings

  | Pexp_record ([({txt=Lident"data";_}, _)],None) ->
     outer, []

  | Pexp_construct (lid,Some(expr)) ->
     let expr, binding = linval expr in
     {outer with pexp_desc=Pexp_construct(lid,Some(expr))}, binding
  | Pexp_variant (lab,Some(expr)) ->
     let expr, binding = linval expr in
     {outer with pexp_desc=Pexp_variant(lab,Some(expr))}, binding
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
     {outer with pexp_desc=Pexp_record(fields,expropt)}, bindings
  | Pexp_array (exprs) ->
     let exprs, bindings =
       List.split (List.map linval exprs)
     in
     {outer with pexp_desc=Pexp_array(exprs)}, List.concat bindings
  | Pexp_constraint (expr,typ) ->
     let expr, binding = linval expr
     in
     {outer with pexp_desc=Pexp_constraint(expr,typ);}, binding
  | Pexp_coerce (expr,typopt,typ) ->
     let expr, binding = linval expr
     in
     {outer with pexp_desc=Pexp_coerce(expr,typopt,typ);}, binding
  | Pexp_lazy expr ->
     let expr, binding = linval expr
     in
     {outer with pexp_desc=Pexp_lazy(expr);}, binding
  | Pexp_open (odecl,expr) ->
     let expr, binding = linval expr
     in
     {outer with pexp_desc=Pexp_open(odecl,expr);}, binding
  | Pexp_apply (expr,exprs) ->
     let expr, binding = linval expr in
     let exprs, bindings =
       List.split @@
         List.map
           (fun (lab,expr) -> let expr,binding = linval expr in (lab,expr),binding)
           exprs
     in
     begin match binding @ List.concat bindings with
     | [] -> {outer with pexp_desc=Pexp_apply(expr,exprs)}, []
     | _ ->
        error pexp_loc "function call inside %linval cannot contain slot references (!! slotname)"
     end
  | Pexp_object ({pcstr_self={ppat_desc=Ppat_any; _}; pcstr_fields=fields} as o) ->
     let new_fields, bindings =
       List.split @@ List.map
         (function
          | ({pcf_desc=Pcf_method (name,Public,Cfk_concrete(fl,expr)); _} as f) ->
             let new_expr, binding = linval expr in
             {f with pcf_desc=Pcf_method(name,Public,Cfk_concrete(fl,new_expr))}, binding
          | _ ->
             error pexp_loc "object can only contain public method")
         fields
     in
     {outer with pexp_desc=Pexp_object({o with pcstr_fields=new_fields});},
     List.concat bindings
  | Pexp_object _ ->
     failwith "object in linval can't refer to itself"
  | Pexp_poly (expr,None) ->
     let expr, binding = linval expr in
     {outer with pexp_desc=Pexp_poly(expr,None);}, binding
  | Pexp_poly (_,_) ->
     failwith "object method can not have type ascription"
  | Pexp_let (_,_,_) | Pexp_function _
  | Pexp_fun (_,_,_,_) | Pexp_match (_,_) | Pexp_try (_,_)
  | Pexp_field (_,_) | Pexp_setfield (_,_,_) | Pexp_ifthenelse (_,_,_)
  | Pexp_sequence (_,_) | Pexp_while (_,_) | Pexp_for (_,_,_,_,_)
  | Pexp_send (_,_) | Pexp_new _ | Pexp_setinstvar (_,_) | Pexp_override _
  | Pexp_letmodule (_,_,_) | Pexp_assert _ | Pexp_newtype (_,_)
  | Pexp_pack _ | Pexp_extension _
  | Pexp_unreachable | Pexp_letexception _
  | Pexp_letop _
    -> failwith "%linval can only contain values"

let expression_mapper id mapper exp attrs =
  let pexp_attributes = exp.pexp_attributes @ attrs in
  let pexp_loc = exp.pexp_loc in
  let pexp_loc_stack = exp.pexp_loc_stack in
  let process_inner expr = mapper.Ast_mapper.expr mapper expr
  in
  match id, exp.pexp_desc with
  | "lin", Pexp_let (Nonrecursive, vbls, expr) ->
     let lin_binding ({pvb_pat; _} as vb) =
         let newpat, linvars = lin_pattern pvb_pat in
         {vb with pvb_pat=newpat}, linvars
     in
     let new_vbls, linvars = List.split (List.map lin_binding vbls) in
     let new_expr = add_putval (List.concat linvars) expr in
     let make_bind {pvb_pat;pvb_expr;pvb_loc; _} expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
           (monad_bind_lin ())
           [pvb_expr; app ~loc:pvb_loc (mkbind ()) [lam ~loc:pvb_loc pvb_pat expr]]
     in
     let new_expr = List.fold_right make_bind new_vbls new_expr
     in
     Some (process_inner new_expr)

  | "lin", Pexp_match(matched, cases) ->
     let new_cases = List.map make_lin_match_case cases in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (monad_bind_lin ())
         [matched; app ~loc:pexp_loc (mkbind ()) [Exp.function_ ~loc:pexp_loc new_cases]]
     in
     Some (process_inner new_expr)

  | "lin", Pexp_function(cases) ->
     let cases = List.map make_lin_match_case cases in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (mkbind ())
         [{pexp_desc=Pexp_function(cases); pexp_loc; pexp_attributes; pexp_loc_stack}]
     in
     Some (process_inner new_expr)

  | "lin", Pexp_fun(Nolabel,None,pat,expr) ->
     let newpat, linvars = lin_pattern pat in
     let newexpr = add_putval linvars expr in
     let new_expr =
       app ~loc:pexp_loc ~attrs:pexp_attributes
         (mkbind ())
         [{pexp_desc=Pexp_fun(Nolabel,None,newpat,newexpr); pexp_loc; pexp_attributes; pexp_loc_stack}]
     in
     Some (process_inner new_expr)

  | "lin", _ ->
     error pexp_loc "Invalid content for extension %lin; it must be \"let%lin slotname = ..\" OR \"match%lin slotname with ..\""

  | "linret", expr ->
     let new_exp, bindings = linval {pexp_desc=expr;pexp_loc;pexp_attributes;pexp_loc_stack} in
     let new_exp = app (monad_return_lin ()) [new_exp] in
     let new_exp = add_takeval bindings new_exp in
     Some(new_exp)

  | _ -> None

let has_runner attrs =
  List.exists (fun ({txt = name; _},_) -> name = "runner")  attrs

let mapper =
  let open Ast_mapper in
  let expr mapper outer =
  match outer with
  | {pexp_desc=Pexp_extension ({ txt = id; _ }, PStr([{pstr_desc=Pstr_eval(inner,inner_attrs); _}])); pexp_attributes=outer_attrs; _} ->
     begin match expression_mapper id mapper inner (inner_attrs @ outer_attrs) with
     | Some exp -> exp
     | None -> default_mapper.expr mapper outer
     end
  | _ -> default_mapper.expr mapper outer
  in
  {default_mapper with expr}

let () =
  Ppxlib.Driver.register_transformation 
    ~impl:(mapper.structure mapper)
    "ppx_linocaml"
    
