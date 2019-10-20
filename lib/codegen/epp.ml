open Ast_global

type local =
  | Out of string * (string * local) list
  | Inp of string * (string * local) list
  | Loop of string
  | Close

type terminal =
  Finished
| Looping of string

let assoc r (cs,t) =
  match (List.assoc_opt r cs, t) with
  | Some c, _ -> c
  | None, Finished -> Close
  | None, Looping(var) -> Loop(var)

let take r ((cs,t) as ls) =
  (assoc r ls, (List.filter (fun (r',_) -> r'<>r) cs, t))

let update r ls f =
  let c, (cs,t) = take r ls in
  ((r,f c)::cs, t)

let add r c (cs,t) =
  ((r,c)::cs, t)

let partition br1 br2 =
  let br1dup,br1only =
    List.partition
      (fun (l1,_) -> List.for_all (fun (l2,_) -> l1=l2) br2)
      br1
  in
  let br2dup,br2only =
    List.partition
      (fun (l2,_) -> List.for_all (fun (l1,_) -> l1=l2) br1)
      br2
  in
  (br1only,br1dup,br2dup,br2only)

let out_merge_ cl cr =
  match cl,cr with
  | Out(d1,br1),Out(d2,br2) ->
     if d1<>d2 then
       failwith @@ "out_merge: destination roles differ: " ^ d1 ^ ", " ^ d2
     else
       let (br1only,br1dup,br2dup,br2only) =
         partition br1 br2
       in
       if br1dup<>[] then
         failwith @@ "merge: duplicate : " ^ String.concat "," (List.map fst br1dup)
       else
         (d1, br1only, br2only)
  | _ -> failwith "non-output merging"

let out_merge cl cr =
  let d1, br1, br2 = out_merge_ cl cr in
  Out(d1, br1 @ br2)

let rec merge_all br1 br2 =
  List.map2 (fun (l1,c1) (l2,c2) -> (l1,merge c1 c2)) br1 br2
and merge cl cr =
  match cl, cr with
  | Out(d1,br1),Out(d2,br2) ->
     if d1<>d2 then
       failwith @@ "merge: output destination roles differ: " ^ d1 ^ ", " ^ d2
     else
       let (br1only,br1dup,br2dup,br2only) =
         partition br1 br2
       in
       if br1only<>[] then
         failwith @@ "merge: only in lhs: " ^ String.concat "," (List.map fst br1)
       else if br2only<>[] then
         failwith @@ "merge: only in rhs: " ^ String.concat "," (List.map fst br2)
       else
         Out(d1, merge_all br1dup br2dup)
  | Inp(d1,br1),Inp(d2,br2) ->
     if d1<>d2 then
       failwith @@ "merge: input source roles differ: " ^ d1 ^ ", " ^ d2
     else
       let (br1only,br1dup,br2dup,br2only) =
         partition br1 br2
       in
       Inp(d1, br1only @ merge_all br1dup br2dup @ br2only)
  | Close,Close -> Close
  | Loop(var1), Loop(var2) ->
     if var1=var2 then
       Loop(var1)
     else
       failwith "merging between different variables is not covered"
  | _ ->
     failwith "not mergeable"
       
    
let merge_locals (cs1,t1) (cs2,t2) =
  if t1<>t2 then
    failwith "branches terminate diferrently"
  else
    (merge_all cs1 cs2, t1)

let rec genlocals = function
  | Finish -> ([],Finished)
  | Seq(Comm, from, to_, l, cont) ->
     let cs = genlocals cont in
     let cs = update from cs (fun c -> Out(to_, [(l, c)])) in
     let cs = update to_ cs (fun c -> Inp(from, [(l, c)])) in
     cs
  | Goto(var) -> ([],Looping(var))
  | Choice(r,gl,gr) ->
     let cl,cr,g = gen_choice r gl gr in
     add r (out_merge cl cr) g
  | Seq(_) -> assert false
  | Guard _ -> assert false
and gen_choice r gl gr =
  let cl, gl = take r (genlocals gl) in
  let cr, gr = take r (genlocals gr) in
  (cl, cr, merge_locals gl gr)
