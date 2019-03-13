open Base

module Make(X:S.RAW) = struct

  let mpst_pipes roles =
    let rec loop myrole children =
      (* make pipes between myrole and children, and update children with a pipe to myrole *)
      let mypipes, children =
        List.map (fun (childrole, childparents_rev) ->
            let mypipe, childpipe = X.create () in
            (childrole, mypipe), (childrole, (myrole, childpipe)::childparents_rev)) children
        |> List.split
      in
      match children with
      | [] -> [], []
      | (r, childparents_rev)::children ->
         (* and make pipes among children recursively *)
         let childchildren, rest = loop r children in
         let childpipes = List.rev_append childparents_rev childchildren in
         mypipes, childpipes::rest
    in
    match roles with
    | [] -> []
    | r::rs ->
       let ps, pss = loop r (List.map (fun r -> (r,[])) rs) in
       ps::pss

  (* outer list: parents, inner list: children*)
  let pipes (myname,mycount) (childname,childcount) =
    repeat mycount (fun i ->
        repeat childcount (fun j -> X.create ()) |> List.split
      ) |> List.split

  type 'a entry = Entry of 'a
  type 'a peer = Peer of 'a
                       
  let mpst_pipes_groups roles =
    let rec loop myrole mycount children =
      let mypipes, children =
        List.map (fun (childrole, childcount, childparents_rev) ->
            let mypipes, childpipes = pipes (myrole,mycount) (childrole,childcount) in
            let childpipes = transpose childpipes in
            ((childrole, mypipes), (childrole, childcount, (myrole, childpipes)::childparents_rev))) children
        |> List.split
      in
      match children with
      | [] -> [], []
      | (childrole,childcount,childparents_rev)::children ->
         let childchildren, rest = loop childrole childcount children in
         let childpipes = List.rev_append childparents_rev childchildren in
         mypipes, childpipes::rest
    in
    match roles with
    | [] -> []
    | (r,c)::rs ->
       let ps, pss = loop r c (List.map (fun (r,c) -> (r,c,[])) rs) in
       List.combine (List.map fst roles) (ps::pss)
end  
