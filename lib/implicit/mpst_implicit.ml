include Mpst_base

module Make(X:sig type conn end) = struct
  module Global = Global.Make(X)
  module Session = Global.Session
end

module Global = Global.Make(struct type conn = Obj.t Forkpipe.conn end)
module Session = Global.Session
module Forkpipe = Forkpipe
                                 
module Util = struct
  module Global = Global
  open Session
  open Global
  open Forkpipe

  let rec count : type t. [>] list * t slots lazy_t -> (Obj.t * int) list  =
    function
    | r::rs, lazy(Cons(lazy(One(lazy(Sess(_,p)))), ss)) -> (Obj.repr r, 1) :: count (rs, ss)
    | r::rs, lazy(Cons(lazy(Many(lazy(ms))), ss)) -> (Obj.repr r, List.length ms) :: count (rs, ss)
    | [],lazy Nil -> []
    | [], lazy (Cons(_,_)) -> failwith "too few roles"
    | _::_, lazy Nil -> failwith "too many roles"
       
  let rec assign : type u. (Obj.t * (Obj.t * Obj.t conn list list)  list) list * u slots lazy_t -> u slots lazy_t =
    function
    | (_,es)::ks,lazy(Cons(lazy(One(lazy(Sess(_,p)))),ss)) ->
       let kt =
         List.fold_left (fun kt (r,ps) ->
             assert (List.length ps = 1);
             ConnTable.putmany kt r (List.hd ps)) (ConnTable.create ()) es in
       lazy(Cons(lazy(One(lazy(Sess(kt,p)))), assign (ks, ss)))
       
    | (_,es)::ks,lazy(Cons(lazy(Many(lazy(ps))),ss)) ->
       let roles, pipes = List.split es in
       let pipes = transpose pipes |> List.map (List.combine roles) in
       let ps = List.map (fun (Sess(kt,p), pipes) ->
           let kt = List.fold_left (fun kt (r,ps) -> ConnTable.putmany kt r ps) (ConnTable.create ()) pipes in
           Sess(kt,p)) (List.combine ps pipes)
       in
       lazy(Cons(lazy(Many(lazy ps)), assign (ks, ss)))
    | [], lazy Nil -> lazy Nil
    | _ -> assert false
    
  let rec mkpipes : type t. [>] list -> t slots lazy_t -> t slots lazy_t  =
    fun rs ss ->
    let roles = count (rs, ss) in
    let pipes = Forkpipe.mpst_pipes_groups roles in
    assign (pipes, ss)

  let fork f =
    if Unix.fork () = 0 then begin
        (f () : unit);
        exit 0
      end else
      ()

  let msg =
    {select_label=(fun f -> object method msg v=f v end);
     offer_label=(fun (v,c) -> `msg(v,c));
     channel={
         sender=(fun k -> write (fun v -> `msg(v)) k);
         receiver=(fun k -> try_read (function `msg(v) -> Some(v) | _ -> None) k)
    }}
    
  let left =
    {select_label=(fun f -> object method left v=f v end);
     offer_label=(fun (v,c) -> `left(v,c));
     channel={
         sender=(fun k -> write (fun v -> `left(v)) k);
         receiver=(fun k -> try_read (function `left(v) -> Some(v) | _ -> None) k)
    }}

  let right =
    {select_label=(fun f -> object method right v=f v end);
     offer_label=(fun (v,c) -> `right(v,c));
     channel={
         sender=(fun k -> write (fun v -> `right(v)) k);
         receiver=(fun k -> try_read (function `right(v) -> Some(v) | _ -> None) k)
    }}

  let left_or_right =
    {label_merge=(fun ol or_ -> object method left=ol#left method right=or_#right end)}
  let right_or_left =
    {label_merge=(fun ol or_ -> object method left=or_#left method right=ol#right end)}

end
                
