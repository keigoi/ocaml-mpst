include Mpst_base

module Make(X:S.RAW) = struct
  module Session = Session.Make(X)
  module Global = Global.Make(X)
  module Util = Util.Make(X)

  module Connection = Connection.Make(X)

  open Session
  open Global
     
  let rec mkpipes : type t. [>] list -> t slots lazy_t -> t slots lazy_t  =
    fun rs ss ->

    let rec count : type t. [>] list * t slots lazy_t -> (Obj.t * int) list  =
      function
      | r::rs, lazy(Cons(lazy(One(lazy(Sess(_,p)))), ss)) -> (Obj.repr r, 1) :: count (rs, ss)
      | r::rs, lazy(Cons(lazy(Many(lazy(ms))), ss)) -> (Obj.repr r, List.length ms) :: count (rs, ss)
      | [],lazy Nil -> []
      | [], lazy (Cons(_,_)) -> failwith "too few roles"
      | _::_, lazy Nil -> failwith "too many roles"
    in
    
    
    let rec assign : type u. (Obj.t * (Obj.t * conn list list)  list) list * u slots lazy_t -> u slots lazy_t =
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
         let ps =
           List.map
             (fun (Sess(kt,p), pipes) ->
               let kt = List.fold_left (fun kt (r,ps) -> ConnTable.putmany kt r ps) (ConnTable.create ()) pipes in
               Sess(kt,p)) (List.combine ps pipes)
         in
         lazy(Cons(lazy(Many(lazy ps)), assign (ks, ss)))
      | [], lazy Nil -> lazy Nil
      | _ -> assert false
    in
    
    let roles = count (rs, ss) in
    let pipes = Connection.mpst_pipes_groups roles in
    assign (pipes, ss)
end

module IPC = struct
  include Make(Raw_unixpipe)

  let fork f =
    if Unix.fork () = 0 then begin
        (f () : unit);
        exit 0
      end else
      ()
end
                    
module Lwt = struct
  include Make(Raw_lwtstream)
end
                    
