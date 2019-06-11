(* include Mpst_base
 * 
 * module Make(F:S.FLAG)(X:S.RAW) = struct
 *   module Session = Session.Make(F)(X)
 *   module Global = Global.Make(F)(X)
 *   module Util = Util.Make(F)(X)
 * 
 *   module Connection = Connection.Make(X)
 * 
 *   open Session
 *   open Global
 * 
 *   type conn = X.conn
 *      
 *   let assign ps ss =   
 *     let rec loop : type u. (Obj.t * (Obj.t * conn list list)  list) list * u slots lazy_t -> u slots lazy_t =
 *       function
 *       | (_,es)::ks,lazy(Cons(lazy(One(lazy {prot=p; _})),ss)) ->
 *          let kt =
 *            List.fold_left (fun kt (r,ps) ->
 *                assert (List.length ps = 1);
 *                ConnTable.putmany kt r (List.hd ps)) (ConnTable.create ()) es in
 *          lazy(Cons(lazy(One(lazy {once=F.create(); conn=kt; prot=p})), loop (ks, ss)))
 *          
 *       | (_,es)::ks,lazy(Cons(lazy(Many(lazy(ps))),ss)) ->
 *          let roles, pipes = List.split es in
 *          let pipes = transpose pipes |> List.map (List.combine roles) in
 *          let ps =
 *            List.map
 *              (fun (s, pipes) ->
 *                let kt = List.fold_left (fun kt (r,ps) -> ConnTable.putmany kt r ps) (ConnTable.create ()) pipes in
 *                {s with conn=kt}) (List.combine ps pipes)
 *          in
 *          lazy(Cons(lazy(Many(lazy ps)), loop (ks, ss)))
 *       | [], lazy Nil -> lazy Nil
 *       | _ -> assert false
 *     in
 *     loop (ps, ss)
 *     
 *   let rec mkpipes : type t. [>] list -> t slots lazy_t -> (Obj.t * (Obj.t * conn list list)  list) list =
 *     fun rs ss ->
 *     let rec count : type t. [>] list * t slots lazy_t -> (Obj.t * int) list  =
 *       function
 *       | r::rs, lazy(Cons(lazy(One(lazy {prot=p; _})), ss)) -> (Obj.repr r, 1) :: count (rs, ss)
 *       | r::rs, lazy(Cons(lazy(Many(lazy ms)), ss)) -> (Obj.repr r, List.length ms) :: count (rs, ss)
 *       | [],lazy Nil -> []
 *       | [], lazy (Cons(_,_)) -> failwith "too few roles"
 *       | _::_, lazy Nil -> failwith "too many roles"
 *     in
 *     let roles = count (rs, ss) in
 *     Connection.mpst_pipes_groups roles
 * 
 *   let pipes rs ss =
 *     let ps = mkpipes rs ss in
 *     assign ps ss 
 *     
 * end
 * 
 * module IPC = struct
 *   include Make(Flags.NanoMutexFlag)(Raw_unixpipe)
 *   module Lin = LinSession.Make(Session)
 * end
 *                     
 * module Lwt = struct
 *   include Make(Flags.NanoMutexFlag)(Raw_lwtstream)
 *   module Lin = LinSession.Make(Session)
 * end
 *                     
 * 
 * module IPCNoDynCheck = struct
 *   include Make(Flags.NoFlag)(Raw_unixpipe)
 *   module Lin = LinSession.Make(Session)
 * end
 *                     
 * module LwtNoDynCheck = struct
 *   include Make(Flags.NoFlag)(Raw_lwtstream)
 *   module Lin = LinSession.Make(Session)
 * end *)

