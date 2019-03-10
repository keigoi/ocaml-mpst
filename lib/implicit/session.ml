module Make(X:sig type conn end) = struct
  
  type ('r,'ls) send = DummySend__
  type ('r,'ls) sendmany = DummySendMany__
  type ('r,'ls) receive = DummyReceive__
  type ('r,'ls) receivemany = DummyReceiveMany__
  type close

  exception RoleNotEnabled
  exception ReceiveFail
          
  type conn = X.conn

  type 't one = One__ of 't
  type 't many = Many__ of 't
                         
                         
  type _ ep =
    EPOne : conn * 'r -> 'r one ep
  | EPMany : conn list * 'r -> 'r many ep
  
  type _ prot =
    | Send :
        'r * (conn -> 'ls)
        -> ('r, 'ls) send prot
    | SendMany :
        'r * (conn -> 'ls)
        -> ('r, 'ls) sendmany prot
    | Receive :
        'r * (conn -> 'ls Lwt.t) list
        -> ('r, 'ls) receive prot
    | ReceiveMany :
        'r * ((conn list -> 'ls Lwt.t) list)
        -> ('r, 'ls) receivemany prot
    | Close : close prot
    | DummyReceive :
        ('r, 'ls) receive prot

  let send : 'r 'ls 'v 's.
             'r one ep ->
             ((< .. > as 'ls) -> 'v -> 's prot) ->
             'v ->
             ('r, 'ls) send prot ->
             's prot =
    fun (EPOne(k,_)) sel v (Send (_,f)) ->
    let s = sel (f k) v in
    s

  let multicast : 'r 'ls 'v 's.
                  'r many ep ->
                  ((< .. > as 'ls) -> 'v -> 's prot) ->
                  (int -> 'v) ->
                  ('r, 'ls) sendmany prot ->
                  's prot =
    fun (EPMany(ks,_)) sel f (SendMany (_,ls)) ->
    match List.mapi (fun i k -> sel (ls k) (f i)) ks with
    | [] -> failwith "no connection"
    | s::_ -> s

  let rec first k = function
    | [] -> Lwt.fail (Failure "receive failed")
    | f::fs ->
       Lwt.catch (fun () -> f k) (function
           | ReceiveFail -> first k fs
           | e -> Lwt.fail e)
      
  let receive : 'r 'ls.
                'r one ep ->
                ('r, 'ls) receive prot -> 'ls Lwt.t =
    fun (EPOne(k,_)) s ->
    match s with
    | Receive(_, fs) ->
       first k fs
    | DummyReceive ->
       failwith "DummyReceive encountered" 

  let gather : 'r 'ls.
               'r many ep ->
               ('r, 'ls) receivemany prot -> 'ls Lwt.t =
    fun (EPMany(ks,_)) s ->
    match s with
    | ReceiveMany(_, f) ->
       first ks f

  let close Close = ()

  module Internal = struct
    
    let merge : type t. t prot -> t prot -> t prot = fun x y ->
      match x, y with
      | Send _, Send _ ->
         raise RoleNotEnabled
      | SendMany _, SendMany _ ->
         raise RoleNotEnabled
      | Receive (r, xs), Receive (_, ys) ->
         Receive (r, xs @ ys)
      | ReceiveMany (r, xs), ReceiveMany (_, ys) ->
         ReceiveMany (r, xs @ ys)
      | Receive (r, xs), DummyReceive ->
         Receive (r, xs)
      | DummyReceive, Receive (r, xs) ->
         Receive (r, xs)
      | DummyReceive, DummyReceive ->
         DummyReceive
      | Close, Close ->
         Close
  end
end
