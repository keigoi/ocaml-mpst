(* module type SESSION =
 *   sig
 *     type 'p sess
 *     val send :
 *       ([>  ] as 'a) ->
 *       ((< .. > as 'b) -> 'v -> 's sess) ->
 *       'v -> ('a, 'b) send sess -> 's sess
 *     val receive : ([>  ] as 'a) -> ('a, 'ls) receive sess -> 'ls Lwt.t
 *     val close : close sess -> unit
 *     (\* val scatter :
 *      *   ([>  ] as 'a) ->
 *      *   ((< .. > as 'b) -> 'v -> 's sess) ->
 *      *   (int -> 'v) -> ('a, 'b) sendmany sess -> 's sess
 *      * val gather : ([>  ] as 'a) -> ('a, 'ls) receivemany sess -> 'ls Lwt.t *\)
 *     module Internal : sig val merge : 't prot -> 't prot -> 't prot end
 *   end *)
