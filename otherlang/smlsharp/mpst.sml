datatype 'r recr = Rec of 'r
datatype close = Close

fun close Close = ()

datatype ('la,'lb,'va,'vb) label =
         L of {out : 'la -> 'va, in_ : 'vb -> 'lb}

functor MPST
            (structure Thread:sig
                           type t
                           val create : ('a -> unit) -> 'a -> t
                       end
             structure Event:sig
                           type 'a event
                           type 'a channel
                           val new_channel : unit -> 'a channel
                           val send : 'a channel -> 'a -> unit event
                           val receive : 'a channel -> 'a event
                           val sync : 'a event -> 'a
                           val wrap : 'a event -> ('a -> 'b) -> 'b event
                           val guard : (unit -> 'a event) -> 'a event
                           val choose : 'a event list -> 'a event
                       end
            ) =

struct


(* Channel vectors for
 * { msg from A to B; msg from B to C; msg from C to A }
 *)

val ch0 = Event.new_channel ()
val ch1 = Event.new_channel ()
val ch2 = Event.new_channel ()

fun ea0 () =
    {role_A =
     {msg = fn v => (Event.send ch0 v; ea1 ()) }}

and ea1 () =
    Event.wrap
        (Event.receive ch2)
        (fn v => (fn obj => #msg (#role_C obj) (v, ea2 ()))) (* todo: role call must not be polymorphic  *)
and ea2 () = Close

fun eb0 () =
    Event.wrap
        (Event.receive ch0)
        (fn v => (fn obj => #msg (#role_A obj) (v, eb1 ())))
and eb1 () =
    {role_C =
     {msg = fn v => (Event.send ch1 v; eb2 ()) }}
and eb2 () = Close

fun ec0 () =
    Event.wrap
        (Event.receive ch1)
        (fn v => (fn obj => (#msg (#role_B obj) (v, ec1 ()))))
and ec1 () =
    {role_B =
     {msg = fn v => (Event.send ch2 v; ec2 ()) }}
and ec2 () = Close

val ea = ea0 ()
val eb = eb0 ()
val ec = ec0 ()

fun ta () =
    Thread.create
        (fn () =>
            let
                val ea = #msg (#role_A ea) ()
                val ea = (Event.sync ea) {role_C={msg=(fn ((),ea) => ea)}}
                val () = close ea
            in
                ()
            end) ()
end

(* (:=) operator is bogus *)
fun assign (r,v) = (fn asgn => fn v' => asgn (r,v')) := v

structure EQ :> sig
              type ('a,'b) eq
              val refl : unit -> ('a,'a) eq
              val symm : ('a,'b) eq -> ('b,'a) eq
              val apply_eq : (('a,'b) eq * 'a) -> 'b
          end = struct
datatype ('a,'b) eq = Refl of 'a option ref * 'b option ref

fun refl () = let val r = ref NONE in Refl (r,r) end

fun symm (Refl (x,y)) = Refl (y,x)

fun apply_eq ((Refl (rx,ry)), x) =
    (assign (rx, (SOME x));
     case !ry of
	 SOME y => (assign (rx, NONE); y)
      |  _ => raise (Fail "Impossible"))
end

datatype 'ts seq = Cons of 'ts | Nil
