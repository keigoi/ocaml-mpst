include Mpst_base

module Make(F:S.FLAG)(L:S.LIN) = struct
  module Session = Session.Make(F)
  module Global = Global.Make(F)(L)
  module Util = Util.Make(F)(L)
end

module Lin = struct
  module F = Flags.NoFlag
  module L = struct type 'a lin = 'a Mpst_base.LinMonad.Syntax.lin_ let mklin x = {Mpst_base.LinMonad.Syntax.__lindata=x} end
  module BareSession = Session.Make(F)
  module Global = Global.Make(F)(L)
  module Util = Util.Make(F)(L)
  module LinMonad = Mpst_base.LinMonad

  module Session = struct
    include LinSession.Make(BareSession)(Global)
    open BareSession
    open Global
    open Mpst_base.LinMonad

  let lv = Lazy.from_val

  let fork : 'a 'pre 'post.
    ('a, empty, 'pre, 'post) lens ->
    (unit -> (('a * unit) slots_, (empty * unit) slots_, unit data) monad) ->
    ('pre lazy_t, 'post lazy_t, unit data) monad =
    fun lens m ->
    {__run=
       fun pre ->
       Lwt.async (fun () ->
           (m ()).__run (lv (Cons (lens_get lens pre, lv Nil))));
       Lwt.return (lens_put_ lens pre Empty, {data=()})
    }
    
  let unone :
        type s pre post.
             (s sess one e lin, empty, pre, post) lens ->
             (pre lazy_t, post lazy_t, s sess lin) monad = fun lens ->
    {__run=
       fun pre ->
       match lens_get_ lens pre with
       | {__lindata=One (lazy s)} ->
          Lwt.return (lens_put_ lens pre Empty, {__lindata=s})
    }
  end

end

           
module NoDynCheck = struct
  module L = struct type 'a lin = 'a let mklin x = x end
  include Make(Flags.NoFlag)(L)
  module Lin = LinSession.Make(Session)(Global)
end
module L = struct type 'a lin = 'a let mklin x = x end                  
include Make(Flags.NanoMutexFlag)(L)
