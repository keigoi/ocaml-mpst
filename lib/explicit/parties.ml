open Session
open Global

let a = {role=`A; lens=FstProt}
let b = {role=`B; lens=Next FstProt}
let c = {role=`C; lens=Next (Next FstProt)}
      
let lv = Lazy.from_val

let nil  = lv Nil
           
let one tl = lv (ConsProt(lv Close, tl))

let many cnt tl =
  let rec make i x = if i=0 then [] else x::make (i-1) x
  in
  lv (ConsList(lv (make cnt Close), tl))

let get_sess r m =
  let p = lens_get_ r.lens m in
  Sess((lv Memory, lv Memory, lv Memory), p)
