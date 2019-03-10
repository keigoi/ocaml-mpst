open Global
include Util

let a = {role=`A; lens=Fst}
let b = {role=`B; lens=Next Fst}
let c = {role=`C; lens=Next (Next Fst)}

let finish = one @@ one @@ one @@ nil
