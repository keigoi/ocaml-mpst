type ('a, 'b) either = Left of 'a | Right of 'b

type ('v1, 'v2, 's1, 's2) lens = {
    get : 's1 -> 'v1;
    put : 's1 -> 'v2 -> 's2;
  }

let swap (k1,k2) = k2, k1
