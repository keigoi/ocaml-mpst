type 'a t = 'a link ref
and 'a link = Link of 'a t | Val of 'a

let rec path_compression n =
  match !n with
  | Val _ -> n
  | Link n1 ->
      let n1' = path_compression n1 in
      n := Link n1';
      n1

let rec unify0 : type a. a t -> a t -> unit =
 fun c1 c2 ->
  match (c1, c2) with
  | _, { contents = Link c2' } -> unify0 c1 c2'
  | { contents = Link c1' }, _ -> unify0 c1' c2
  | _ -> if c1 == c2 then () else c2 := Link c1

let unify c1 c2 =
  unify0 c1 c2;
  ignore @@ path_compression c1;
  ignore @@ path_compression c2

let make v = ref @@ Val v
let rec get n = match !n with Val ch -> ch | Link n -> get n
