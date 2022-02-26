type 'a name_ = Name of 'a Event.channel | Link of 'a name
and 'a name = 'a name_ ref

type 'a endpoint = 'a Event.channel

let new_name () = ref @@ Name (Event.new_channel ())

let rec unify (n1 : 'a name) (n2 : 'a name) =
  if n1 == n2 then ()
  else
    match (!n1, !n2) with
    | Name _, Name _ -> n2 := Link n1
    | Link n1, _ -> unify n1 n2
    | _, Link n2 -> unify n1 n2

let rec finalise n1 =
  match !n1 with
  | Name ch -> ch
  | Link n1' ->
      let ch = finalise n1' in
      n1 := Name ch;
      ch
