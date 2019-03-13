exception InvalidEndpoint
exception RoleNotEnabled
exception ReceiveFail

let repeat num f =
  let r = ref [] in
  for i=0 to num-1
  do
    r := (f i)::!r
  done;
  !r
            
let rec transpose : 'a list list -> 'a list list = fun xss ->
  match xss with
  | [] -> []
  | []::_ -> []
  | xss ->
     let hds, tls =
       List.map (fun xs -> List.hd xs, List.tl xs) xss |> List.split
     in
     hds :: transpose tls

let fork f =
  if Unix.fork () = 0 then begin
      (f () : unit);
      exit 0
    end else
    ()
