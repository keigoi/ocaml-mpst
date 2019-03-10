open Session

type ('pre, 'post, 'a) monad = 'pre -> ('post * 'a) Lwt.t

let return v = (fun s -> Lwt.return (s, v))

let (>>=) m f = (fun s ->
    Lwt.bind (m s) (fun (mid, a) ->
    f a mid))

let __in f = f
let __run s f = f s

let lift m = (fun s -> Lwt.bind m (fun v -> Lwt.return (s, v)))

(* let put l v = (fun s -> Lwt.return (Lazy.force (lens_put_ l (Lazy.from_val s) (One v)), ())) *)
