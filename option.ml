let return a = Some a

let bind a_opt f =
  match a_opt with
  | Some a -> f a
  | None -> None

let map a_opt f =
  match a_opt with
  | Some a -> Some (f a)
  | None -> None

let join a_opt_opt =
  match a_opt_opt with
  | Some (Some a) -> Some a
  | _ -> None

let extract d opt =
  match opt with
  | Some a -> a
  | None -> d

let (>>=) = bind

let (>>|) = map
