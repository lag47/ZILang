type 'a t =
  | Solution of 'a
  | Nosol of string

let return a =
  Solution a

let bind a f =
  match a with
  | Solution s -> f s
  | Nosol _ -> a

let map a f =
  match a with
  | Solution s -> Solution (f s)
  | Nosol s -> Nosol s

let _ = (map:'a t -> ('a -> 'b) -> 'b t)

let join at =
  match at with
  | Solution (Solution s) -> Solution s
  | Solution (Nosol s) -> Nosol s
  | Nosol s -> Nosol s

let (>>=) = bind
let (>>|) = map

let bez a1 b1 =
  let rec gcd a b =
    if b=0
    then (a,1,0)
    else
      let (q,r0)= (a/b,a mod b) in
      let (g,x,y)= gcd b r0 in
      (g, y , x - q * y )
  in
  if a1 >= b1
  then gcd a1 b1
  else (fun (x,y,z)->(x,z,y)) (gcd b1 a1)

let gcd a b =
  bez a b |> fun (x,_,_) -> x

let solvemod a m b=
  let (g,x,_)= bez a m in
  if b mod g = 0
  then let m' = m / g in
    Solution ((x * b / g) mod m', m')
  else Nosol "No Solution"

let invmod  a m =
  solvemod a m 1


let bin_chin a1 a2 n1 n2 =
  solvemod n1 n2 (a2 - a1) >>|
  fun (kc, n2') ->
  (n1 * kc + a1, n1 * n2')

let chin_rem pairs =
  let f = (fun (a1,n1) (a2,n2) -> bin_chin a1 a2 n1 n2) in
  match pairs with
  | (a1,n1)::t -> List.fold_left (fun acc x -> acc >>= f x) (Solution (a1,n1)) t
  | [] -> failwith "empty list"
