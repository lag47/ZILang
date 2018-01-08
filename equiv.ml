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
exception No_solution

let solvemod a m b=
  let (g,x,_)= bez a b in
  if m mod g = 0
  then x*m/g
  else raise No_solution

let invmod  a m =
  solvemod a 1 m

let bin_chin a1 a2 n1 n2 =
  let n1inv= invmod n1 n2 in
  let x = (n1*n1inv*(a2-a1)+a1) mod (n1*n2)
  in if x<0 then x + (n1 * n2) else x

let chin_rem ak nk =
  let pairs = List.combine ak nk in
  match pairs with
  | (a1,n1)::t -> List.fold_left
                    (fun (ai,ni) (ai1,ni1) ->  (bin_chin ai ai1 ni ni1),ni*ni1)
                    (a1,n1) t
  | [] -> failwith "Empty List"
