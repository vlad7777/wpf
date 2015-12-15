(* Zadanie Modyfikacja drzew AVL *)
(* Autor: Uladzislau Sobal 374078 *)
(* Code-review: Tomasz Gąsior 370797*)

(* Typ wariantowy drzewo 
 * składa się z:
 * t - lewego poddrzewa
 * (int, int) - przedziała
 * t - prawego poddrzewa 
 * int - wysokości drzewa
 * int - ilości elementow w przedziałach drzewa *)
type t = Null | Node of t * (int * int) * t * int * int

let empty = Null

(* Funkcja height:
*   bierze: drzewo
*   zwraca: wysokość drzewa, 0 dla pustego *)
let height = function
    | Node (_, _, _, h, _) -> h
    | Null -> 0

(* Funkcja height:
    * bierze: drzewo
    * zwraca: ilość elementów w przedziałach drzewa *)
let sum = function
    | Node (_, _, _, _, s) -> s
    | Null -> 0

(* Funcjca ++
 * bierze: dwie liczby
 * zwraca: a + b gdy a + b < max_int, inaczej max_int  *)
let (++) a b = 
    let sum = a + b in
    if sum < b then
        max_int
    else sum

(* Funkcja make:
    * bierze: dwa poddrzewa l, r i przedział (lo, hi)
    *           l < (lo, hi) < r
    * zwraca: drzewo składające się z l (lo, hi) r. 
    * Wynikowe drzewo ma obliczone sumę i wysokość *)
let make l (lo, hi) r = 
    let nHeight = max (height l) (height r) + 1
    and nSum = (sum l) ++ (sum r) ++ (hi) ++ (-lo) ++ (1) in
    Node (l, (lo, hi), r, nHeight, nSum)

(* Funkca bal:
    * bierze: dwa poddrzewa l, r i przedział k
    * zwraca: zbalansowane drzewo, składające się z l, r, k *)
let bal l k r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
    else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Null -> assert false)
    | Null -> assert false
    else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Null -> assert false)
    | Null -> assert false
    else make l k r 

(* Funkcja cmp
 * bierze: dwa przedziały
 * zwraca:  *)
let cmp (lo1, hi1) (lo2, hi2) = 
    if hi1 < lo2 then -1
    else if lo1 > hi2 then 1
    else 0

(* Funkcja add_sep
 * bierze: przedział x i drzewo, przy założeniu że x jest rozłączny ze wszystkimi przedziałami drzewa
 * zwraca: drzewo z przypisanym przedziałem x *)
let rec add_sep x = function
  | Node (l, k, r, _, _) ->
      let c = cmp x k in
      let () = assert( c != 0 ) in 
      if c < 0 then
        let nl = add_sep x l in
        bal nl k r
      else
        let nr = add_sep x r in
        bal l k nr
  | Null -> make Null x Null 

(* Funkcja join
 * bierze: przedział v i dwa poddrzewa l r, l < x < r
 * zwraca: zbalansowane drzewo, składające się z l, v, r *)
let rec join l v r =
  match (l, r) with
    (Null, _) -> add_sep v r
  | (_, Null) -> add_sep v l
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r) else
      if rh > lh + 2 then bal (join l v rl) rv rr else
      make l v r

(* Funkcja divide
 * bierze: drzewo t i przedział (lo1, hi1)
 * zwraca: 2 drzewa a, b takie że, a < (lo1, hi1), b > (lo1, hi1) *)
let rec divide t (lo1, hi1) = 
    match t with
    | Null -> Null, Null
    | Node(l, (lo, hi), r, h, _) ->  
            (* odcinki nie przecinają sie *)
            if lo > hi1 then
                let (less, greater) = divide l (lo1, hi1) in
                less, join greater (lo, hi) r 
            else if hi < lo1 then
                let (less, greater) = divide r (lo1, hi1) in
                join l (lo, hi) less, greater
            else
                (* odcinki się przecinają *)
                if lo < lo1 then
                    if hi1 >= hi then
                        let (less, greater) = divide r (lo1, hi1) in
                        join l (lo, lo1 - 1) less, greater 
                    else
                        join l (lo, lo1 - 1) Null, join Null (hi1 + 1, hi) r
                else
                    if hi1 >= hi then
                        let (less, _) = divide l (lo1, hi1) and
                        (_, greater) = divide r (lo1, hi1) in
                        less, greater
                    else
                        let (less, greater) = divide l (lo1, hi1) in
                        less, join greater (hi1 + 1, hi) r 

(* Funkcja min_elt 
 * bierze: drzewo
 * zwraca: minimalny przedział tego drzewa *)
let rec min_elt = function
    | Node (Null, (lo, hi), _, _, _) -> lo, hi 
    | Node (l, _, _, _, _) -> min_elt l
    | Null -> raise Not_found

(* Funkcja remove_min_elt
 * bierze: drzewo
 * zwraca: drzewo z usuniętym minimalnym przedziałem *)
let rec remove_min_elt = function
  | Node (Null, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> join (remove_min_elt l) k r
  | Null -> invalid_arg "PSet.remove_min_elt"

(* Funkcja max_elt
 * bierze: drzewo
 * zwraca: maksymalny przedział tego drzewa *)
let rec max_elt = function
    | Node (_, (lo, hi), Null, _, _) -> lo, hi 
    | Node (_, _, r, _, _) -> max_elt r
    | Null -> raise Not_found

(* Funkcja merge
 * bierze: dwa zbalansowanych t1 t2
 * zwraca: zbalansowane drzewo skladające się z t1 t2 *)
let merge t1 t2 =
    match t1, t2 with
    | Null, _ -> t2
    | _, Null-> t1
    | _ ->
        let k = min_elt t2 in
        join t1 k (remove_min_elt t2)

(* Funkcja add
 * bierze: przedział i drzewo t
 * zwraca: drzewo, zawierające wszystkie elementy od lo do hi *)
let add (lo, hi) t =
    let less, greater = divide t (lo, hi)
    and cur = (lo, hi)
    in let (mxl, mxr) = try max_elt less with Not_found -> (1, -1)
    and (mnl, mnr) = try min_elt greater with Not_found -> (1, -1)
    in 
        if mxl <= mxr && mxr = lo - 1 && mnl <= mnr && mnl = hi + 1 then
            let cur = (mxl, mnr) 
            and less, _ = divide less (mxl, mxr)
            and _, greater = divide greater (mnl, mnr)
            in join less cur greater
        else if mxl <= mxr && mxr = lo - 1 then
            let cur = (mxl, hi) 
            and less, _ = divide less (mxl, mxr)
            in join less cur greater
        else if mnl <= mnr && mnl = hi + 1 then
            let cur = (lo, mnr) 
            and _, greater = divide greater (mnl, mnr)
            in join less cur greater
        else
            join less cur greater

(* Funkcja remove
 * bierze: przedział (lo, hi) i drzewo t
 * zwraca: drzewo t bez elementów od lo do hi *)
let remove (lo, hi) t =
    let less, greater = divide t (lo, hi) in    
        merge less greater

(* Funkcja is_empty
 * bierze: drzewo t
 * zwraca: true jeśli drzewo t jest puste, inaczej false*)
let is_empty t =
    t = Null

(* Funkcja iter
 * bierze: funkcje f i t
 * zwraca: unit, zanim wywołuje funkcję iter dla każdego przedzuału drzewa *)
let rec iter f t = 
    match t with
    | Null -> ()
    | Node(l, v, r, _, _) -> 
            let () = iter f l 
        and () = f v 
        and () = iter f r in () 

(* Funkcja fold
 * bierze: funkcje, drzewo, akumulator
 * zwraca: wynik funkcji f po kolei wywołanej dla każdego przedziału *)
let rec fold f t a = 
    match t with 
    | Null -> a
    | Node(l, v, r, _, _) -> fold f r (f v (fold f l a))

(* Funkcja mem
 * bierze: int x oraz drzewo t
 * zwraca: true jeśli drzewo zawiera element t, inaczej false *)
let rec mem x t = 
    match t with 
    | Null -> false
    | Node(l, (lo, hi), r, _, _) -> 
            if x < lo then  
                mem x l
            else if x > hi then
                mem x r
            else
                true

(* Funkcja elements
 * bierze: drzewo t
 * zwraca: listę przedziałów t *)
let elements t = 
    List.rev (fold (fun e l -> e::l) t [])

(* Funkcja split
 * bierze: int x i drzewo t
 * zwraca: krotkę (l, b, r) - l - drzewo przedziałów mniejszych x,
 *                            b - true jeśli t zawiera x, inaczej false
 *                            r - drzewo przedziałów większych x *)
let split (x : int) (tr : t) =
    let b = mem x tr in
    let (l, r) = divide tr (x, x) in
        l, b, r    

(* Funkcja below
 * bierze: int x drzewo t
 * zwraca: int - ilość elementów drzewa t mniejszych lub równych x *)
let  below x t =
    let rec help x t ac = 
        match t with 
        | Null -> ac 
        | Node(l, (lo, hi), r, _, _) -> 
                if x < lo then  
                    help x l ac
                else if x > hi then
                    help x r (ac ++ sum l ++ (hi) ++ (-lo) ++ (1))
                else
                    ac ++ sum l ++ (x) ++ (-lo) ++ (1) 
    in help x t 0


(* TESTY *)

let fillArray k = 
    let rec fill l k = if k = 0 then l
                else (fill ((k * 2) :: l) (k - 1)) in
    fill [] k

let rec is_avl k = 
    match k with
    | Null -> true
    | Node(l, _, r, _, _) -> abs(height l - height r) <= 2 && is_avl l && is_avl r

let tr = List.fold_left (fun t x -> add (x, x) t) empty (fillArray 10000)
let () = assert (is_avl tr)
let tr = empty
let tr = add (2, 5) tr
let () = assert (below 5 tr = 4)
let () = assert (below 0 tr = 0)
let tr = add (4, 10) tr
let () = assert (below 6 tr = 5)
let tr = remove (2, 3) tr
let () = assert (below 6 tr = 3)
let () = assert (is_avl tr)
let tr = add (-max_int, max_int) tr
let () = assert (below 0 tr = max_int)
let tr = remove (-max_int + 1, max_int - 1) tr
let h::t = elements tr
let () = assert ( h = (-max_int, -max_int) ) 
let () = assert ( is_avl tr ) 
