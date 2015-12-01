(* Autor: Uladzislau Sobal 374078
 * Code-review: Karol Wołonciej *)

type point = (float * float)
type kartka = point -> int
type pozycja = Po_lewej | Na_prostej | Po_prawej

(* funkcja prostokąt
 * bierze: dwa punkty, należący do prostokąta
 * zwraca: kartkę, odpowiadającą temu prostokątowi *)
let prostokat p1 p2 = 
    fun p -> let x, y = p and x1, y1 = p1 and x2, y2 = p2 in
        if x >= min x1 x2 && x <= max x1 x2 && 
        y >= min y1 y2 && y <= max y1 y2 then 1
        else 0;;

(* funkcja kolko 
 * bierze: centrum kólka oraz promień 
 * zwraca: kartkę, odpowiadającą temu kołku *)
let kolko pc r =
    fun p -> let x, y = p and xc, yc = pc in 
        if ((x -. xc) ** 2.) +. ((y -. yc) ** 2.) <= (r ** 2.) then 1 else 0;;

(* stała eps - maksymalne odchylenie w obliczeniach *)
let eps = 1e-7

(* oblicz prostą
 * bierze: dwa punkty
 * zwraca: trójkę wartośći - (a, b, c) - wspólczynniki 
 * prostej Ax + By + C = 0 do której nalezą punkty p1 p2 *)
let oblicz_prosta p1 p2 = 
    let x1, y1 = p1 and x2, y2 = p2 in
        let a = y1 -. y2 and
            b = x2 -. x1 and
            c = x1 *. y2 -. y1 *. x2
        in (a, b, c)

(* oblicz_znak
 * bierze: trzy punkty
 * zwraca: 1 gdy p jest po lewej stronie od prostej (p1, p2) patrząc od p1,
 *         -1 gdy p jest po prawej stronie (p1, p2)
 *         0 gdy p należy do prostej (p1, p2) *)
let znajdz_strone p1 p2 p =
    let x, y = p and (a, b, c) = oblicz_prosta p1 p2 in
        let v = a *. x +. b *. y +. c in
        (* korzystam z właściwości Ax + By + C. ten wyraz
         * jest równy zero kiedy x, y leży na prostej
         * >0 gdy wyżej, niż prosta
         * <0 gdy niżej *)
        if abs_float (v) < eps then Na_prostej else if v < 0. then Po_prawej else Po_lewej 

(* odbicie 
 * bierze: trzy punkty
 * zwraca: punkty - odbicie p wzdłuż prostej (p1, p2) *)
let odbicie p1 p2 p =
    let x, y = p and (a, b, c) = oblicz_prosta p1 p2 in
    let xr = ((b *. b -. a *. a) *. x -. 2. *. a *. (b *. y +. c)) /. (a *. a +. b *. b)
    and yr = ((a *. a -. b *. b) *. y -. 2. *. b *. (a *. x +. c)) /. (a *. a +. b *. b) in
        (xr, yr);;

(* złóż 
 * bierze: dwa punkty oraz kartkę
 * zwraca: kartkę, ktorą odpowiada złożonej kartce *)
let zloz p1 p2 k = fun x -> 
    let v = znajdz_strone p1 p2 x in
        if v == Na_prostej then k x else
        if v == Po_lewej then k x + k (odbicie p1 p2 x) else
            0

(* składaj
 * bierze: listę par punktów oraz kartke
 * zwraca: kartkę, złożoną wzdłuż każdej prostej w liście *)
let skladaj l k = 
    List.fold_left (fun k (p1, p2) -> zloz p1 p2 k) k l

let () = assert ((znajdz_strone (1.0, 1.0) (2.0, 2.0) (3.0, 3.0)) = Na_prostej)
let () = assert ((znajdz_strone (1.0, 1.0) (2.0, 2.0) (100.0, 3.0)) = Po_prawej)
let () = assert ((znajdz_strone (1.0, 1.0) (2.0, 2.0) (3.0, 300.0)) = Po_lewej)
let () = assert ((znajdz_strone (2.0, 2.0) (1.0, 1.0) (3.0, 300.0)) = Po_prawej)

let k = prostokat (1.0, 1.0) (5., 5.)
let () = assert( k (0., 0.) = 0 )
let () = assert( k (1., 1.) = 1 )
let () = assert( k (5., 5.) = 1 )
let () = assert( k (2., 2.) = 1 )

let k = zloz (5., 4.) (4., 5.) k
let () = assert( k (4.1, 4.1) = 2 )
let () = assert( k (4.9, 4.9) = 0 )

let k = kolko (0., 0.) 5.
let () = assert( k (5., 0.) = 1 )
let () = assert( k (5., 5.) = 0 )
let k = zloz (4., 1.) (4., 2.) k
let () = assert( k (5., 0.) = 0 )
let () = assert( k (4., 0.) = 1 ) 
let () = assert( k (3.9, 0.) = 2 ) 
