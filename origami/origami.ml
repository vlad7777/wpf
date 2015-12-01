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
        fun p -> let x, y = p and xc, yc = pc in if ((x -. xc) ** 2.) +. ((y -. yc) ** 2.) <= (r ** 2.) then 1 else 0;;

(* stała eps - maksymalne odchylenie w obliczeniach *)
let eps = 1e-7

(* oblicz prostą
 * bierze: dwa punkty
 * zwraca: trójkę wartośći - (a, b, c) - wspólczynniki 
 * prostej Ax + By + C = 0 do której nalęzą punkty p1 p2 *)
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

(*
let () = assert (abs_float (equation (1.0, 1.0) (2.0, 2.0) (3.0, 3.0)) < eps)
let () = assert (abs_float ((equation (1.0, 1.0) (2.0, 2.0) (2.0, 0.0)) -. -1.) < eps)
let () = assert (abs_float ((equation (2.0, 2.0) (1.0, 1.0) (2.0, 0.0)) -. 1.) < eps)
let () = assert (abs_float ((equation (1.0, 1.0) (2.0, 2.0) (2.0, 5.0)) -. 1.) < eps) *)

let zle = ref 0
let test n b =
  if not b then begin
    Printf.printf "Zly wynik testu %d!!\n" n;
    incr zle
  end

let k1 = prostokat (1.,1.) (6.,4.);;

test 1 (k1 (1.,1.) = 1);;
test 2 (k1 (6.,4.) = 1);;
test 3 (k1 (2.,3.) = 1);;
test 4 (k1 (1.,4.) = 1);;
test 5 (k1 (7.,1.) = 0);;
test 6 (k1 (3.,5.) = 0);;

let k2 = zloz (4.,-2.) (4.,6.) k1;;

test 7 (k2 (1.,1.) = 1);;
test 8 (k2 (1.,4.) = 1);;
test 9 (k2 (4.,2.) = 1);;
test 10 (k2 (3.,1.) = 2);;
test 11 (k2 (3.,2.) = 2);;
test 12 (k2 (3.,4.) = 2);;
test 13 (k2 (2.,1.) = 2);;
test 14 (k2 (2.,2.) = 2);;
test 15 (k2 (5.,2.) = 0);;
test 16 (k2 (5.,4.) = 0);;
test 17 (k2 (2.,3.) = 2);;

let k2 = skladaj [((4.,-2.), (4.,6.))] k1;;

test 18 (k2 (1.,1.) = 1);;
test 19 (k2 (1.,4.) = 1);;
test 20 (k2 (4.,2.) = 1);;
test 21 (k2 (3.,1.) = 2);;
test 22 (k2 (3.,2.) = 2);;
test 23 (k2 (3.,4.) = 2);;
test 24 (k2 (2.,1.) = 2);;
test 25 (k2 (2.,2.) = 2);;
test 26 (k2 (5.,2.) = 0);;
test 27 (k2 (5.,4.) = 0);;
test 28 (k2 (2.,3.) = 2);;

let k2 = zloz (4.,6.) (4.,-2.) k1;;

test 29 (k2 (1.,1.) = 0);;
test 30 (k2 (1.,4.) = 0);;
test 31 (k2 (4.,2.) = 1);;
test 32 (k2 (3.,1.) = 0);;
test 33 (k2 (4.,4.) = 1);;
test 34 (k2 (2.,1.) = 0);;
test 35 (k2 (2.,2.) = 0);;
test 36 (k2 (5.,2.) = 2);;
test 37 (k2 (5.,4.) = 2);;
test 38 (k2 (2.,3.) = 0);;

let k2 = skladaj [((4.,6.), (4.,-2.))] k1;;

test 39 (k2 (1.,1.) = 0);;
test 40 (k2 (1.,4.) = 0);;
test 41 (k2 (4.,2.) = 1);;
test 42 (k2 (3.,1.) = 0);;
test 43 (k2 (4.,4.) = 1);;
test 44 (k2 (2.,1.) = 0);;
test 45 (k2 (2.,2.) = 0);;
test 46 (k2 (5.,2.) = 2);;
test 47 (k2 (5.,4.) = 2);;
test 48 (k2 (2.,3.) = 0);;

let k2 = skladaj [((4.,-2.), (4.,6.)); ((-1.,2.), (5.,2.))] k1;;

test 49 (k2 (1.,2.) = 1);;
test 50 (k2 (1.5,2.5) = 2);;
test 51 (k2 (2.,3.) = 4);;
test 52 (k2 (1.5,3.5) = 1);;
test 53 (k2 (2.5,2.5) = 4);;
test 54 (k2 (4.,2.) = 1);;
test 55 (k2 (3.5,2.5) = 4);;
test 56 (k2 (3.5,3.5) = 2);;
test 57 (k2 (3.,3.) = 4);;
test 58 (k2 (5.,3.) = 0);;
test 59 (k2 (1.,5.) = 0);;

let k2 = prostokat (-15.,-10.) (25.,15.);;

test 60 (k2 (0.,15.) = 1);;
test 61 (k2 (5.,5.) = 1);;
test 62 (k2 (-15.1,4.) = 0);;

let k2 = zloz (-5.,-10.) (25.,0.) k2;;

test 63 (k2 (0.,5.) = 1);;
test 64 (k2 (19.,8.) = 2);;
test 65 (k2 (10.,-5.) = 1);;
test 66 (k2 (15.,-5.) = 0);;
test 67 (k2 (15.,0.) = 2);;

let k2 = zloz (-20.,15.) (15.,-10.) k2;;

test 68 (k2 (-12.,14.) = 1);;
test 69 (k2 (-5.,10.) = 2);;
test 70 (k2 (7.,-3.) = 4);;
test 71 (k2 (-10.,6.) = 0);;
test 72 (k2 (8.,2.) = 3);;

let k2 = zloz (10.,-10.) (15.,20.) k2;;

test 73 (k2 (9.3,-4.7) = 2);;
test 74 (k2 (7.,-3.) = 4);;
test 75 (k2 (8.,2.) = 5);;
test 76 (k2 (8.,-1.) = 6);;
test 77 (k2 (4.,18.) = 1);;
test 78 (k2 (7.,1.) = 5);;
test 79 (k2 (4.,2.) = 4);;
test 80 (k2 (5.26,18.3) = 1);;
test 81 (k2 (8.51,8.8) = 4);;
test 82 (k2 (2.,18.) = 0);;

let k2 = prostokat (-15.,-10.) (25.,15.);;
let k2 = skladaj [((-5.,-10.), (25.,0.));((-20.,15.), (15.,-10.));((10.,-10.), (15.,20.))] k2;;

test 83 (k2 (9.3,-4.7) = 2);;
test 84 (k2 (7.,-3.) = 4);;
test 85 (k2 (8.,2.) = 5);;
test 86 (k2 (8.,-1.) = 6);;
test 87 (k2 (4.,18.) = 1);;
test 88 (k2 (7.,1.) = 5);;
test 89 (k2 (4.,2.) = 4);;
test 90 (k2 (5.26,18.3) = 1);;
test 91 (k2 (8.51,8.8) = 4);;
test 92 (k2 (2.,18.) = 0);;

let k2 = kolko (6.,4.) 14.;;

test 93 (k2 (6.,4.) = 1);;
test 94 (k2 (20.,4.) = 1);;
test 95 (k2 (20.,0.) = 0);;

let k2 = zloz (-2.,-10.) (20.,4.) k2;;

test 96 (k2 (6.,4.) = 1);;
test 97 (k2 (20.,4.) = 1);;
test 98 (k2 (20.,0.) = 0);;
test 99 (k2 (8.,1.) = 2);;
test 100 (k2 (9.3,4.48) = 1);;
test 101 (k2 (8.02,3.93) = 2);;
test 102 (k2 (8.83,-3.11) = 0);;

let k2 = zloz (25.,-5.) (5.,20.) k2;;

test 103 (k2 (14.34,3.96) = 3);;
test 104 (k2 (17.05356, 3.53971) = 4);;
test 105 (k2 (17.26873, 2.08641) = 2);;
test 106 (k2 (17.24603, 2.19642) = 1);;
test 107 (k2 (11.13925, 5.0046) = 3);;

let k2 = kolko (6.,4.) 14.;;
let k2 = skladaj [((-2.,-10.), (20.,4.));((25.,-5.), (5.,20.))] k2;;

test 108 (k2 (14.34,3.96) = 3);;
test 109 (k2 (17.05356, 3.53971) = 4);;
test 110 (k2 (17.26873, 2.08641) = 2);;
test 111 (k2 (17.24603, 2.19642) = 1);;
test 112 (k2 (11.13925, 5.0046) = 3);;



let _ = 
  if !zle = 0 then 
    Printf.printf "\nTesty OK!\n"
  else  
    Printf.printf "\nBlednych testow: %d...\n" !zle
;;
