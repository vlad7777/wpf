(* Zadanie przelewanka *) 
(* Autor: Uladzislau Sobal 374078 *)
(* Code-review: Rastislau Matusievich *)

(* Modul kolejka *)
module Queue =
struct
    
    exception EmptyQueueException

    type 'a queue = 'a list * 'a list

    let rearrange q = 
    let (a, b) = q in (List.rev b), a 

    let rec pop q = 
        match q with
        | [], [] -> raise EmptyQueueException
        | [], l -> let k = rearrange q in pop k
        | h::t, l -> t, l

    let rec front q = 
        match q with
        | [], _ -> front (rearrange q)
        | h::t, _ -> h

    let push q x =
        let l1, l2 = q in l1, x::l2

    let empty = [], []

    let is_empty q = 
        match q with
        | [], [] -> true
        | _ -> false

end;;

(* Modul tablica mieszająca *)
module HashTable =
struct 

    let prime = 113 
    let modulo = 100003

    let hash a = 
        let q = ref 1 in
        let res = ref 0 in
        Array.iter (fun x -> res := !res + !q * x; res := !res mod modulo; q := !q * prime; q := !q mod modulo;) a;
        !res

    let empty () = Array.make modulo ([] : int array list)

    let insert ht a = 
        let h = hash a in
        ht.(h) <- a::ht.(h);
        ()

    let contains ht a = 
        let h = hash a in
        let res = ref false in
        List.iter (fun x -> if x = a then res := true) ht.(h);
        !res 
        
end;;

(* Funkcja getAdjacentStates *)
(* Przyjmuje: a - stan szklanek (tablica z ilością wody w każdej szklance) *)
(*            c - tablica z pojmnością każdej szklanki *)
(* Zwraca: liste osiągalnych stanów z bieżącego stanu a *)
let getAdjacentStates a c = 
    let res = ref [] in
    for i = 0 to Array.length a - 1 do 
        (* Wylewamy wodę *)
        if a.(i) <> 0 then begin
            let b = Array.copy a in
            b.(i) <- 0;
            res := b::!res;
        end;
        (* Nalewamy wodę *)
        if a.(i) <> c.(i) then begin
            let b = Array.copy a in
            b.(i) <- c.(i);
            res := b::!res;
        end;
        (* Przelewamy z jednej szklanki do drugiej *)
        for j = 0 to Array.length a - 1 do
            if i <> j then begin 
                let t = min (c.(j) - a.(j)) a.(i) in 
                if t <> 0 then begin
                    let b = Array.copy a in
                    b.(i) <- a.(i) - t;
                    b.(j) <- a.(j) + t;
                    res := b::!res;
                end;
            end;
        done;
    done;
    !res;;

(* wyjątki:
    * found int - rzucamy kiedy uzyskamy potrzebne wartośći
    * NoSolution - rzucamy kiedy od razu widać, że nie da się otrzymać
    *               potrzebnych wartośći *)
exception Found of int;;
exception NoSolution;;

(* Funkcja gcd
 * przyjmuje: a b - ints
 * zwraca: NWD *)
let rec gcd a b = 
    if a = 0 then b 
    else if a > b then
        gcd b a 
    else 
        gcd (b mod a) a;;

(* Funkcja przelewaka
 * przyjmuje: tablica par (pojemnośc, potrzebna wartość)
 * zwraca: ilośc operacji potrzebnych dla osiągania potrzebnych wartości *)
let przelewanka a = 
    (* sprawdzamy przypadki brzegowe które nie mogą mieć rozwiązania *)
    try
        if Array.length a = 0 then raise (Found 0);
        let g = ref 0 in
        let b = ref false in
        Array.iter (fun (x, y) -> 
            if x < y then raise NoSolution;
            if !g = 0 && x <> 0 then g := x else g := gcd !g x;
            if y = 0 || y = x then b := true;
        ) a;
        if not !b then raise NoSolution; 
        if !g <> 0 then
            Array.iter (fun (_, y) ->
                if y mod !g <> 0 then raise NoSolution;
            ) a;

        let start = Array.make (Array.length a) 0 in
        let capacity = Array.map (fun (x, y) -> x) a in
        let finish = Array.map (fun (x, y) -> y) a in
        let ht = HashTable.empty () in
        let q = ref (Queue.push Queue.empty (start, 0)) in
        (* BFS *)
        while not (Queue.is_empty !q) do
            let (cur, d) = Queue.front !q in
            if cur = finish then 
                (* Jeśli uzyskaliśmy potrzebną wartość rzucamy wyjątek *)
                raise (Found d);
            q := Queue.pop !q;
            let adjacent = getAdjacentStates cur capacity in
            List.iter (fun x -> if not (HashTable.contains ht x) then begin
                                    q := Queue.push !q (x, d + 1); HashTable.insert ht x; 
                                end;) adjacent;
        done;
    -1
    with 
        | Found(d) -> d
        | _ -> -1;;


(* TESTY *)
(*
(* Queue testy *)

let q = Queue.empty in
let q = Queue.push q 1 in
assert (Queue.front q = 1);;

let q = Queue.empty in
let q = Queue.push q 1 in
let q = Queue.pop q in
assert (Queue.is_empty q);;

(* getAdjacentStates testy *)

let a = [|0; 0; 0|] in
let c = [|1; 0; 0|] in
let s = getAdjacentStates a c in
assert (s = [[|1; 0; 0|]]);;

(* HastTable testy *)

let h = HashTable.empty ();;
assert (not (HashTable.contains h [|1; 2; 3;|]));
HashTable.insert h [|1|];
HashTable.insert h [|1; 2; 3|];
assert (HashTable.contains h [|1|]);
assert (not (HashTable.contains h [|2|]));;

(* przelewanka testy *)

let test k a ans =
    let r = przelewanka a in
    if ans <> r then
        Printf.printf "Wrong answer to test #%d\nExpected %d, but got %d instead.\n" k ans r;;

test 1 [|(1, 1); (2, 2); (3, 3)|] 3;;
test 2 [|(1, 1); (2, 2); (3, 0)|] 2;;
test 3 [|(1, 0); (2, 0); (3, 0)|] 0;;
test 4 [|(1, 0); (2, 1); (3, 2)|] 3;; 
(*test 5 [|(1, 1); (3, 1); (3, 1); (5, 1); (9, 1); (4, 1); (4, 1); (3, 1); |] 11;;*)
test 6 [|(4, 3); (4, 3);|] (-1);;
test 7 [|(4, 2); (4, 2);|] (-1);;
test 8 [|(5, 0); (3, 2);|] (4);;
test 9 [|(154, 23); (23, 15); (15, 13); (13, 0);|] (6);;
test 10 [|(93, 11);|] (-1);;
test 11 [|(99, 98); (1, 1);|] 2;;
test 12 [|(100, 50); (1, 1);|] 100;;
test 13 [|(7, 6); (6, 5); (5, 4); (4, 3); (3, 2); (2, 1); (1, 0);|] 11;;
test 14 [|(11, 12)|] (-1);;
test 15 [||] (0);; *)
