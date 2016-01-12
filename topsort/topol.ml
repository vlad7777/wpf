(* Zadanie : sortowanie topologiczne *)
(* Autor: Uladzislau Sobal 374078 *)
(* Code-review: Aliaksei Suvorau 374118 *)

exception Cykliczne

let topol l = 
    let res = ref ([]) in
    let isvisited = ref PMap.empty in
    let mp = ref PMap.empty in 
    let _ = List.mapi (fun i e -> 
                            let (el, li) = e in 
                            mp := PMap.add el li !mp;) l 
    (* Funkcja dfs - buduje listę res *)
    in let rec dfs v = 
        (* gdy isvisited zawiera element (e, false), to znaczy że my zaczęliśmy przeszukiwać e, ale jeszcze nie skonczyliśmy *)
        isvisited := PMap.add v (false) !isvisited;
        List.iter (fun e ->
            (* jeśli isvisited zawiera element (e, false) rzucamy wyjątek Cykliczne *)
            let (f, s) = if (PMap.mem e !isvisited) then (true, PMap.find e !isvisited) else (false, false) in
            if f && not s then raise Cykliczne else
            if not f && not s then dfs e ) (PMap.find v !mp);
        (* jeśli isvisited zawiera (e, true), to znaczy, że już skończyliśmy przeszukiwać e *)
        isvisited := PMap.add v (true) !isvisited;
        res := v::(!res)
    in
    List.iter (fun (k, _) -> if not (PMap.mem k !isvisited) then dfs k) l;
    !res

(* TESTY *)

(* funkcja, która sprawdza sortowanie res dla listy input *)
(* zwraca true gdy sortowanie jest poprawne *)
let sprawdz input res = 
    let verdict = ref true in
    let mp = ref PMap.empty in
    List.iteri (fun i x -> mp := PMap.add x i !mp) res;
    List.iter (fun (e, l) -> List.iter (fun k -> if PMap.find e !mp > PMap.find k !mp then verdict := false) l ) input;
    !verdict 

let test input = 
    assert (sprawdz input (topol input))
    
let _ = test [(1, [2; 3]); (2, []); (3, [2]); (4, [2; 3])]
let _ = test [(1, [2; 3; 4; 5]); (2, [3; 4; 5]); (3, [4; 5]); (4, [5]); (5, [])]
let l = [(1, [2; 3; 4; 5]); (2, [3; 4; 5]); (3, [4; 5]); (4, [5]); (5, [4])]
let e = ref false
let _ = try topol l with Cykliczne -> e := true;[]
let _ = assert (!e)
let _ = test [(1, [2]); (2, []); (3, [4]); (4, []); (5, [6]); (6, [])]
let _ = test [("winter", ["summer"; "spring"]); ("spring", ["summer"]); ("summer", ["autumn"]); ("autumn", [])] 
let _ = test [(1, [2; 3]); (2, [4]); (3, [4; 5]); (4, []); (5, [])]
