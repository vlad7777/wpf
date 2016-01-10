exception Cykliczne

let topol l = 
    let res = ref ([]) in
    let isvisited = ref PMap.empty in
    let mp = ref PMap.empty in 
    let _ = List.mapi (fun i e -> 
                            (let (el, li) = e in 
                            mp := PMap.add el li !mp;
                            isvisited := PMap.add el (false, false) !isvisited;)
                    ) l 
    in let rec dfs v = 
        isvisited := PMap.add v (true, false) !isvisited;
        List.iter (fun e ->
            let (f, s) = PMap.find e !isvisited in
            if f && not s then raise Cykliczne else
            if not f && not s then dfs e ) (PMap.find v !mp);
        isvisited := PMap.add v (true, true) !isvisited;
        res := v::(!res)
    in
    List.iter (fun (k, _) -> if PMap.find k !isvisited = (false, false) then dfs k) l;
    !res

let l = [(1, [2; 3]); (2, []); (3, [2]); (4, [2; 3])]
let r = topoldfs l
let _ = List.iter (fun x -> Printf.printf "%d " x) r
