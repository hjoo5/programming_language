let uniq list =
  let resultList = [] in
  let rec memCheck listA listB =
    (match listA with
    | [] -> listB
    | hd::tl -> 
      if not(List.mem hd listB) 
        then  memCheck tl (listB@[hd])
        else memCheck tl listB)
  in match list with
    | [] -> resultList
    | _ -> memCheck list resultList
  

  

