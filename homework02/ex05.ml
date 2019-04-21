type relationships = (string * string) list

let rec likes : relationships -> string -> 'a list =
  fun rel per ->
  let listOfName = (first_list rel per []) in
  (find_second rel listOfName listOfName)

and first_list relation person listName=
  if  (compare (try (List.assoc person relation)  with Not_found -> "None") "None") = 0
  then listName
  else 
    if  (List.mem (List.assoc person relation) listName)= false
    then
      let listName' = listName@[(List.assoc person relation)] in
      let relation' = (List.remove_assoc person relation) in
      first_list relation' person listName' 
    else 
      let relation' = (List.remove_assoc person relation) in
      (first_list relation' person listName)
and find_second relation personList listName = 
  match personList with
  | [] -> listName
  | hd::tl -> 
    let listName' = (first_list relation hd listName) in 
      if (List.length listName') != List.length listName
      then (find_second relation listName' listName')
      else (find_second relation tl listName')
    





let rec selflove : relationships -> int =
  fun rel -> 
  let startPointList = makeStartPointList rel []
  in  List.length (countSelfLove rel startPointList [] )

and makeStartPointList rel li = 
  match rel with 

  | hd::tl -> if (List.mem (fst hd) li)
    then (makeStartPointList tl li)
    else let li' = li@[(fst hd)] in (makeStartPointList tl li')
  | [] -> li
and countSelfLove  graph rel count =
    match rel with
    
    | hd::tl -> 
      if (List.mem hd (likes graph hd)) = true
      then let count' = count@[hd] in ( countSelfLove graph tl count')
      else (countSelfLove graph tl count)
    | [] ->  count
    
;;


