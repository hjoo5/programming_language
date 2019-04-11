
let rec cartesian listA listB =
  match listA with 
  | [] -> []
  | ahd::atl -> 
    match listB with 
    | [] -> []
    | bhd::btl -> (ahd,bhd)::[]@ cartesian [ahd] btl@cartesian atl listB
;;