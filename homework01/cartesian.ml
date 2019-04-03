
let rec cartesian listA listB =
  match listA with 
  | [] -> []
  | ahd::atl -> 
    match listB with 
    | [] -> []
    | bhd::btl -> (ahd,bhd)::[]@ cartesian listA btl@cartesian atl listB