let rec cartesian listA listB =
  match listA with 
  | [] ->[]
  | ahd::atl -> 
    match listB with 
    | [] ->[]
    | bhd::btl -> 