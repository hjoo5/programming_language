let rec gcd n m = 
  if (n mod m) = 0 then m else gcd m (n mod m);;