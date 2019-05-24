let f := proc (x) (x := 3) in

let g := proc (x) (x := 3) in

let x := 1 in

let y := 2 in
f <x>;print(x); g (y);
print (x + y);
print(x);
print(y);

