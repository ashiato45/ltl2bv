open Core
open Mylib





let () =
  print_endline "Hello, world!";
  let x =  Formula.FNot (Formula.FAtomic 2)  in
  let y = Formula.FAnd (x, (Formula.FAtomic 3)) in
  let z = 0 in
  print_endline (x |> [%sexp_of: Formula.formula] |> Sexp.to_string);
  print_endline (y |> [%sexp_of: Formula.formula] |> Sexp.to_string);  
      
