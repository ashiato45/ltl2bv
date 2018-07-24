open Core

type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]


let () =
  print_endline "Hello, world!";
  let x =  FNot (FAtomic 2)  in
  let y = FAnd (x, (FAtomic 3)) in
  let z = 0 in
  print_endline (x |> [%sexp_of: formula] |> Sexp.to_string);
  print_endline (y |> [%sexp_of: formula] |> Sexp.to_string);  
      
