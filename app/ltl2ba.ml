open Core
open Mylib





let () =
  (* let x =  Formula.FNot (Formula.FAtomic 2)  in
   * let y = Formula.FAnd (x, (Formula.FAtomic 3)) in
   * let z = 0 in
   * (\* print_endline (x |> [%sexp_of: Formula.formula] |> Sexp.to_string);
   *  * print_endline (y |> [%sexp_of: Formula.formula] |> Sexp.to_string); *\) *)
  let open Formula in
  let f_or a b = FNot (FAnd (FNot a, FNot b)) in
  let f_true = f_or (FAtomic 0) (FNot (FAtomic 0)) in
  let f_finally a = FUntil (f_true, a) in
  (* let w = (FAtomic 1) in
   * let ws = w |> formula_to_beuchi |> Formula.Beuchi.to_dot in
   * print_endline ws; *)
  let input = Sys.argv.(1) in
  let output = input |> parse |> Option.value_exn |> formula_to_beuchi in
  Printf.printf "%a" Formula.Beuchi.to_dot output
