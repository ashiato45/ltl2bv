open Core

type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]


let parse txt_ =
  let words = String.split ~on:' ' txt_ in
  let help st w =
    match st with 
    | None -> None
    | Some st -> (
      match (String.get w 0) with 
      | 'p' -> (
        let intlen = String.length w - 1 in
        let num = int_of_string (String.sub ~pos:1 ~len:intlen w) in
        Stack.push st (FAtomic num);
        Some st
      )
      | '&' -> (
        let left = Stack.pop st in
        let right = Stack.pop st in 
        match right with
        | None -> None
        | Some right -> (
          match left with
          | None -> None (* cannot happen... *)
          | Some left -> (
            Stack.push st (FAnd (left, right));
            Some st
          )
        )
      )
      | '!' -> (
        let left = Stack.pop st in
        match left with
        | None -> None
        | Some left -> (
          Stack.push st (FNot left);
          Some st
        )
      )
      | 'X' -> (
        let left = Stack.pop st in
        match left with
        | None -> None
        | Some left -> (
          Stack.push st (FNext left);
          Some st
        )
      )
      | 'U' -> (
        let left = Stack.pop st in
        match left with
        | None -> None
        | Some left -> (
          let right = Stack.pop st in
          match right with
          | None -> None
          | Some right -> (
            Stack.push st (FUntil (left, right));
            Some st
          )
        )
      )
      | otherwise -> None
    ) in
  let st = List.fold_left ~init:(Some (Stack.create ())) ~f:help words in
  match st with
  | None -> None
  | Some st -> (
    let rest = Stack.pop st in
    rest
  )
let%expect_test "parse1" =
  "p2" |> parse |> [%sexp_of: formula option] |> Sexp.to_string |> print_endline;
  [%expect {| ((FAtomic 2)) |}]
let%expect_test "parse2" =
  "p0 p1 U" |> parse |> [%sexp_of: formula option] |> Sexp.to_string |> print_endline;
[%expect {| ((FUntil(FAtomic 1)(FAtomic 0))) |}]  
let%expect_test "parse3" =
  "p2 p3 & X" |> parse |> [%sexp_of: formula option] |> Sexp.to_string |> print_endline;
[%expect {| ((FNext(FAnd(FAtomic 3)(FAtomic 2)))) |}]  
    

