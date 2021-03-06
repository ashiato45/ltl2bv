open Core
open Set


type formula = FAtomic of int
             | FAnd of formula*formula
             | FNot of formula
             | FNext of formula
             | FUntil of formula*formula
                                   [@@deriving compare, sexp]


                                   
module PropSet = Set.Make(Int)
module AlphSet = Set.Make(PropSet)
module Formula = struct
  type t = formula
  let compare = compare_formula
  let t_of_sexp = formula_of_sexp
  let sexp_of_t = sexp_of_formula
end
module FormulaSet = Set.Make(Formula)


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
    

let get_subformulae fml_ =
  let res = ref [] in
  let rec help fml_ =
    res := fml_::!res;
    match fml_ with
    | FAtomic _ -> ()
    | FAnd (f1, f2) -> (
      help f1;
      help f2;
    );
    | FNot f -> help f;
    | FNext f -> help f;
    | FUntil (f1, f2) -> (help f1; help f2) in
  help fml_;
  !res
let%expect_test "get_subformulae1" =
  "p2" |> parse |> Option.value_exn |> get_subformulae |> [%sexp_of: formula list] |> Sexp.to_string |> print_endline;
  [%expect {| ((FAtomic 2)) |}]
let%expect_test "get_subformulae2" =
  "p2 p3 & X" |> parse |> Option.value_exn |> get_subformulae |> [%sexp_of: formula list] |> Sexp.to_string |> print_endline;
  [%expect {| ((FAtomic 2)(FAtomic 3)(FAnd(FAtomic 3)(FAtomic 2))(FNext(FAnd(FAtomic 3)(FAtomic 2)))) |}]
                      

let rec reduce_doublenegs fml_ =
  match fml_ with
  | FNot (FNot f) -> reduce_doublenegs f
  | FAtomic n -> FAtomic n
  | FAnd (f1, f2) -> FAnd (reduce_doublenegs f1, reduce_doublenegs f2)
  | FNot f -> FNot (reduce_doublenegs f)
  | FNext f -> FNext (reduce_doublenegs f)
  | FUntil (f1, f2) -> FUntil (reduce_doublenegs f1, reduce_doublenegs f2)
let%expect_test "reduce_doublenegs1" =
  "p2" |> parse |> Option.value_exn |> reduce_doublenegs |> [%sexp_of: formula] |> Sexp.to_string |> print_endline;
  [%expect {| (FAtomic 2) |}]
let%expect_test "reduce_doublenegs2" =
  "p2 !" |> parse |> Option.value_exn |> reduce_doublenegs |> [%sexp_of: formula] |> Sexp.to_string |> print_endline;
  [%expect {| (FNot(FAtomic 2)) |}]
let%expect_test "reduce_doublenegs3" =
  "p2 ! !" |> parse |> Option.value_exn |> reduce_doublenegs |> [%sexp_of: formula] |> Sexp.to_string |> print_endline;
  [%expect {| (FAtomic 2) |}]
let%expect_test "reduce_doublenegs4" =
  "p2 ! ! p3 ! & X" |> parse |> Option.value_exn |> reduce_doublenegs |> [%sexp_of: formula] |> Sexp.to_string |> print_endline;
  [%expect {| (FNext(FAnd(FNot(FAtomic 3))(FAtomic 2))) |}]
          
let gen_states_for_aba fml_ =
  let pos = fml_ |> get_subformulae in
  let neg = pos |> List.map ~f:(fun x -> FNot x) in
  let res = (List.append pos neg) |> List.map ~f:reduce_doublenegs |> FormulaSet.of_list in
  res
let%expect_test "gen_states_for_aba1" =
  "p2 !" |> parse |> Option.value_exn |> gen_states_for_aba |> FormulaSet.to_list |> [%sexp_of: formula list] |> Sexp.to_string |> print_endline;
  [%expect {| ((FAtomic 2)(FNot(FAtomic 2))) |}]  

module type LTLABA = Aba.ABA
  with module AlphSet = AlphSet
  with module StateSet = FormulaSet
                     
module LtlAba = Aba.Make (AlphSet) (FormulaSet)


let formula_to_aba fml_ =
  let rec take_bar affml_ =
    let open Aba in
    match affml_ with
    | AFAtomic a -> AFAtomic (a |> (fun x -> FNot x) |> reduce_doublenegs)
    | AFTrue -> AFTrue
    | AFFalse -> AFFalse
    | AFAnd (a, b) -> AFOr (take_bar a, take_bar b)
    | AFOr (a, b) -> AFAnd (take_bar a, take_bar b)
  in
  let states = gen_states_for_aba fml_ in
  let props = states
             |> PropSet.filter_map ~f:(fun fml ->
                    match fml with
                    | FAtomic n -> Some n
                    | otherwise -> None
                  ) in
  let alphs = props |> PropSet.to_list |> Util.power_list |> List.map ~f:(PropSet.of_list) |> AlphSet.of_list in
  let init = fml_ in
  let final = states |> FormulaSet.filter ~f:(fun fml ->
                            match fml with
                            | FNot (FUntil (a, b)) -> true
                            | otherwise -> false
                          ) in
  let rec trans s a =
    let open Aba in
    match s with
    | FAtomic n -> if (PropSet.mem a n) then AFTrue else AFFalse
    | FAnd (s1, s2) -> AFAnd (trans s1 a, trans s2 a)
    | FNot s -> take_bar (trans s a)
    | FNext s -> AFAtomic s
    | FUntil (xi, psi) -> AFOr (trans psi a, AFAnd (trans xi a, (AFAtomic (FUntil (xi, psi))))) in
  let res = {LtlAba.alph = alphs; LtlAba.states = states; LtlAba.trans = trans; LtlAba.init = init; LtlAba.final = final} in
  res

module type BEUCHISTATE = sig
  type t = FormulaSet.t * FormulaSet.t
  val sexp_of_t: t -> Sexp.t
  val t_of_sexp: Sexp.t -> t
end
module BeuchiState = struct
  type t = FormulaSet.t * FormulaSet.t
  let sexp_of_t = [%sexp_of: FormulaSet.t * FormulaSet.t]
  let t_of_sexp = [%of_sexp: FormulaSet.t * FormulaSet.t]
  let compare = [%compare: FormulaSet.t * FormulaSet.t]
end
module BeuchiStateSet = Set.Make(BeuchiState)
module Beuchi = Beuchi.Make (AlphSet) (BeuchiStateSet)
module ToBeuchi = LtlAba.ToBeuchi (BeuchiStateSet) (Beuchi)

let aba_to_beuchi aba_ = ToBeuchi.convert aba_

let formula_to_beuchi fml_ = fml_ |> formula_to_aba |> aba_to_beuchi
