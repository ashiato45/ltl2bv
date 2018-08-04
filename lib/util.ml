open Core
open Set

let test_test _ = 1
let%test "test_test1" = (test_test 2 = 1)

let rec power_list lst =
  let rec help lst acc =
    match lst with
    | [] -> acc
    | x::xs -> (
      let yes = acc |> List.map ~f:(fun ys -> x::ys) in
      let no = acc in
      help xs (List.append yes no)
    )
  in
  help lst [[]]
let%test "power_list1" = (power_list [1;2] = [[];[1];[2];[1;2]])       
                          
let rec make_pairs lst1 lst2 =
  let help lst1 lst2 acc =
    match lst1 with
    | [] -> acc
    | x::xs ->
       let adding = lst2 |> List.map ~f:(fun y -> (x, y)) in
       List.append adding acc
  in help lst1 lst2 []
let%test "make_pairs1" = (make_pairs [1;2] [3;4] = [(1, 3); (2, 3); (1, 4); (2, 4)])                 
