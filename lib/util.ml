open Core
open Set

let test_test _ = 1
let%test "test_test1" = (test_test 2 = 1)

                          
