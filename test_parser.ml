open OUnit2
open Parser


let tests = [
  "basic test" >:: (fun _ -> assert_equal CIllegal (text_to_message "#leave" Organizations));
]




let suite = "Data ops suite" >:::
  tests

let _ = run_test_tt_main suite