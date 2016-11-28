open OUnit2
open Parser


let org_screen_tests = [
  "valid switch" >:: (fun _ -> assert_equal (CSwitch "org1") (text_to_message "org1" Organizations));
  "valid switch" >:: (fun _ -> assert_equal (CSwitch "org1 ;asldkfj") (text_to_message "org1 ;asldkfj" Organizations));
  "valid delete" >:: (fun _ -> assert_equal (CDelete "org1") (text_to_message "#delete org1" Organizations));
  "valid delete" >:: (fun _ -> assert_equal (CDelete "org1 1") (text_to_message "#delete org1 1" Organizations));
  "invalid delete" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#delete" Organizations));
  "valid create" >:: (fun _ -> assert_equal (CCreate "org1") (text_to_message "#create org1" Organizations));
  "valid create" >:: (fun _ -> assert_equal (CCreate "org1 123") (text_to_message "#create org1 123" Organizations));
  "invalid create" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#create" Organizations));
  "valid invite" >:: (fun _ -> assert_equal (CInvite ("john", "org1")) (text_to_message "#invite john org1" Organizations));
  "invalid invite" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#invite org1" Organizations));
  "valid leave" >:: (fun _ -> assert_equal (CLeave ("john", "org1")) (text_to_message "#leave john org1" Organizations));
  "valid kick" >:: (fun _ -> assert_equal (CLeave ("john", "org1")) (text_to_message "#leave john org1" Organizations));
  "invalid leave" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#leave org1" Organizations));


  "valid logout" >:: (fun _ -> assert_equal (CLogout) (text_to_message "#logout" Organizations));
  "invalid logout" >:: (fun _ -> assert_equal (CSwitch "#logout org1") (text_to_message "#logout org1" Organizations));
  "valid back" >:: (fun _ -> assert_equal (CBack) (text_to_message "#back" Organizations));
  "valid help" >:: (fun _ -> assert_equal (CHelp) (text_to_message "#help" Organizations));
]

let chan_screen_tests = [
  "valid switch" >:: (fun _ -> assert_equal (CSwitch "chan1") (text_to_message "chan1" Channels));
  "valid switch" >:: (fun _ -> assert_equal (CSwitch "chan1 ;asldkfj") (text_to_message "chan1 ;asldkfj" Channels));
  "valid delete" >:: (fun _ -> assert_equal (CDelete "chan1") (text_to_message "#delete chan1" Channels));
  "valid delete" >:: (fun _ -> assert_equal (CDelete "chan1 1") (text_to_message "#delete chan1 1" Channels));
  "invalid delete" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#delete" Channels));
]
let mess_screen_tests = [
  "valid simple messages" >:: (fun _ -> assert_equal (CSimpleMessage "hello") (text_to_message "hello" Messages));
  "valid simple messages2" >:: (fun _ -> assert_equal (CSimpleMessage "hello") (text_to_message " hello" Messages));
  "valid reminder messages" >:: (fun _ -> assert_equal (CReminderMessage ("hello", 12)) (text_to_message "#set_reminder 12 hello" Messages));
  "invalid reminder messages2" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_reminder" Messages));
  "invalid reminder messages3" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_reminder 12" Messages));
  "invalid reminder messages4" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_reminder hello" Messages));
  "invalid reminder messages5" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_reminder hello 12" Messages));
  "valid poll messages" >:: (fun _ -> assert_equal (CPollMessage ("hello", ["choice1"; "choice2"])) (text_to_message "#set_poll [choice1, choice2] hello" Messages));
  "invalid poll messages" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_poll" Messages));
  "invalid poll messages2" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_poll hello" Messages));
  "invalid poll messages3" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_poll test1 test 2" Messages));
  "invalid poll messages4" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_poll [test 1 a" Messages));
  "invalid poll messages5" >:: (fun _ -> assert_equal (CIllegal) (text_to_message "#set_poll [test 1 a] " Messages));
]




let suite = "Parser suite" >:::
  org_screen_tests @ chan_screen_tests @ mess_screen_tests

let _ = run_test_tt_main suite