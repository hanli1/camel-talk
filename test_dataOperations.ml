open OUnit2
open DataOperations

let cmp_str (s1 : string) (s2 : string) : int =
  if s1 < s2 then -1 else if s1 > s2 then 1 else 0

let fix_timestamp (m : message) : message =
  {m with timestamp=0}

let sort_org (org : organization option) : organization option =
  match org with
  | Some o -> Some
    {
      name=o.name;
      channel_names=(List.sort cmp_str o.channel_names);
      users=(List.sort cmp_str o.users);
      admin=o.admin;
    }
  | None -> None

let sort_channel (chan : channel option) : channel option =
  match chan with
  | Some c -> Some {
    name=c.name;
    users=(List.sort cmp_str c.users);
    is_public=c.is_public;
    messages=(List.map fix_timestamp c.messages);
  }
  | None -> None

(* Empty database *)
let d0 = make_data ()

(* Populated database *)
let d1 = make_data ()

let r1 = add_user d1 "user1" "pass1"
let r2 = add_user d1 "user1" "passfoo" (* should fail *)
let r3 = add_user d1 "user2" "pass2"
let r4 = add_user d1 "user3" "pass3"
let r5 = add_user d1 "user4" "pass4"
let r6 = add_user d1 "user5" "pass5"
let r7 = add_user d1 "user3" "passbar" (* should fail *)

let r8 = add_org d1 "org1" "user1"
let r9 = add_org d1 "org1" "user2" (* should fail *)
let r10 = add_org d1 "org1" "userfoo" (* should fail *)
let r11 = add_user_org d1 "user2" "org1"
let r12 = add_user_org d1 "user3" "org1"
let r13 = add_user_org d1 "user1" "org1" (* should fail *)
let r14 = add_channel d1 "org1" "channel1" "user2" true
let r15 = add_channel d1 "org1" "channel2" "user1" false
let r16 = join_channel d1 "channel1" "user3" "org1"
let r17 = join_channel d1 "channel2" "user3" "org1"

let r18 = add_message d1 "org1" "channel1" "user2" (SimpleMessage "msg1")
let r19 = add_message d1 "org1" "channel1" "user3" (ReminderMessage ("foo", 100))
let r20 = add_message d1 "org1" "channel1" "user2" (PollMessage ("poll1", [("opt1",1);("opt2",2);("opt3",3)]))
let r21 = add_message d1 "org1" "channel2" "user1" (SimpleMessage "msg2")
let r22 = add_message d1 "org1" "channel2" "user3" (SimpleMessage "msg3")
let r23 = add_message d1 "org1" "channel2" "user5" (SimpleMessage "foobar") (* should fail *)

let r24 = add_org d1 "org2" "user4"
let r25 = add_user_org d1 "user5" "org2"
let r26 = add_user_org d1 "user3" "org2"
let r27 = add_user_org d1 "user3110" "org2" (* should fail *)
let r28 = add_channel d1 "org2" "channel3" "user5" true
let r29 = add_channel d1 "org2" "channel4" "user4" true
let r30 = add_channel d1 "org2" "channel5" "user4" true
let r31 = join_channel d1 "channel3" "user3" "org2"
let r32 = join_channel d1 "channel4" "user5" "org2"
let r33 = join_channel d1 "channel4" "user3" "org2"

let r34 = add_message d1 "org2" "channel3" "user5" (SimpleMessage "msg4")
let r35 = add_message d1 "org2" "channel3" "user5" (SimpleMessage "msg5")

(* Populated database from backup *)
let b1 = backup_data d1
let d2 = load_data ()

(* Test miscellaneous write functions with emphasis on removal *)
let d3 = load_data ()

let r36 = change_user_pass d3 "user1" "pass3110"
let r37 = change_user_pass d3 "user3110" "passfoo" (* should fail *)
let r38 = change_user_pass d3 "user3" "pass3000"
let r39 = remove_user d3 "user4"
let r40 = remove_user d3 "user3110" (* should fail *)

let r41 = remove_user_org d3 "user4" "org2"

let r42 = remove_user_org d3 "user4" "org3" (* should fail *)

let r43 = leave_channel d3 "channel5" "user4" "org2"

let r44 = leave_channel d3 "channel6" "user4" "org2" (* should fail *)
let r45 = leave_channel d3 "channel5" "user4" "org2" (* should fail *)
let r46 = remove_channel d3 "org2" "channel5"

let r47 = remove_channel d3 "org2" "channel6" (* should fail *)
let r48 = remove_channel d3 "org2" "channel5" (* should fail *)
let r49 = remove_org d3 "org2"
let r50 = remove_org d3 "org2" (* should fail *)
let r51 = vote_poll d3 "org1" "channel1" "poll1" "opt1"
let r52 = vote_poll d3 "org1" "channel1" "poll1" "opt2"
let r53 = vote_poll d3 "org1" "channel1" "poll1" "opt3"
let r54 = vote_poll d3 "org1" "channel1" "poll1" "opt4" (* should fail *)
let r55 = vote_poll d3 "org1" "channel1" "poll2" "opt1" (* should fail *)
let r56 = vote_poll d3 "org1" "channel2" "poll1" "opt1" (* should fail *)
let r57 = vote_poll d3 "org2" "channel1" "poll1" "opt1" (* should fail *)

(* populated2 from backup *)
let b2 = backup_data d3
let d4 = load_data ()

(* backup empty db *)
let b3 = backup_data d0
let d5 = load_data ()


let empty_tests = [
  "get_user_list0" >:: (fun _ -> assert_equal [] (get_user_list d0));
  "get_user_data0" >:: (fun _ -> assert_equal None (get_user_data d0 "foo"));
  "get_user_data1" >:: (fun _ -> assert_equal None (get_user_data d0 "bar"));
  "get_org_list0" >:: (fun _ -> assert_equal [] (get_org_list d0));
  "get_org_data0" >:: (fun _ -> assert_equal None (get_org_data d0 "foo"));
  "get_org_data1" >:: (fun _ -> assert_equal None (get_org_data d0 "bar"));
  "get_channel_data0"
    >:: (fun _ -> assert_equal None (get_channel_data d0 "foo" "bar"));
]

let populate_tests1 = [
  "r1" >:: (fun _ -> assert_equal true r1);
  "r2" >:: (fun _ -> assert_equal false r2);
  "r3" >:: (fun _ -> assert_equal true r3);
  "r4" >:: (fun _ -> assert_equal true r4);
  "r5" >:: (fun _ -> assert_equal true r5);
  "r6" >:: (fun _ -> assert_equal true r6);
  "r7" >:: (fun _ -> assert_equal false r7);
  "r8" >:: (fun _ -> assert_equal true r8);
  "r9" >:: (fun _ -> assert_equal false r9);
  "r10" >:: (fun _ -> assert_equal false r10);
  "r11" >:: (fun _ -> assert_equal true r11);
  "r12" >:: (fun _ -> assert_equal true r12);
  "r13" >:: (fun _ -> assert_equal false r13);
  "r14" >:: (fun _ -> assert_equal true r14);
  "r15" >:: (fun _ -> assert_equal true r15);
  "r16" >:: (fun _ -> assert_equal true r16);
  "r17" >:: (fun _ -> assert_equal true r17);
  "r18" >:: (fun _ -> assert_equal true r18);
  "r19" >:: (fun _ -> assert_equal true r19);
  "r20" >:: (fun _ -> assert_equal true r20);
  "r21" >:: (fun _ -> assert_equal true r21);
  "r22" >:: (fun _ -> assert_equal true r22);
  "r23" >:: (fun _ -> assert_equal false r23);
  "r24" >:: (fun _ -> assert_equal true r24);
  "r25" >:: (fun _ -> assert_equal true r25);
  "r26" >:: (fun _ -> assert_equal true r26);
  "r27" >:: (fun _ -> assert_equal false r27);
  "r28" >:: (fun _ -> assert_equal true r28);
  "r29" >:: (fun _ -> assert_equal true r29);
  "r30" >:: (fun _ -> assert_equal true r30);
  "r31" >:: (fun _ -> assert_equal true r31);
  "r32" >:: (fun _ -> assert_equal true r32);
  "r33" >:: (fun _ -> assert_equal true r33);
  "r34" >:: (fun _ -> assert_equal true r34);
  "r35" >:: (fun _ -> assert_equal true r35);

  "get_user_list1" >::
    (fun _ -> assert_equal ["user1";"user2";"user3";"user4";"user5"] (List.sort cmp_str (get_user_list d1)));

  "get_org_list1" >::
    (fun _ -> assert_equal ["org1";"org2"] (List.sort cmp_str (get_org_list d1)));

  "get_user_data2" >::
    (fun _ -> assert_equal (Some {username="user1"; password="pass1"}) (get_user_data d1 "user1"));

  "get_user_data3" >::
    (fun _ -> assert_equal (Some {username="user2"; password="pass2"}) (get_user_data d1 "user2"));

  "get_user_data4" >::
    (fun _ -> assert_equal (Some {username="user3"; password="pass3"}) (get_user_data d1 "user3"));

  "get_user_data5" >::
    (fun _ -> assert_equal (Some {username="user4"; password="pass4"}) (get_user_data d1 "user4"));

  "get_user_data6" >::
    (fun _ -> assert_equal (Some {username="user5"; password="pass5"}) (get_user_data d1 "user5"));

  "get_user_data7" >::
    (fun _ -> assert_equal None (get_user_data d1 "user6"));

  "get_org_data2" >::
    (fun _ -> assert_equal (Some {name="org1";
      channel_names=["channel1"; "channel2"]; users=["user1"; "user2"; "user3"];
      admin="user1"}) (sort_org (get_org_data d1 "org1")));

  "get_org_data3" >::
    (fun _ -> assert_equal (Some {name="org2";
      channel_names=["channel3"; "channel4"; "channel5"];
      users=["user3"; "user4"; "user5"]; admin="user4"})
      (sort_org (get_org_data d1 "org2")));

  "get_org_data4" >::
    (fun _ -> assert_equal None (get_org_data d1 "org3"));

  "get_channel_data1" >::
    (fun _ -> assert_equal (Some {name="channel1"; is_public=true;
      users=["user2"; "user3"]; messages=[{user_id="user2"; timestamp=0;
      body=(SimpleMessage "msg1")}; {user_id="user3"; timestamp=0;
      body=(ReminderMessage ("foo", 100))}; {user_id="user2"; timestamp=0;
      body=(PollMessage ("poll1", [("opt1",1);("opt2",2);("opt3",3)]))}]})

      (sort_channel (get_channel_data d1 "org1" "channel1")));

  "get_channel_data2" >::
    (fun _ -> assert_equal (Some {name="channel2"; is_public=false;
      users=["user1"; "user3"]; messages=[
      {
        user_id="user1";
        timestamp=0;
        body=(SimpleMessage "msg2");
      };
      {
        user_id="user3";
        timestamp=0;
        body=(SimpleMessage "msg3");
      };
      ]})
      (sort_channel (get_channel_data d1 "org1" "channel2")));

  "get_channel_data3" >::
    (fun _ -> assert_equal (Some {name="channel3"; is_public=true;
      users=["user3"; "user5"]; messages=[
      {
        user_id="user5";
        timestamp=0;
        body=(SimpleMessage "msg4");
      };
      {
        user_id="user5";
        timestamp=0;
        body=(SimpleMessage "msg5");
      };
      ]})
      (sort_channel (get_channel_data d1 "org2" "channel3")));

  "get_channel_data4" >::
    (fun _ -> assert_equal (Some {name="channel4"; is_public=true;
      users=["user3"; "user4"; "user5"]; messages=[]})
      (sort_channel (get_channel_data d1 "org2" "channel4")));

  "get_channel_data5" >::
    (fun _ -> assert_equal (Some {name="channel5"; is_public=true;
      users=["user4"]; messages=[]})
      (sort_channel (get_channel_data d1 "org2" "channel5")));

  "get_channel_data6" >::
    (fun _ -> assert_equal None (get_channel_data d1 "org1" "channel6"));

  "get_channel_data7" >::
    (fun _ -> assert_equal None (get_channel_data d1 "org2" "channel6"));

  "get_channel_data8" >::
    (fun _ -> assert_equal None (get_channel_data d1 "org3" "channel1"));
]

let backup_tests1 = [
  "backup_data0" >:: (fun _ -> assert_equal true b1);
  "get_user_list1" >::
    (fun _ -> assert_equal ["user1";"user2";"user3";"user4";"user5"] (List.sort cmp_str (get_user_list d2)));

  "get_org_list1" >::
    (fun _ -> assert_equal ["org1";"org2"] (List.sort cmp_str (get_org_list d2)));

  "get_user_data2" >::
    (fun _ -> assert_equal (Some {username="user1"; password="pass1"}) (get_user_data d2 "user1"));

  "get_user_data3" >::
    (fun _ -> assert_equal (Some {username="user2"; password="pass2"}) (get_user_data d2 "user2"));

  "get_user_data4" >::
    (fun _ -> assert_equal (Some {username="user3"; password="pass3"}) (get_user_data d2 "user3"));

  "get_user_data5" >::
    (fun _ -> assert_equal (Some {username="user4"; password="pass4"}) (get_user_data d2 "user4"));

  "get_user_data6" >::
    (fun _ -> assert_equal (Some {username="user5"; password="pass5"}) (get_user_data d2 "user5"));

  "get_user_data7" >::
    (fun _ -> assert_equal None (get_user_data d2 "user6"));

  "get_org_data2" >::
    (fun _ -> assert_equal (Some {name="org1";
      channel_names=["channel1"; "channel2"]; users=["user1"; "user2"; "user3"];
      admin="user1"}) (sort_org (get_org_data d2 "org1")));

  "get_org_data3" >::
    (fun _ -> assert_equal (Some {name="org2";
      channel_names=["channel3"; "channel4"; "channel5"];
      users=["user3"; "user4"; "user5"]; admin="user4"})
      (sort_org (get_org_data d2 "org2")));

  "get_org_data4" >::
    (fun _ -> assert_equal None (get_org_data d2 "org3"));

  "get_channel_data1" >::
    (fun _ -> assert_equal (Some {name="channel1"; is_public=true;
      users=["user2"; "user3"]; messages=[{user_id="user2"; timestamp=0;
      body=(SimpleMessage "msg1")}; {user_id="user3"; timestamp=0;
      body=(ReminderMessage ("foo", 100))}; {user_id="user2"; timestamp=0;
      body=(PollMessage ("poll1", [("opt1",1);("opt2",2);("opt3",3)]))}]})

      (sort_channel (get_channel_data d2 "org1" "channel1")));

  "get_channel_data2" >::
    (fun _ -> assert_equal (Some {name="channel2"; is_public=false;
      users=["user1"; "user3"]; messages=[
      {
        user_id="user1";
        timestamp=0;
        body=(SimpleMessage "msg2");
      };
      {
        user_id="user3";
        timestamp=0;
        body=(SimpleMessage "msg3");
      };
      ]})
      (sort_channel (get_channel_data d2 "org1" "channel2")));

  "get_channel_data3" >::
    (fun _ -> assert_equal (Some {name="channel3"; is_public=true;
      users=["user3"; "user5"]; messages=[
      {
        user_id="user5";
        timestamp=0;
        body=(SimpleMessage "msg4");
      };
      {
        user_id="user5";
        timestamp=0;
        body=(SimpleMessage "msg5");
      };
      ]})
      (sort_channel (get_channel_data d2 "org2" "channel3")));

  "get_channel_data4" >::
    (fun _ -> assert_equal (Some {name="channel4"; is_public=true;
      users=["user3"; "user4"; "user5"]; messages=[]})
      (sort_channel (get_channel_data d2 "org2" "channel4")));

  "get_channel_data5" >::
    (fun _ -> assert_equal (Some {name="channel5"; is_public=true;
      users=["user4"]; messages=[]})
      (sort_channel (get_channel_data d2 "org2" "channel5")));

  "get_channel_data6" >::
    (fun _ -> assert_equal None (get_channel_data d2 "org1" "channel6"));

  "get_channel_data7" >::
    (fun _ -> assert_equal None (get_channel_data d2 "org2" "channel6"));

  "get_channel_data8" >::
    (fun _ -> assert_equal None (get_channel_data d2 "org3" "channel1"));
]

let populate_tests2 = [
  "r36" >:: (fun _ -> assert_equal true r36);
  "r37" >:: (fun _ -> assert_equal false r37);
  "r38" >:: (fun _ -> assert_equal true r38);
  "r39" >:: (fun _ -> assert_equal true r39);
  "r40" >:: (fun _ -> assert_equal false r40);
  "r41" >:: (fun _ -> assert_equal true r41);
  "r42" >:: (fun _ -> assert_equal false r42);
  "r43" >:: (fun _ -> assert_equal true r43);
  "r44" >:: (fun _ -> assert_equal false r44);
  "r45" >:: (fun _ -> assert_equal false r45);
  "r46" >:: (fun _ -> assert_equal true r46);
  "r47" >:: (fun _ -> assert_equal false r47);
  "r48" >:: (fun _ -> assert_equal false r48);
  "r49" >:: (fun _ -> assert_equal true r49);
  "r50" >:: (fun _ -> assert_equal false r50);
  "r51" >:: (fun _ -> assert_equal true r51);
  "r52" >:: (fun _ -> assert_equal true r52);
  "r53" >:: (fun _ -> assert_equal true r53);
  "r54" >:: (fun _ -> assert_equal false r54);
  "r55" >:: (fun _ -> assert_equal false r55);
  "r56" >:: (fun _ -> assert_equal false r56);
  "r57" >:: (fun _ -> assert_equal false r57);

  "get_user_list1" >::
    (fun _ -> assert_equal ["user1";"user2";"user3";"user5"] (List.sort cmp_str (get_user_list d3)));

  "get_org_list1" >::
    (fun _ -> assert_equal ["org1"] (List.sort cmp_str (get_org_list d3)));

  "get_user_data2" >::
    (fun _ -> assert_equal (Some {username="user1"; password="pass3110"}) (get_user_data d3 "user1"));

  "get_user_data3" >::
    (fun _ -> assert_equal (Some {username="user2"; password="pass2"}) (get_user_data d3 "user2"));

  "get_user_data4" >::
    (fun _ -> assert_equal (Some {username="user3"; password="pass3000"}) (get_user_data d3 "user3"));

  "get_user_data6" >::
    (fun _ -> assert_equal (Some {username="user5"; password="pass5"}) (get_user_data d3 "user5"));

  "get_user_data7" >::
    (fun _ -> assert_equal None (get_user_data d3 "user4"));

  "get_org_data2" >::
    (fun _ -> assert_equal (Some {name="org1";
      channel_names=["channel1"; "channel2"]; users=["user1"; "user2"; "user3"];
      admin="user1"}) (sort_org (get_org_data d3 "org1")));

  "get_org_data4" >::
    (fun _ -> assert_equal None (get_org_data d3 "org2"));

  "get_channel_data1" >::
    (fun _ -> assert_equal (Some {name="channel1"; is_public=true;
      users=["user2"; "user3"]; messages=[{user_id="user2"; timestamp=0;
      body=(SimpleMessage "msg1")}; {user_id="user3"; timestamp=0;
      body=(ReminderMessage ("foo", 100))}; {user_id="user2"; timestamp=0;
      body=(PollMessage ("poll1", [("opt1",2);("opt2",3);("opt3",4)]))}]})

      (sort_channel (get_channel_data d3 "org1" "channel1")));

  "get_channel_data2" >::
    (fun _ -> assert_equal (Some {name="channel2"; is_public=false;
      users=["user1"; "user3"]; messages=[
      {
        user_id="user1";
        timestamp=0;
        body=(SimpleMessage "msg2");
      };
      {
        user_id="user3";
        timestamp=0;
        body=(SimpleMessage "msg3");
      };
      ]})
      (sort_channel (get_channel_data d3 "org1" "channel2")));

  "get_channel_data6" >::
    (fun _ -> assert_equal None (get_channel_data d3 "org1" "channel6"));

  "get_channel_data7" >::
    (fun _ -> assert_equal None (get_channel_data d3 "org2" "channel6"));

  "get_channel_data8" >::
    (fun _ -> assert_equal None (get_channel_data d3 "org3" "channel1"));
]

let backup_tests2 = [
  "backup_data1" >:: (fun _ -> assert_equal b2 true);
  "get_user_list1" >::
    (fun _ -> assert_equal ["user1";"user2";"user3";"user5"] (List.sort cmp_str (get_user_list d4)));

  "get_org_list1" >::
    (fun _ -> assert_equal ["org1"] (List.sort cmp_str (get_org_list d3)));

  "get_user_data2" >::
    (fun _ -> assert_equal (Some {username="user1"; password="pass3110"}) (get_user_data d4 "user1"));

  "get_user_data3" >::
    (fun _ -> assert_equal (Some {username="user2"; password="pass2"}) (get_user_data d4 "user2"));

  "get_user_data4" >::
    (fun _ -> assert_equal (Some {username="user3"; password="pass3000"}) (get_user_data d4 "user3"));

  "get_user_data6" >::
    (fun _ -> assert_equal (Some {username="user5"; password="pass5"}) (get_user_data d4 "user5"));

  "get_user_data7" >::
    (fun _ -> assert_equal None (get_user_data d4 "user4"));

  "get_org_data2" >::
    (fun _ -> assert_equal (Some {name="org1";
      channel_names=["channel1"; "channel2"]; users=["user1"; "user2"; "user3"];
      admin="user1"}) (sort_org (get_org_data d4 "org1")));

  "get_org_data4" >::
    (fun _ -> assert_equal None (get_org_data d4 "org2"));

  "get_channel_data1" >::
    (fun _ -> assert_equal (Some {name="channel1"; is_public=true;
      users=["user2"; "user3"]; messages=[{user_id="user2"; timestamp=0;
      body=(SimpleMessage "msg1")}; {user_id="user3"; timestamp=0;
      body=(ReminderMessage ("foo", 100))}; {user_id="user2"; timestamp=0;
      body=(PollMessage ("poll1", [("opt1",2);("opt2",3);("opt3",4)]))}]})

      (sort_channel (get_channel_data d4 "org1" "channel1")));

  "get_channel_data2" >::
    (fun _ -> assert_equal (Some {name="channel2"; is_public=false;
      users=["user1"; "user3"]; messages=[
      {
        user_id="user1";
        timestamp=0;
        body=(SimpleMessage "msg2");
      };
      {
        user_id="user3";
        timestamp=0;
        body=(SimpleMessage "msg3");
      };
      ]})
      (sort_channel (get_channel_data d4 "org1" "channel2")));

  "get_channel_data6" >::
    (fun _ -> assert_equal None (get_channel_data d4 "org1" "channel6"));

  "get_channel_data7" >::
    (fun _ -> assert_equal None (get_channel_data d4 "org2" "channel6"));

  "get_channel_data8" >::
    (fun _ -> assert_equal None (get_channel_data d4 "org3" "channel1"));
]

let backup_tests3 = [
  "backup_data2" >:: (fun _ -> assert_equal true b3);
  "get_user_list0" >:: (fun _ -> assert_equal [] (get_user_list d5));
  "get_user_data0" >:: (fun _ -> assert_equal None (get_user_data d5 "foo"));
  "get_user_data1" >:: (fun _ -> assert_equal None (get_user_data d5 "bar"));
  "get_org_list0" >:: (fun _ -> assert_equal [] (get_org_list d5));
  "get_org_data0" >:: (fun _ -> assert_equal None (get_org_data d5 "foo"));
  "get_org_data1" >:: (fun _ -> assert_equal None (get_org_data d5 "bar"));
  "get_channel_data0"
    >:: (fun _ -> assert_equal None (get_channel_data d5 "foo" "bar"));
]



let suite = "Data ops suite" >:::
  empty_tests
  @populate_tests1
  @backup_tests1
  @populate_tests2
  @backup_tests2
  @backup_tests3

let _ = run_test_tt_main suite