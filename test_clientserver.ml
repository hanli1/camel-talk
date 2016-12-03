open OUnit2
open Client

let local = "http://127.0.0.1:8000/"

let tests = [

"register 1" >:: (fun _ -> assert_equal 
	{status = "success" ; message = "successfully authenticated"} 
	((register_user "joshua" "ying" local) |> Lwt_main.run));

"register 2" >:: (fun _ -> assert_equal 
	{status = "failure" ; message = "Unable to register new user due to 
	invalid username/password " ^ "or because username already exists."} 
	((register_user "joshua" "ying" local) |> Lwt_main.run));

"login 1" >:: (fun _ -> assert_equal 
	{status = "success" ; message = "successfully authenticated"}
	((login_user "joshua" "ying" local) |> Lwt_main.run));

"login 2" >:: (fun _ -> assert_equal 
	{status = "failure" ; message = "Password is incorrect for this user"} 
	((login_user "joshua" "yeeng" local) |> Lwt_main.run));

"create_organization 1" >:: (fun _ -> assert_equal
	{status = "success" ; message = "Organization successfully created"}
	((create_organization "joshua" "org1" local) |> Lwt_main.run));

"create_organization 2" >:: (fun _ -> assert_equal
	{status = "success" ; message = "Organization successfully created"}
	((create_organization "joshua" "org2" local));

"create_organization 3" >:: (fun _ -> assert_equal
	{status = "failure" ; message = "Unable to create organization because 
	the name is invalid or " ^ "an organization with that name already exists."}
	(create_organization "asdf" "org3" local));

"create_organization 4" >:: (fun _ -> assert_equal
	{status = "failure" ; message = "Unable to create organization because 
	the name is invalid or " ^ "an organization with that name already exists."}
	(create_organization "joshua" "org1" local));

"create_channel 1" >:: (fun _ -> assert_equal
	{status = "success" ; message = "Channel in organization successfully created"}
	(create_channel "joshua" "org1" "chan1" local));

"create_channel 2" >:: (fun _ -> assert_equal
	{status = "failure" ; message = "Channel already exists"}
	(create_channel "joshua" "org1" "chan1" local));

"create_channel 3" >:: (fun _ -> assert_equal
	{status = "failure" ; message = "Unable to create organization because 
	the name is invalid or " ^ "an organization with that name already exists."}
	(create_organization "asdf" "org3" local));

"create_organization 4" >:: (fun _ -> assert_equal
	{status = "failure" ; message = "Unable to create organization because 
	the name is invalid or " ^ "an organization with that name already exists."}
	(create_organization "asdf" "org3" local));

(* "send_message_simple 1" >:: (fun _ -> assert_equal 
	{status = "success" ; message = "Password is incorrect for this user"} 
	(send_message_simple "joshua" "ying" local)); *)
]


let suite = "Client and server suite" >::: tests
let _ = run_test_tt_main suite