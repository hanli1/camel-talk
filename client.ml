open Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe

type response = {
  status: string;
  message: string
}

let login_user usern passw =
  let resp = Client.post (Uri.of_string "http://127.0.0.1:8000/login_user")
  ~body: (
    `String (
    	 Yojson.Basic.to_string
    	 (`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string}

let register_user usern passw =
  let resp = Client.post (Uri.of_string "http://127.0.0.1:8000/register_user")
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let send_message_simple usern chanid orgid jmessage =
  let _ = Client.post
  (Uri.of_string "http://127.0.0.1:8000/send_message")
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid);("message_type", `String "simple");
       ("message", jmessage)])
    )
  )
 in ()

let send_message_poll usern chanid orgid jmessage =
  let _ = Client.post
  (Uri.of_string "http://127.0.0.1:8000/send_message")
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid);("message_type", `String "poll");
  	   ("message", jmessage)])
    )
  )
 in ()

let send_message_reminder usern chanid orgid jmessage =
  let _ = Client.post
  (Uri.of_string "http://127.0.0.1:8000/send_message")
  ~body:(
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid); ("message_type", `String "reminder");
       ("message", jmessage)])
    )
  )
 in ()

let create_organization usern orgid =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/create_organization")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let delete_organization usern orgid =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/delete_organization")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let create_channel usern orgid chanid =
  let resp = Client.post
    (Uri.of_string "http://127.0.0.1:8000/create_channel")
  ~body: (
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let delete_channel usern orgid chanid =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/delete_channel")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let invite usern orgid requester =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/invite_user_organization")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("requester_id", `String requester)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let leave usern orgid requester =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/remove_user_organization")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("requester_id", `String requester)])
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let vote org chan poll choice =
  let resp = Client.post
  (Uri.of_string "http://127.0.0.1:8000/vote_poll")
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc
        [
          ("organization_id", `String org);
          ("channel_id", `String chan);
          ("poll_id", `String poll);
          ("choice_id", `String choice)
        ]
      )
    )
  )
  in
  let defresp = resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in {
    status =
      defresp
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      defresp
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }

let get_org_info usern orgid =
  let resp = Client.get (Uri.of_string
    ("http://127.0.0.1:8000/get_org_info?"^"user_id="^
      usern^"&organization_id="^orgid))
  in
  let resp_json = resp >>= (fun (_,body) ->
    body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run in
    ((resp_json |> Yojson.Basic.Util.member "status"
    |> Yojson.Basic.Util.to_string), resp_json)

let get_messages usern chanid orgid start_index =
  let resp = Client.get (Uri.of_string
    ("http://127.0.0.1:8000/get_messages?"^"user_id="^usern
      ^"&channel_id="^chanid
      ^"&organization_id="^orgid
      ^"&start_index="^(string_of_int start_index)))
  in
  let resp_json = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
    in
    ((resp_json |> Yojson.Basic.Util.member "status"
    |> Yojson.Basic.Util.to_string),
    resp_json)

let get_user_organizations usern =
  let resp = Client.get (Uri.of_string
    ("http://127.0.0.1:8000/get_user_organizations?"^"user_id="^usern))
  in
  let resp_json = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
    in
    ((resp_json |> Yojson.Basic.Util.member "status"
    |> Yojson.Basic.Util.to_string),
    resp_json)