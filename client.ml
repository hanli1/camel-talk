open Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe

type response = {
  status: string;
  message: string
}

let server_address _ =
  "http://97cdbffd.ngrok.io/"
  (* "http://127.0.0.1:8000/" *)

let login_user usern passw server_addr=
  let resp = Client.post (Uri.of_string (server_addr ^ "login_user"))
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

let register_user usern passw server_addr=
  let resp = Client.post (Uri.of_string (server_addr ^ "register_user"))
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

let send_message_simple usern chanid orgid jmessage server_addr=
  let _ = Client.post
  (Uri.of_string (server_addr^"send_message"))
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid);("message_type", `String "simple");
       ("message", jmessage)])
    )
  )
 in ()

let send_message_poll usern chanid orgid jmessage server_addr=
  let _ = Client.post
  (Uri.of_string (server_addr^"send_message"))
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid);("message_type", `String "poll");
  	   ("message", jmessage)])
    )
  )
 in ()

let send_message_reminder usern chanid orgid jmessage server_addr=
  let _ = Client.post
  (Uri.of_string (server_addr^"send_message"))
  ~body:(
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid); ("message_type", `String "reminder");
       ("message", jmessage)])
    )
  )
 in ()

let create_organization usern orgid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"create_organization"))
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

let delete_organization usern orgid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"delete_organization"))
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

let create_channel usern orgid chanid server_addr=
  let resp = Client.post
    (Uri.of_string (server_addr^"create_channel"))
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

let delete_channel usern orgid chanid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"delete_channel"))
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

let invite usern orgid requester server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"invite_user_organization"))
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

let leave usern orgid requester server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"remove_user_organization"))
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

let vote org chan poll choice server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr^"vote_poll"))
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

let get_org_info usern orgid server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr^"get_org_info?"^"user_id="^
      usern^"&organization_id="^orgid))
  in
  let resp_json = resp >>= (fun (_,body) ->
    body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run in
    ((resp_json |> Yojson.Basic.Util.member "status"
    |> Yojson.Basic.Util.to_string), resp_json)

let get_messages usern chanid orgid start_index server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr^"get_messages?"^"user_id="^usern
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

let get_user_organizations usern server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr^"get_user_organizations?"^"user_id="^usern))
  in
  let resp_json = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
    (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
    in
    ((resp_json |> Yojson.Basic.Util.member "status"
    |> Yojson.Basic.Util.to_string),
    resp_json)