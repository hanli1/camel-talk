open Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe

type response = {
  status: string;
  message: string
}

(**
 * [resp_to_def resp] takes in a deferred Cohttp.Response.t [resp] and returns a 
 * deferred response object with the status and message 
 *)
let resp_to_def resp = 
  resp >>= (fun (_,body) -> 
    body |> Cohttp_lwt_body.to_string >|= (
    fun s -> 
      let sjson = s |> Yojson.Basic.from_string in

  {
    status =
      sjson
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string;
    message =
      sjson
      |> Yojson.Basic.Util.member "message"
      |> Yojson.Basic.Util.to_string
  }))

let login_user usern passw server_addr=
  let resp = Client.post (Uri.of_string (server_addr ^ "/login_user"))
  ~body: (
    `String (
    	 Yojson.Basic.to_string
    	 (`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  )
  in
  resp_to_def resp

let register_user usern passw server_addr=
  let resp = Client.post (Uri.of_string (server_addr ^ "/register_user"))
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  )
  in
  resp_to_def resp


let send_message usern chanid orgid message_type jmessage server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/send_message"))
  ~body: (
    `String (
  	   Yojson.Basic.to_string
  	   (`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	   ("organization_id", `String orgid);("message_type", `String message_type);
       ("message", jmessage)])
    )
  )
 in
 resp_to_def resp

let create_organization usern orgid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/create_organization"))
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid)])
    )
  )
  in
  resp_to_def resp

let delete_organization usern orgid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/delete_organization"))
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid)])
    )
  )
  in
  resp_to_def resp


let create_channel usern orgid chanid server_addr=
  let resp = Client.post
    (Uri.of_string (server_addr ^ "/create_channel"))
  ~body: (
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  )
  in
  resp_to_def resp


let delete_channel usern orgid chanid server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/delete_channel"))
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  )
  in
  resp_to_def resp


let invite usern orgid requester server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/invite_user_organization"))
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("requester_id", `String requester)])
    )
  )
  in
  resp_to_def resp

let leave usern orgid requester server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/remove_user_organization"))
  ~body:(
    `String (
      Yojson.Basic.to_string
      (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("requester_id", `String requester)])
    )
  )
  in
  resp_to_def resp

let vote org chan poll choice server_addr=
  let resp = Client.post
  (Uri.of_string (server_addr ^ "/vote_poll"))
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
  resp_to_def resp

let get_org_info usern orgid server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr ^ "/get_org_info?" ^ "user_id=" ^
      usern ^ "&organization_id=" ^ orgid))
  in
  resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string) >|=
    fun s -> 
      let sjson = s |> Yojson.Basic.from_string in 
      ((sjson
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string),
    sjson)

let get_messages usern chanid orgid start_index server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr ^ "/get_messages?" ^ "user_id=" ^ usern
      ^ "&channel_id=" ^ chanid
      ^ "&organization_id=" ^ orgid
      ^ "&start_index=" ^ (string_of_int start_index)))
  in
  resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string) >|=
    fun s -> 
      let sjson = s |> Yojson.Basic.from_string in 
      ((sjson
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string),
    sjson)

let get_user_organizations usern server_addr=
  let resp = Client.get (Uri.of_string
    (server_addr ^ "/get_user_organizations?" ^ "user_id=" ^ usern))
  in
  resp >>= (fun (_,body) -> body |> Cohttp_lwt_body.to_string) >|=
    fun s -> 
      let sjson = s |> Yojson.Basic.from_string in 
      ((sjson
      |> Yojson.Basic.Util.member "status"
      |> Yojson.Basic.Util.to_string),
    sjson)
