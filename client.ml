open Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix

type response = {
  status: string;
  message: string
}

(* (*the last string and int are the user_id of the sender of the message and
the timestamp of the message respectively*)
type messsge =
| Simple of string * string * int
| Reminder of string * int * string * int
| Poll of string * (string list) * string * int

type get_response = {
  status: bool;
  messages: message list;
} *)

let login_user usern passw = 
  let resp = Client.post (Uri.of_string "http://localhost:8000/login_user") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}
(*    >>=
  }
  (fun j -> j |> Yojson.Basic.Util.member "status" |> return) >>=
  (fun st -> st |> Yojson.Basic.Util.to_bool |> return)) |> Lwt_main.run
 in
  {status = defresp ; message = ""} (*handle response error message*) *)

let register_user usern passw =   
  let resp = Client.post (Uri.of_string "http://localhost:8000/register_user") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("user_id", `String usern);("password", `String passw)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}

let send_message_simple usern chanid orgid jmessage =   
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_simple") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	("organization_id", `String orgid);("message_type", `String "simple"); 
    ("message", jmessage)])
    )
  )
 in ()

let send_message_poll usern chanid orgid jmessage =
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_poll") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	("organization_id", `String orgid);("message_type", `String "poll"); 
  	("message", jmessage)])
    )
  )
 in ()

let send_message_reminder usern chanid orgid jmessage =
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_simple") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("user_id", `String usern);("channel_id", `String chanid);
  	("organization_id", `String orgid); ("message_type", `String "reminder"); 
    ("message", jmessage)])
    )
  )
 in ()

let create_organization usern orgid = 
  let resp = Client.post 
  (Uri.of_string "http://localhost:8000/create_organization") 
  ~body:(`String 
    (
    Yojson.Basic.Util.to_string 
    (`Assoc [("user_id", `String usern);("orgname", `String orgid)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}

let delete_organization usern orgid = 
  let resp = Client.post 
  (Uri.of_string "http://localhost:8000/delete_organization") 
  ~body:(`String 
    (
    Yojson.Basic.Util.to_string 
    (`Assoc [("user_id", `String usern);("orgname", `String orgid)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}

let create_channel usern orgid chanid = 
  let resp = Client.post 
  (Uri.of_string "http://localhost:8000/create_channel") 
  ~body:(`String 
    (
    Yojson.Basic.Util.to_string 
    (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}

let delete_channel usern orgid chanid = 
  let resp = Client.post 
  (Uri.of_string "http://localhost:8000/delete_channel") 
  ~body:(`String 
    (
    Yojson.Basic.Util.to_string 
    (`Assoc [("user_id", `String usern);("organization_id", `String orgid);
      ("channel_id", `String chanid)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in
  {status = 
  defresp |> Yojson.Basic.Util.member "status" |> Yojson.Basic.Util.to_string;
  message = 
  defresp |> Yojson.Basic.Util.member "message" |> Yojson.Basic.Util.to_string}

let get_channels usern orgid =
  let resp = Client.get (Uri.of_string 
    ("http://localhost:8000/?"^"user_id="^usern^"&organization_id="^orgid))
  in
  let resp_json = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
  (fun s -> s |> Yojson.Basic.from_string |> return)) |> Lwt_main.run
  in 
  ((resp_json |> Yojson.Basic.Util.member "status" 
  |> Yojson.Basic.Util.to_string),
  resp_json)

let get_messages usern chanid orgid start_index = 
  let resp = Client.get (Uri.of_string 
    ("http://localhost:8000/?"^"user_id="^usern
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
