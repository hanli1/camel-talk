include Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix

type response = {
  status: bool;
  message: string
}

let login_user usern passw = 
  let resp = Client.post (Uri.of_string "http://localhost:8000/login_user") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("User_id", `String usern);("Password", `String passw)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
(fun s -> s |> Yojson.Basic.from_string |> return) >>=
(fun j -> j |> Yojson.Basic.Util.member "status" |> return) >>=
(fun st -> st |> Yojson.Basic.Util.to_bool |> return)) |> Lwt_main.run
 in
  {status = defresp ; message = ""} (*handle response error message*)

let register_user usern passw =   
  let resp = Client.post (Uri.of_string "http://localhost:8000/register_user") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("User_id", `String usern);("Password", `String passw)])
    )
  ) in
  let defresp = resp >>= (fun (_,body) ->
  body |> Cohttp_lwt_body.to_string >>=
(fun s -> s |> Yojson.Basic.from_string |> return) >>=
(fun j -> j |> Yojson.Basic.Util.member "status" |> return) >>=
(fun st -> st |> Yojson.Basic.Util.to_bool |> return)) |> Lwt_main.run
 in
  {status = defresp ; message = ""}

let send_message_simple usern chanid orgid jmessage =   
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_simple") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("User_id", `String usern);("Channel_id", `String chanid);
  	("Organization_id", `String orgid); ("Message", jmessage
  	)])
    )
  )
 in ()

let send_message_poll usern chanid orgid jmessage =
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_poll") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("User_id", `String usern);("Channel_id", `String chanid);
  	("Organization_id", `String orgid); ("Message", jmessage
  	)])
    )
  )
 in ()

let send_message_reminder usern chanid orgid jmessage =
  let _ = Client.post 
  (Uri.of_string "http://localhost:8000/send_message_simple") 
  ~body:(`String 
  	(
  	Yojson.Basic.Util.to_string 
  	(`Assoc [("User_id", `String usern);("Channel_id", `String chanid);
  	("Organization_id", `String orgid); ("Message", jmessage
  	)])
    )
  )
 in ()

let create_organization s1 s2 = {status = true; message = ""}
let delete_organization s1 s2 = {status = true; message = ""}
let create_channel s1 s2 s3 = {status = true; message = ""}
let delete_channel s1 s2 s3 = {status = true; message = ""}

let get_channels s1 s2 = `Null

let get_messages  s1 s2 s3 i = `Null