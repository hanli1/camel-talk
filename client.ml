open Yojson.Safe

type response = {
  status: bool;
  message: string
}

let login_user usern passw = {status = false; message = ""}
let register_user usern passw = {status = true; message = ""}

let send_message_simple s1 s2 s3 j = ()
let send_message_poll s1 s2 s3 j = ()
let send_message_reminder s1 s2 s3 j = ()

let create_organization s1 s2 = {status = true; message = ""}
let delete_organization s1 s2 = {status = true; message = ""}
let create_channel s1 s2 s3 = {status = true; message = ""}
let delete_channel s1 s2 s3 = {status = true; message = ""}

let get_channels s1 s2 = to_basic `Null

let get_messages  s1 s2 s3 i = to_basic `Null