type response = {
  status: bool;
  message: string;
}

val send_message_simple : string -> string -> string -> Yojson.Basic.json -> unit
val send_message_poll : string -> string -> string -> Yojson.Basic.json -> unit
val send_message_reminder : string -> string -> string -> Yojson.Basic.json -> unit

val register_user : string -> string -> response
val login_user : string -> string -> response

val create_organization : string -> string -> response
val delete_organization : string -> string -> response

val create_channel : string -> string -> string -> response
val delete_channel : string -> string -> string -> response

val get_channels : string -> string -> Yojson.Basic.json
val get_messages : string -> string -> Yojson.Basic.json