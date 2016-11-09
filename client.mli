(**
 * Response body from the server. Status is true if it passes, false if it
 * doesn't pass. The message may contain an error message, such as
 * “can’t create channel that already exists”
 *)
type response = {
  status: bool;
  message: string;
}

(**
 * Send messages take in the user id, channel id, organization id, the message
 * body itself.
 *)

(**
 * Simple message: only text
 *)
val send_message_simple : string -> string -> string ->
	Yojson.Basic.json -> unit

(**
 * Poll message: other members of the channel can vote on a specific option
 *)
val send_message_poll : string -> string -> string ->
	Yojson.Basic.json -> unit

(**
 * Reminder message: the message is sent to the specified channel only at a
 * specified pre-set time. The time is set up in the json.
 *)
val send_message_reminder : string -> string -> string ->
	Yojson.Basic.json -> unit

(**
 * Upon startup, prompts the user to either login or register. Takes in
 * a new username and password unique pairing to store in the server and allow
 * future authentications
 *)
val register_user : string -> string -> response

(**
 * Takes in existing username and password, checks if it is a correct
 * unique pairing
 *)
val login_user : string -> string -> response

(**
 * Takes in the organization name to be created and te user_id, then
 * creates the organization. This should also update/repaint the interface
 *)
val create_organization : string -> string -> response

(**
 * Similar inputs to create_organization, but instead deletes an existing
 * organization
 *)
val delete_organization : string -> string -> response

(**
 * Takes in the user id and an organization name, as well as the name of the
 * channel to be created. Creates the channel inside the organization specified
 *)
val create_channel : string -> string -> string -> response

(**
 * Similar to create_channel, except deletes the specified channel name from
 * the specified organization
 *)
val delete_channel : string -> string -> string -> response

(**
 * Takes in user id and organization name and returns a json object containing
 * an array of team channels and private channels
 *)
val get_channels : string -> string -> Yojson.Basic.json

(**
 * Takes in user id, organization name, channel name, start index, and
 * returns a json object containing the message, message type, user_id of the
 * responder, and time stamp, among other things
 *)
val get_messages : string -> string -> string -> int -> Yojson.Basic.json