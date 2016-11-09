open DataOperations

(* processes the send simple message and updates the data store*)
val send_message_simple_api : Cohttp_Request -> string

(* processes the send poll message and updates the data store*)
val send_message_poll_api : Cohttp_Request -> string

(* processes the send reminder message and updates the data store*)
val send_message_reminder_api : Cohttp_Request -> string

(* processes the register user request and updates the data store*)
val register_user_api : Cohttp_Request -> string

(* processes the login user request*)
val login_user_api : Cohttp_Request -> string

(* processes the create new organization for a user and updates the data store*)
val create_organization_api : Cohttp_Request -> string

(* processes the delete organization for a user and updates the data store*)
val delete_organization_api : Cohttp_Request -> string


(* processes the create new channel for a user and updates the data store*)
val create_channel_api : Cohttp_Request -> string

(* processes the delete channel for a user and updates the data store*)
val delete_channel_api : Cohttp_Request -> string


(* processes the get channels request and gives the user all channels associated
 * with an organization
 *)
val get_channels_api : Cohttp_Request -> string

(* processes the get messages request and gives the user all messages associated
 * with a channel inside an organization]
 *)
val get_messages_api : Cohttp_Request -> string