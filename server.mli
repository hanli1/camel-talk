(**
 * request_record is a type that represents the contents of an HTTP Request
 * for an API call 
 *)
type request_record = {
  request_info : Cohttp.Request.t;
  request_body : string;
}

(**
 * response_record is a type that represents the contents of an HTTP Response
 * for an API call
 *)
type response_record = {
  status_code : int;
  response_body : string;
}

(**
 * processes a send message request and updates the data store
 *)
val send_message_api : request_record -> response_record

(**
 * processes the register user request and updates the data store
 *)
val register_user_api : request_record -> response_record

(**
 * processes the login user request
 *)
val login_user_api : request_record -> response_record

(**
 * processes the create new organization for a user and updates the data store
 *)
val create_organization_api : request_record -> response_record

(**
 * processes the delete organization for a user and updates the data store
 *)
val delete_organization_api : request_record -> response_record


(**
 * processes the create new channel for a user and updates the data store
 *)
val create_channel_api : request_record -> response_record

(**
 * processes the delete channel for a user and updates the data store
 *)
val delete_channel_api : request_record -> response_record


(**
 * processes the get channels request and gives the user all channels associated
 * with an organization
 *)
val get_channels_api : request_record -> response_record

(**
 * processes the get messages request and gives the user all messages associated
 * with a channel inside an organization]
 *)
val get_messages_api : request_record -> response_record