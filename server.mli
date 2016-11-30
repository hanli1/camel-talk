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
 * processes a user request to send a message
 *)
val send_message_api : request_record -> response_record

(**
 * processes a user request to register a user on the server
 *)
val register_user_api : request_record -> response_record

(**
 * processes a user request to login to the application
 *)
val login_user_api : request_record -> response_record

(**
 * processes a user request to create an organization
 *)
val create_organization_api : request_record -> response_record

(**
 * processes a user request to delete an organization
 *)
val delete_organization_api : request_record -> response_record

(**
 * processes a user request to invite a user to an organization
 *)
val invite_user_organization_api : request_record -> response_record

(**
 * processes a user request to remove a user from an organization
 *)
val remove_user_organization_api : request_record -> response_record

(**
 * processes a user request to create a new channel in an orgnanization
 *)
val create_channel_api : request_record -> response_record

(**
 * processes a user request to delete a channel of an organization
 *)
val delete_channel_api : request_record -> response_record

(**
 * processes a user request to get information about an organization, which
 * includes public channels, private channels, and users of the organization
 *)
val get_org_info_api : request_record -> response_record

(**
 * processes a user request to get up to 10 messages of a channel of an
 * organization
 *)
val get_messages_api : request_record -> response_record

(**
 * processes a user request to get the organizations he/she is a part of
 *)
val get_user_organizations_api : request_record -> response_record

(**
 * processes a user request to vote on a given poll of a channel of an
 * organization
 *)
val vote_poll_api : request_record -> response_record