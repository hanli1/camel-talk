open Yojson.Basic
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Safe

(**
 * Response body from the server. status is either "success" or "failure".
 * message may contain an error message, such as "can’t create channel that 
 * already exists"
 *)
type response = {
  status: string;
  message: string
}


(**
 * [send_message usern chanid orgid message_type jmessage server_addr] sends
 * a message of type [message_type] (can be "simple", "reminder", or "poll") 
 * from a user on a channel of an organization
 *)
val send_message : string -> string -> string -> string ->
	Yojson.Basic.json -> string -> response Lwt.t

(**
 * [register_user usern passw server_addr] registers a user on the server
 * specified by [server_addr]
 *)
val register_user : string -> string -> string -> response Lwt.t

(**
 * [login_user usern passw server_addr] logins a user on the server specified
 * by [server_addr]
 *)
val login_user : string -> string -> string -> response Lwt.t

(**
 * [create_organization usern orgid server_addr] creates an organization with
 * name [orgid] and admin [usern]
 *)
val create_organization : string -> string -> string -> response Lwt.t

(**
 * [delete_organization usern orgid server_addr] deletes the organization with
 * name [orgid]
 *)
val delete_organization : string -> string -> string -> response Lwt.t

(**
 * [create_channel usern orgid chanid server_addr] creates the channel in the
 * specified organization
 *)
val create_channel : string -> string -> string -> string -> response Lwt.t

(**
 * [delete_channel usern orgid chanid server_addr] deletes the channel in the
 * specified organization
 *)
val delete_channel : string -> string -> string -> string -> response Lwt.t

(**
 * [get_org_info usern orgid server_addr] returns a response containing
 * information about the organization: the list of team channels, the list of 
 * private channels, and the list of users in the organization
 *)
val get_org_info : string -> string -> string -> (string * Yojson.Basic.json) Lwt.t

(**
 * [get_messages usern chanid orgid start_index server_addr] takes in user id, 
 * organization name, channel name, start index, and returns a response
 * containing up to 10 messages
 *)
val get_messages : string -> string -> string -> int -> string -> (string * Yojson.Basic.json) Lwt.t

(**
 * [get_user_organizations usern server_addr] returns a response containing the
 * list of organizations a user is a part of
 *)
val get_user_organizations : string -> string -> (string * Yojson.Basic.json) Lwt.t

(**
 * [invite usern orgid requester server_addr] invites a user to an organization
 * (the user is automatically added the organization)
 *)
val invite : string -> string -> string -> string -> response Lwt.t

(**
 * [leave usern orgid requester server_addr] allows a user to leave an organization
 * or for an admin to kick out a user from an organization
 *)
val leave : string -> string -> string -> string -> response Lwt.t

(**
 * [vote org chan poll choice server_addr] submits a vote for an option of a poll
 * of a given channel of a given organization
 *)
val vote : string -> string -> string -> string -> string -> response Lwt.t