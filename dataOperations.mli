(** 
 * Value of type message contains constructors for the different types of
 * messages
 *)
type message =   
  | SimpleMessage of string
  | ReminderMessage of string * int
  | PollMessage of string * string list

(** 
 * Value of type channel represents the data associated with a single channel
 *)
type channel = {
  name: string;
  organization_name: string;
  channel_type: string;
  messages: message list
}

(** 
 * Value of type user represents the data associated with a single user
 *)
type user = {
  username: string;
  password: string;
}

(** 
 * Value of type organization represents the data associated with a single
 * organization
 *)
type organization = {
  name: string;
  channel_names: string list;
  users: user list;
  admin: user
}

(** 
 * Value of type all_data represents all data needed by all clients
 *)
type all_data = {
  users: user list;
  organization_names: string list
}

(** 
 * read_json_file reads the file at the file location [string] and returns
 * all data with a record type of [all_data]
 *)
val read_json_file : string -> all_data

(** 
 * write_json_file writes all current data [all_data] to the file at the 
 * location [string]
 *)
val write_json_file : all_data -> string -> unit
