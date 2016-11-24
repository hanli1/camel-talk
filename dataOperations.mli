(** 
 * Value of type message_body contains constructors for the different types of
 * message bodies.
 *)
type message_body =   
  | SimpleMessage of string
  | ReminderMessage of string * int
  | PollMessage of string * ((string * int) list)

(**
 * Value of type message contains the message body as well as user, channel,
 * and organization.
 *)
type message = {
  user_id : string;
  channel_id : string;
  organization_id : string;
  timestamp : int;
  body : message_body;
}

(** 
 * Value of type channel represents the data associated with a single channel
 *)
type channel = {
  name : string;
  organization_name : string;
  messages : message list;
  users : string list;
  is_public : bool;
}

(** 
 * Value of type user represents the data associated with a single user
 *)
type user = {
  username : string;
  password : string;
}

(** 
 * Value of type organization represents the data associated with a single
 * organization
 *)
type organization = {
  name : string;
  channel_names : string list;
  users : string list;
  admin : string;
}

(**
 * The representation type of the data store.
 *)
type t

(**
 * Initializes an empty data store. If there is no existing data store, this
 * function must be called before load_data will work.
 *)
val init_data : unit -> bool

(** 
 * [load_data ()] extracts a data structure from the data store and returns
 * its representation type.
 *)
val load_data : unit -> t

(** 
 * [backup_data t] updates the data store based on the representation type
 * t. Returns true or false indicating success or failure.
 *)
val backup_data : t -> bool

(** [get_user_list t] is the list of user_ids. *)
val get_user_list : t -> string list

(** [get_user_data t uid] is the user data for user with user_id [uid]. *)
val get_user_data : t -> string -> user option

(** [get_org_list t] is the list of organization names. *)
val get_org_list : t -> string list

(** [get_org_data t o] is the org data for organization with name [o]. *)
val get_org_data : t -> string -> organization option

(** 
 * [get_channel_data o c] is the channel data for channel with name c
 * in organization with name o.
 *)
val get_channel_data : t -> string -> string -> channel option

(** 
 * [add_user t u p] mutates t to include user with username u and password p.
 *)
val add_user : t -> string -> string -> bool

(** [remove_user t u] removes user [u] from t. His messages stay intact. *)
val remove_user : t -> string -> bool

(**
 * [change_user_pass t u p] changes user [u]'s password to [p].
 *)
val change_user_pass : t -> string -> string -> bool

(**
 * [add_user_org t u o] adds organization [o] to user [u].
 *)
val add_user_org : t -> string -> string -> bool

(**
 * [remove_user_org t u o] removes user [u] from organization [o].
 *)
val remove_user_org : t -> string -> string -> bool

(**
 * [add_message t m] adds message [m] to [t].
 *)
val add_message : t -> message -> bool

(**
 * [vote_poll t o c p op] increments option [op] of poll with name [p] in
 * channel with name [c] in organization with name [o] vote count by 1.
 *)
val vote_poll : t -> string -> string -> string -> string -> bool

(**
 * [add_channel t o c u pub] adds channel [c] to organization [o]. The creating
 * user is [u], and the [pub] is true if this channel is public, false
 * otherwise.
 *)
val add_channel : t -> string -> string -> string -> bool -> bool

(**
 * [remove_channel t o c] removes channel [c] from organization [o].
 *)
val remove_channel : t -> string -> string -> bool

(** [join_channel c u o] adds user [u] to channel [c] in organization o. *)
val join_channel : string -> string -> string -> bool

(**
 * [add_org t o a] adds organization [o] with admin [a].
 *)
val add_org : t -> string -> string -> bool

(**
 * [remove_org t o] removes organization [o].
 *)
val remove_org : t -> string -> bool

