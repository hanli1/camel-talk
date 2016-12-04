(**
 * Value of type message_body contains constructors for the different types of
 * message bodies.
 *
 * Formats:
 * SimpleMessage [text]
 * ReminderMessage ([text], [time])
 * PollMessage ([id], [question], [(option1, votes1), ..., (optionx, votesx)])
 *)
type message_body =
  | SimpleMessage of string
  | ReminderMessage of string * int
  | PollMessage of string * string * ((string * int) list)

(**
 * Value of type message contains the sending user, the timestamp, and the
 * message body.
 *)
type message = {
  user_id : string;
  timestamp : int;
  body : message_body;
}

(**
 * Value of type channel represents the data associated with a single channel:
 * the name of the channel, the number of messages, the users in that channel
 * (relevant to private channels) and whether the channel is public.
 *)
type channel = {
  name : string;
  message_count : int;
  users : string list;
  is_public : bool;
}

(**
 * Value of type user represents the data associated with a single user, which
 * is simply the username and password.
 *)
type user = {
  username : string;
  password : string;
}

(**
 * Value of type organization represents the data associated with a single
 * organization: its name, the list of channels names within it, the list
 * of users in the organization, and the admin.
 *)
type organization = {
  name : string;
  channel_names : string list;
  users : string list;
  admin : string;
}

(** The type of the in-memory data store. *)
type t

(** [make_data ()] is am empty data store, with no users or organizations. *)
val make_data : unit -> t

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

(**
 * [get_user_data t uid] is the user data for user with user_id [uid],
 * returned as an option. None if this user does not exist.
 *)
val get_user_data : t -> string -> user option

(** [get_org_list t] is the list of organization names. *)
val get_org_list : t -> string list

(**
 * [get_org_data t o] is the data for organization with name [o]. 
 * Returned as an option. None if this organization does not exist.
 *)
val get_org_data : t -> string -> organization option

(**
 * [get_channel_data o c] is the channel data for channel with name c
 * in organization with name o, returned as an option. None if the
 * channel does not eist in that organization.
 *)
val get_channel_data : t -> string -> string -> channel option

(**
 * [get_recent_msg o c t x n] fetches [n] messages starting at the [x]th most
 * recent messages, 0-indexed at channel [c] in organization [o]. The most
 * recent message is at index 0. The messages are returned in order from most
 * recent to least recent.
 *)
val get_recent_msg : t -> string -> string -> int -> int -> message list option

(** [add_user t u p] adds a user with username [u] and password [p]. *)
val add_user : t -> string -> string -> bool

(** [remove_user t u] removes user [u] from t. [u]'s messages stay intact. *)
val remove_user : t -> string -> bool

(** [change_user_pass t u p] changes user [u]'s password to [p]. *)
val change_user_pass : t -> string -> string -> bool

(** [add_user_org t u o] adds organization [o] to user [u]. *)
val add_user_org : t -> string -> string -> bool

(** [remove_user_org t u o] removes user [u] from organization [o]. *)
val remove_user_org : t -> string -> string -> bool

(**
 * [add_message t org chan uid msg_body] adds a message with user [uid],
 * and body [msg_body] to channel [chan] in organization [org]. The
 * timestamp is the time that this function is applied.
 *
 * Special note: When sending a PollMessage, the id field can be any
 * arbitrary string. It will be replaced with the autoincremented poll id
 * before being added to the channel's messages.
 *)
val add_message : t -> string -> string -> string -> message_body -> bool

(**
 * [vote_poll t o c id op] increments option [op] of poll with id [id] in
 * channel with name [c] in organization with name [o].
 *)
val vote_poll : t -> string -> string -> string -> string -> bool

(**
 * [add_channel t o c pub] adds channel [c] to organization [o]. [pub] is true
 * if this channel is public, false if private.
 *)
val add_channel : t -> string -> string -> bool -> bool

(** [remove_channel t o c] removes channel [c] from organization [o]. *)
val remove_channel : t -> string -> string -> bool

(** [join_channel t c u o] adds user [u] to channel [c] in organization [o]. *)
val join_channel : t -> string -> string -> string -> bool

(**
 * [leave_channel t c u o] removes user [u] from channel [c] in organization [o]
 *)
val leave_channel : t -> string -> string -> string -> bool

(** [add_org t o a] adds organization [o] with admin [a]. *)
val add_org : t -> string -> string -> bool

(** [remove_org t o] removes organization [o]. *)
val remove_org : t -> string -> bool

(**
 * [add_reminder t o c content time] sets a reminder [content] for time
 * [time] in organizaiton [o], channel [c].
 *)
val add_reminder : t -> string -> string -> string -> int -> bool

(**
 * [flush_reminders ()] removes any reminders that exceed their reminder
 * time and sends their corresponding message to the channel as a
 * ReminderMessage.
 *)
val flush_reminders : t -> bool
