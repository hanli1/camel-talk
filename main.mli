include Yojson

(* The current_state type contains the particular organization/channel
as well as the user id. *)
type current_state = 
{
  current_org : string option;
  current_channel : string option;
  current_user : string;
}

(* Continuously called to refresh for new messages, repaints the terminal 
to display new current messages *)
val draw_update : current_state -> unit

(* Prompts the user to enter in username and password information as the
first thing when the program is run. *)
val prompt_login : -> string -> string -> bool

(* User has a choice to also register and stores account information in
the server *)
val register : string -> string -> bool

(* Contains the REPL *)
val main : unit -> unit