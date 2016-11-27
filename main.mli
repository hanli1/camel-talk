open Parser
open Lwt

(**
 * The current_state type contains the particular organization/channel
 * as well as the user id.
 *)
type current_state = {
  mutable current_org : string option;
  mutable current_channel : string option;
  mutable current_user : string;
  mutable current_screen : Parser.screen;
  mutable logged_out : bool
}

(*
 * Continuously called to refresh for new messages, repaints the terminal
 * to display new current messages
 *)
val draw_update : current_state -> unit Lwt.t

(*
 * Prompts the user to enter in username and password information as the
 * first thing when the program is run.
 *)
val login : unit -> unit

(*
 * User has a choice to also register and stores account information in
 * the server
 *)
val register : unit -> unit

(*
 * Contains the REPL
 *)
val main : current_state -> unit Lwt.t