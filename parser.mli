(**
 * Values of type command represent user commands to the REPL.
 * CCreate - Creates an organization or public channel from the given string
 * CDelete - Deletes an organization or public channel specified by given 
 * string
 * CSwitch - Switch into the given organization or channel
 * CSimpleMessage - Sends a simple message (regular text)
 * CReminderMessage - Sends a message indicating that a reminder message
 * should be sent by the server/camel talk bot at a given time
 * CPollMessage - Sends a poll message with name and various options
 * CHelp - Displays a help menu for the user displaying syntax for commands
 * CBack - Returns the user to the previous screen
 * CIllegal - Represents an illegal command
 * CLogout - Logouts a user from his/her session
 * CQuit -  Quits a user out of the application
 * CInvite - Invites another user to an organization
 * CScrollUp - Scrolls up in a channel's message history
 * CScrollDown - Scrolls down in a channel's message history
 * CLeave - Allows a user to leave an organization
 * CVote - Allows the user to vote (select an option) in a given poll
 * CCreateDirectMessage - Creates a direct message channel between two users
 * CDirectMessage - Switch to a direct message channel between two users
 *)
type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CHelp
  | CBack
  | CIllegal
  | CLogout
  | CQuit
  | CInvite of string * string
  | CScrollUp
  | CScrollDown
  | CLeave of string * string
  | CVote of string * string
  | CCreateDirectMessage of string
  | CDirectMessage of string


type screen =
  | Messages
  | Channels
  | Organizations

(** text_to_message [cmd] is a command representing that message. *)
val text_to_message : string -> screen -> command