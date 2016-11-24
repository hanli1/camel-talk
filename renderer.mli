open Main
(**
 * render on the command line based on the current state [state] and a list
 * of channels
 *)
val render_channels_list : current_state -> string list -> unit

(**
 * render on the command line based on the current state [state] and a list
 * of messages
 *)
val render_channel_messages : current_state -> message list -> unit