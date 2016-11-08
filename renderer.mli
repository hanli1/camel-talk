(*
 * render on the command line based on the current state [state] and a list
 * of channels
 *)
val render_channels_list : state -> string list -> unit

(*
 * render on the command line based on the current state [state] and a list
 * of messages
 *)
val render_channel_messages : state -> message list -> unit