open Main
(**
 * render on the command line based on the current state [state] and a list
 * of channels
 *)
val render_channels_list : string -> Yojson.Basic.json -> unit

val render_organizations_list : Yojson.Basic.json -> unit

(**
 * render on the command line based on the current state [state] and a list
 * of messages
 *)
val render_channel_messages : Yojson.Basic.json -> unit


