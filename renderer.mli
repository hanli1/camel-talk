(**
 * [render_org_info org user resp] renders on the command line the information 
 * regarding a single organization: the public channels, the private channels, 
 * and the users in the organization
 *)
val render_org_info : string -> string -> Yojson.Basic.json -> unit

(**
 * [render_organizations_list resp] renders on the command line all the 
 * organizations the user is currently a member of
 *)
val render_organizations_list : Yojson.Basic.json -> unit

(**
 * [render_channel_messages status_message org channel resp] renders on the 
 * command line some messages of the channel the user has switched into
 *)
val render_channel_messages : string -> string -> string -> Yojson.Basic.json -> unit


