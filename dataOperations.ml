type message_body =   
  | SimpleMessage of string
  | ReminderMessage of string * int
  | PollMessage of string * ((string * int) list)

type message = {
  user_id : string;
  channel_id : string;
  organization_id : string;
  timestamp : int;
  body : message_body;
}

type channel = {
  name : string;
  organization_name : string;
  messages : message list;
  users : string list;
  is_public : bool;
}

type user = {
  username : string;
  password : string;
}

type organization = {
  name : string;
  channel_names : string list;
  users : string list;
  admin : string;
}

type channel_mutable = {
  name_mut : string;
  organization_name_mut : string;
  messages_mut : message DynArray.t;
  mutable users_mut : string list;
  is_public_mut : bool;
}

type user_mutable = {
  username_mut : string;
  mutable password_mut : string;
}

type organization_mutable = {
  name_mut : string;
  channels_mut : channel_mutable DynArray.t;
  mutable users_mut : string list;
  admin_mut : string;
}

type t = {
  users : user_mutable DynArray.t;
  organizations : organization_mutable DynArray.t;
}
(******************************************************************************)
(*                            Helper Functions                                *)
(******************************************************************************)
let get_org (orgs : organization_mutable DynArray.t) (orgname : string) : organization_mutable =
  let oidx = DynArray.index_of (fun org -> org.name_mut = orgname) orgs in
  DynArray.get orgs oidx

let get_chan (chans : channel_mutable DynArray.t) (channame : string) : channel_mutable =
  let cidx = DynArray.index_of (fun (chan : channel_mutable) -> chan.name_mut = channame) chans in
  DynArray.get chans cidx

(******************************************************************************)
(*                                                                            *)
(******************************************************************************)

let load_data () =
  failwith "Unimplemented"

let backup_data data =
  failwith "Unimplemented"

let get_user_list data =
  let ulist = DynArray.to_list data.users in
  List.map (fun u -> u.username_mut) ulist

let get_user_data data uid =
  try (
    let idx = DynArray.index_of (fun u -> u.username_mut = uid) data.users in
    let u = DynArray.get data.users idx in
    Some {username = u.username_mut; password = u.password_mut}
  ) with Not_found -> None

let get_org_list data =
  let olist = DynArray.to_list data.organizations in
  List.map (fun o -> o.name_mut) olist

let get_org_data data orgname =
  try (
    let org = get_org data.organizations orgname in
    let org_channels = org.channels_mut |> DynArray.to_list 
                                        |> List.map (fun (c : channel_mutable) -> c.name_mut)
    in
    Some {
      name=org.name_mut;
      channel_names=org_channels;
      users=org.users_mut;
      admin=org.admin_mut
    }
  ) with Not_found -> None

let get_channel_data data orgname channame =
  try (
    let org = get_org data.organizations orgname in
    let chan = get_chan org.channels_mut channame in
    let messages = DynArray.to_list chan.messages_mut in
    Some {
      name=chan.name_mut;
      organization_name=chan.organization_name_mut;
      messages=messages; users=chan.users_mut;
      is_public=chan.is_public_mut
    }
  ) with Not_found -> None

let add_user data uid p =
  try (
    let _ = DynArray.index_of (fun u -> u.username_mut = uid) data.users in
    false
  ) with Not_found -> (
    let new_user = {username_mut=uid; password_mut=p} in
    DynArray.add data.users new_user;
    true
  )

let remove_user data uid =
  try (
    let idx = DynArray.index_of (fun u -> u.username_mut = uid) data.users in
    DynArray.delete data.users idx;
    true
  ) with Not_found -> false

let change_user_pass data uid p =
  try (
    let idx = DynArray.index_of (fun u -> u.username_mut = uid) data.users in
    let user = DynArray.get data.users idx in
    user.password_mut <- p;
    true
  ) with Not_found -> false

let add_user_org data uid orgname =
  try (
    let _ = DynArray.index_of (fun u -> u.username_mut = uid) data.users in
    let org = get_org data.organizations orgname in

    if List.mem uid org.users_mut then
      false
    else (
      org.users_mut <- uid::org.users_mut;
      true
    )
  ) with Not_found -> false

let remove_user_org data uid orgname =
  let org = get_org data.organizations orgname in
  if List.mem uid org.users_mut then (
    org.users_mut <- List.filter (fun u -> u <> uid) org.users_mut;
    true
  )
  else false

let add_message data msg =
  try (
    let org = get_org data.organizations msg.organization_id in
    let chan = get_chan org.channels_mut msg.channel_id in
    DynArray.add chan.messages_mut msg;
    true
  ) with Not_found -> false

let vote_poll data oname cname pname op =
  try (
    let org = get_org data.organizations oname in
    let chan = get_chan org.channels_mut cname in

    let is_poll p m =
      match m.body with
      | PollMessage (name, _) -> name=p
      | _ -> false
    in

    let midx = DynArray.index_of (is_poll pname) chan.messages_mut in
    let msg = DynArray.get chan.messages_mut midx in

    let rec increment_opt opts opt =
      match opts with
      | [] -> raise Not_found
      | (name,votes)::t ->
        if name = opt then (name, votes+1)::t
        else (name,votes)::(increment_opt t opt)
    in

    let new_msg_body =
      match msg.body with
      | PollMessage (pid, opts) -> PollMessage (pid, increment_opt opts op)
      | _ -> raise Not_found
    in

    let new_msg = {msg with body=new_msg_body} in
    DynArray.set chan.messages_mut midx new_msg;
    true
  ) with Not_found -> false

let add_channel data oname cname u pub =
  let org = get_org data.organizations oname in
  try (
    let _ = get_chan org.channels_mut cname in
    false
  ) with Not_found -> (
    let new_channel = {
      name_mut=cname;
      organization_name_mut=oname;
      messages_mut=DynArray.create ();
      users_mut=[u];
      is_public_mut=pub
    }
    in
    DynArray.add org.channels_mut new_channel;
    true
  )

let remove_channel data oname cname =
  try (
    let org = get_org data.organizations oname in
    let cidx = DynArray.index_of (fun (c : channel_mutable) -> c.name_mut = cname) org.channels_mut in
    DynArray.delete org.channels_mut cidx;
    true
  ) with Not_found -> false

let join_channel data c u o =
  try (
    let org = get_org data.organizations o in
    if List.mem u org.users_mut then (
      let chan = get_chan org.channels_mut c in
      if List.mem u chan.users_mut then (
        false
      ) else (
        chan.users_mut <- u::chan.users_mut;
        true
      )
    ) else (
      false
    )
  ) with Not_found -> false

let leave_channel data c uid o =
  try (
    let org = get_org data.organizations o in
    let chan = get_chan org.channels_mut c in
    if List.mem uid chan.users_mut then (
      chan.users_mut <- List.filter (fun u -> u <> uid) chan.users_mut;
      true
    ) else (
      false
    )
  ) with Not_found -> false


let add_org data orgname adname =
  try (
    let _ = get_org data.organizations orgname in
    false
  ) with Not_found -> (
    try (
      let _ = DynArray.index_of (fun u -> u.username_mut = adname) data.users in
      let new_org = {
        name_mut=orgname;
        channels_mut=DynArray.create ();
        users_mut=[adname];
        admin_mut=adname
      }
      in
      DynArray.add data.organizations new_org;
      true
    ) with Not_found -> false
  )

let remove_org data orgname =
  try (
    let oidx = DynArray.index_of (fun o -> o.name_mut = orgname) data.organizations in
    DynArray.delete data.organizations oidx;
    true
  ) with Not_found -> false