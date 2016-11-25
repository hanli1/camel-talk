type message_body =   
  | SimpleMessage of string
  | ReminderMessage of string * int
  | PollMessage of string * ((string * int) list)

type message = {
  user_id : string;
  timestamp : int;
  body : message_body;
}

type channel = {
  name : string;
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
(*                              Helper Functions                              *)
(******************************************************************************)
let get_org (orgs : organization_mutable DynArray.t)
            (orgname : string) : organization_mutable =
  let oidx = DynArray.index_of (fun org -> org.name_mut = orgname) orgs in
  DynArray.get orgs oidx

let get_chan (chans : channel_mutable DynArray.t)
             (channame : string) : channel_mutable =
  let cidx = DynArray.index_of
    (fun (chan : channel_mutable) -> chan.name_mut = channame)
    chans
  in
  DynArray.get chans cidx

(******************************************************************************)
(*                         Text Database Interaction                          *)
(******************************************************************************)

let init_data () : unit =
  let open Sys in
  let _ =
    if file_exists "database" && is_directory "database" then
      command "rm -rf database/"
    else 0
  in
  let _ = command "mkdir database/" in
  let _ = command "touch database/users.txt" in
  ()

let read_data () : t =
  let open Unix in
  let open Sys in

  let users = DynArray.create () in
  let organizations = DynArray.create () in

  let line_to_fields (line : string) : string list =
    let open Str in
    split (regexp "[\t]+") line
  in

  let field_to_values (field : string) : string list =
    let open Str in
    split (regexp "[;]+") field
  in

  let ends_with (s1 : string) (s2 : string) : bool =
    let s2len = String.length s2 in
    (String.sub s1 (String.length s1 - s2len) s2len) = s2
  in

  let rec collect_users (darr : user_mutable DynArray.t)
                        (fh : in_channel) : unit =
    try (
      let line = input_line fh in
      let fields = line_to_fields line in
      let new_user =
        match fields with
        | username::password::[] -> {
          username_mut=username;
          password_mut=password;
        }
        | _ -> failwith "badly formed line"
      in
      DynArray.add darr new_user;
      collect_users darr fh
    ) with End_of_file -> close_in fh
  in

  let add_channel fh channels =
    let channel_info = line_to_fields (input_line fh) in
    let (name, is_public, users) =
      match channel_info with
      | n::p::us::[] ->
        (n, bool_of_string p, field_to_values us)
      | _ -> failwith "badly formed channel info"
    in
    let done_iter = ref false in
    let messages = DynArray.create () in
    while not !done_iter do
      try (
        let new_fields = line_to_fields (input_line fh) in
        let (uid, time, body) =
          match new_fields with
          | u::t::mtype::binfo ->
            let b =
              if mtype = "SimpleMessage" then (
                match binfo with
                | text::[] -> SimpleMessage text
                | _ -> failwith "badly formed SimpleMessage"
              ) else if mtype = "ReminderMessage" then (
                match binfo with
                | text::rtime::[] -> ReminderMessage (text, int_of_string rtime)
                | _ -> failwith "badly formed ReminderMessage"
              ) else if mtype = "PollMessage" then (
                match binfo with
                | text::optvotes::[] ->
                  let optvotelist = field_to_values optvotes in
                  let optvotepairs = List.map
                    (fun ovl -> 
                      let pipe = String.index ovl '|' in
                      let opt = String.sub ovl 0 pipe in
                      let num = String.sub ovl (pipe+1)
                                               (String.length ovl - (pipe+1)) in
                      (opt, int_of_string num)
                    )
                    optvotelist
                  in
                  PollMessage (text, optvotepairs)
                | _ -> failwith "badly formed PollMessage"
              ) else (
                failwith "badly formed message type"
              )
            in
            (u, int_of_string t, b)
          | _ -> failwith "badly formed message"
        in
        DynArray.add messages {user_id=uid; timestamp=time; body=body}
      ) with End_of_file -> (
        done_iter := true; close_in fh
      )
    done;
    DynArray.add channels {
      name_mut=name;
      is_public_mut=is_public;
      users_mut=users;
      messages_mut=messages;
    }
  in


  let add_channels channels dirname =
    let files = Array.to_list (readdir dirname) in
    let chan_files = files
      |> List.map (fun x -> dirname^x)
      |> List.filter (fun x -> not (is_directory x)
                               && String.length x > String.length "_channel.txt"
                               && ends_with x "_channel.txt")
    in
    List.iter (fun fn -> add_channel (open_in fn) channels) chan_files
  in


  let add_org organizations prefix (dn : string) : unit =
    let dirname = prefix^dn^"/" in
    if ends_with dirname "_org/" && is_directory dirname then (
      let org_info_fh = open_in (dirname ^ "org_info.txt") in
      let org_info = line_to_fields (input_line org_info_fh) in
      close_in org_info_fh;
      let (name, admin, users) =
        match org_info with
        | n::a::us::[] ->
          (n, a, field_to_values us)
        | _ -> failwith "badly formed org_info.txt"
      in
      let channels = DynArray.create () in

      add_channels channels dirname;

      let new_org = {
        name_mut=name;
        admin_mut=admin;
        users_mut=users;
        channels_mut=channels;
      }
      in
      DynArray.add organizations new_org
    )
    else ()
  in

  collect_users users (open_in "database/users.txt");

  let dbfiles = readdir "database/" in
  Array.iter (add_org organizations "database/") dbfiles;

  {users=users; organizations=organizations}

let load_data () =
  try (
    read_data ()
  ) with exn -> (
    init_data ();
    read_data ()
  )

let backup_data data =
  failwith "Unimplemented"

(******************************************************************************)
(*                            Type t Manipulation                             *)
(******************************************************************************)

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
    let org_channels =
      org.channels_mut 
      |> DynArray.to_list 
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
      messages=messages;
      users=chan.users_mut;
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

let add_message data oname cname uid msg_body =
  try (
    let org = get_org data.organizations oname in
    if List.mem uid org.users_mut then
      let chan = get_chan org.channels_mut cname in
      let new_message = {
        user_id=uid;
        timestamp=int_of_float (Unix.time ());
        body=msg_body;
      }
      in
      DynArray.add chan.messages_mut new_message;
      true
    else
      false
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
    let cidx = DynArray.index_of
      (fun (c : channel_mutable) -> c.name_mut = cname)
      org.channels_mut
    in
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
    let oidx = DynArray.index_of
      (fun o -> o.name_mut = orgname)
      data.organizations
    in
    DynArray.delete data.organizations oidx;
    true
  ) with Not_found -> false
