open Main

let json_to_list json =
  let open Yojson.Basic.Util in
  json |> to_list
(*
 * higher order function for getting a JSON Member and casting it to the
 * convert_type
 *)
let get_member json member_name convert_type =
  let open Yojson.Basic.Util in
  json |> member member_name |> convert_type

(*
 * gets a string property of the JSON
 *)
let get_member_string json member_name =
  let open Yojson.Basic.Util in
  get_member json member_name to_string

(*
 * gets an int property of the JSON
 *)
let get_member_int json member_name =
  let open Yojson.Basic.Util in
  get_member json member_name to_int

(*
 * gets a list property of the JSON
 *)
let get_member_list json member_name =
  let open Yojson.Basic.Util in
  get_member json member_name to_list

(*
 * gets a string list property of the JSON
 *)
let get_member_list_of_string json member_name =
  let open Yojson.Basic.Util in
  List.map to_string (get_member json member_name to_list)

let get_current_org state =
  match state.current_org with
  | None -> "Not part of organization"
  | Some i -> i

let rec render_channels_list_helper channels_lst=
  match channels_lst with
  | [] -> ()
  | h::t ->
  ANSITerminal.(print_string [green] (" | " ^ h ^ " | "));
  render_channels_list_helper t

let render_channels_list state resp_obj =
  ANSITerminal.(print_string [blue] ("Current organization: " ^ get_current_org state));
  render_channels_list_helper (get_member_list_of_string resp_obj "team_channels");
  render_channels_list_helper (get_member_list_of_string resp_obj "private_channels")

let print_linebreak _ =
  ANSITerminal.(print_string [] "___________________________________\n")

let print_newline _ =
  ANSITerminal.(print_string [] "\n")

let rec print_votes lst =
  match lst with
  | [] -> ()
  | h::t ->
    let choice = get_member_string h "name" in
    let count = get_member_int h "count" in
    ANSITerminal.(print_string [] (choice ^ ": " ^ (string_of_int count) ^ ", "));
    print_votes t

let render_message msg =
  let open Yojson.Basic.Util in
  let msg_type = get_member_string msg "message_type" in
  let user_id = get_member_string msg "user_id" in
  let time_stamp = get_member_string msg "time_stamp" in
  if msg_type = "simple" then begin
    ANSITerminal.(print_string [] (get_member_string msg "text"));
    ANSITerminal.(print_string [green] (user_id ^ "     " ^ time_stamp));
    print_linebreak ()
  end
  else if msg_type = "reminder" then begin
    ANSITerminal.(print_string [] ("Reminder set for " ^ (get_member_string msg "reminder")));
    ANSITerminal.(print_string [green] (user_id ^ "     " ^ time_stamp));
    print_linebreak ()
  end
  else begin
    ANSITerminal.(print_string [] (get_member_string msg "pollname"));
    print_newline ();
    print_votes (get_member_list msg "choices");
    ANSITerminal.(print_string [green] (user_id ^ "     " ^ time_stamp));
    print_linebreak ()
  end

let rec render_channel_messages_helper lst =
  match lst with
  | [] -> ()
  | h::t -> render_message h; render_channel_messages_helper t

let rec render_channel_messages state resp_obj =
  render_channel_messages_helper (json_to_list resp_obj)

