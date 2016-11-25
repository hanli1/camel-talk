
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

(* let get_current_org state =
  match state.current_org with
  | None -> "Not part of organization"
  | Some i -> i
 *)
let rec render_channels_list_helper channels_lst=
  match channels_lst with
  | [] -> ()
  | h::t ->
  ANSITerminal.(print_string [green] (" | " ^ h ^ " | "));
  render_channels_list_helper t

let render_channels_list curr_org resp_obj =
  ANSITerminal.(print_string [blue] ("Current organization: " ^ curr_org));
  render_channels_list_helper (get_member_list_of_string resp_obj "team_channels");
  render_channels_list_helper (get_member_list_of_string resp_obj "private_channels")

let get_width _ =
  fst (ANSITerminal.size())
let get_height _ =
  snd (ANSITerminal.size())

let rec gen_line str count =
  if count = 0 then str
  else gen_line (str ^ "_") (count - 1)
let print_linebreak _ =
  ANSITerminal.(print_string [] ("\n"^ (gen_line "" (get_width ())) ^"\n"))

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

let print_meta_data name time =
  ANSITerminal.(print_string [green] name);
  let time_length = String.length time in
  ANSITerminal.set_cursor (get_width() - time_length) (snd(ANSITerminal.pos_cursor()));
  ANSITerminal.(print_string [green] time)

let render_message msg =
  let open Yojson.Basic.Util in
  let msg_type = get_member_string msg "message_type" in
  let user_id = get_member_string msg "user_id" in
  let time_stamp = get_member_string msg "time_stamp" in
  let mess = msg |> member "message" in
  if msg_type = "simple" then begin
    print_meta_data user_id time_stamp;
    print_newline();
    ANSITerminal.(print_string [] (get_member_string mess "text"));
    print_linebreak ()
  end
  else if msg_type = "reminder" then begin
    print_meta_data user_id time_stamp;
    print_newline();
    ANSITerminal.(print_string [] ("Reminder set for " ^ (get_member_string mess "reminder")));
    print_linebreak ()
  end
  else begin
    print_meta_data user_id time_stamp;
    print_newline();
    ANSITerminal.(print_string [] (get_member_string mess "pollname"));
    print_newline ();
    print_votes (get_member_list mess "choices");
    print_linebreak ()
  end

let rec render_channel_messages_helper lst =
  match lst with
  | [] -> ()
  | h::t -> render_message h; render_channel_messages_helper t

let rec render_channel_messages resp_obj =
  render_channel_messages_helper (json_to_list resp_obj)

