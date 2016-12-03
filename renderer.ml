open Unix
open Str

(**
 * convert json into a list of jsons
 *)
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

(**
 * gets the width of the terminal
 *)
let get_width _ =
  fst (ANSITerminal.size())

(**
 * gets the height of the terminal
 *)
let get_height _ =
  snd (ANSITerminal.size())

(**
 * gets the x location of the cursor
 *)
let cursor_x _ =
  fst (ANSITerminal.pos_cursor())

(**
 * gets the y loctaion of the cursor
 *)
let cursor_y _ =
  snd (ANSITerminal.pos_cursor())

(**
 * [gen_line str_dup str count] generates a string [str] by duplicating a string
 * [str_dup] a number [count] of times
 *)
let rec gen_line str_dup str count =
  if count = 0 then str
  else gen_line str_dup (str ^ str_dup) (count - 1)

(**
 * [print_across_screen str_dup] takes in a string [str_dup] to duplicate and
 * prnit across the width of the terminal
 *)
let print_across_screen str_dup =
  ANSITerminal.(print_string [] (gen_line str_dup "" (get_width ())));
  print_newline()

(**
 * prints the bottom half of a rectangle
 *)
let print_bottom_rectangle str_dup =
  ANSITerminal.(print_string [] ("└"));
  ANSITerminal.(print_string [] (gen_line str_dup "" (get_width () - 2 )));
  ANSITerminal.(print_string [] ("┘"));
  print_newline()

(**
 * a helper method for rendering all of the channels in a list
 *)
let rec render_channels_list_helper channels_lst=
  match channels_lst with
  | [] -> ANSITerminal.(print_string [green] ("--"));
  | h::[] -> ANSITerminal.(print_string [green] (" | " ^ h ^ " | "));
  | h::t ->
  ANSITerminal.(print_string [green] (" | " ^ h));
  render_channels_list_helper t

(**
 * [render_org_info] takes in an string [curr_org], a user [current_user], and
 * a yojson [resp_obj] and displays the information about the organization
 *)
let render_org_info curr_org current_user resp_obj =
  ANSITerminal.(print_string [blue] ("Current Organization: " ^ curr_org));
  print_newline();
  print_across_screen "─";
  ANSITerminal.(print_string [green] ("Public Channels"));
  print_newline();
  render_channels_list_helper (get_member_list_of_string resp_obj "team_channels");
  print_newline();
  print_across_screen "─";
  ANSITerminal.(print_string [green] ("Private Channels"));
  print_newline();
  render_channels_list_helper (List.map (
    fun c ->
      let channel_components = split (regexp_string "@") c in
      let first_user = List.nth channel_components 1 in
      let second_user = List.nth channel_components 2 in
      if current_user = first_user then second_user else first_user
  ) (get_member_list_of_string resp_obj "private_channels"));
  print_newline();
  print_across_screen "─";
  ANSITerminal.(print_string [green] ("Users in Organization:"));
  print_newline();
  render_channels_list_helper (get_member_list_of_string resp_obj "users");
  print_newline();
  print_across_screen "─";
  flush_all ()

(**
 * [render_organizations_list] takes in a json [resp_obj] and displays the
 * list of organizations that a user has
 *)
let render_organizations_list resp_obj =
  print_across_screen "─";
  ANSITerminal.(print_string [blue] ("Organizations:"));
  print_newline();
  render_channels_list_helper (get_member_list_of_string resp_obj "organizations");
  print_newline();
  flush_all ()

(**
 * [print_votes lst] takes in a choices [lst] and displays a formatted version
 * of a poll
 *)
let rec print_votes lst =
  match lst with
  | [] -> ()
  | h::[] ->
    let choice = get_member_string h "option" in
    let count = get_member_int h "count" in
    ANSITerminal.(print_string [] (choice ^ ": " ^ (string_of_int count)))
  | h::t ->
    let choice = get_member_string h "option" in
    let count = get_member_int h "count" in
    ANSITerminal.(print_string [] (choice ^ ": " ^ (string_of_int count) ^ "\n"));
    print_votes t

(**
 * prints things at x and y on screen
 *)
let set_and_print x y styles text =
  (* ANSITerminal.save_cursor(); *)
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [green] text)
  (* ANSITerminal.restore_cursor() *)

(**
 * [fill_zero num] takes in a int [num] and returns a string that is always 2
 * characters long
 *)
let fill_zero num =
  let num = string_of_int num in
  if String.length num = 1 then "0"^num
  else num

(**
 * [print_meta_data name time] prints the header data for each message with
 * the author [name] and the date posted [time]
 *)
let print_meta_data name time =
  let date_rec = Unix.localtime (float_of_string time) in
  let time = string_of_int (date_rec.tm_mon +1) ^ "/" ^ string_of_int date_rec.tm_mday ^ "/" ^ string_of_int (date_rec.tm_year+1900)
    ^ " " ^ fill_zero date_rec.tm_hour ^ ":"^ fill_zero date_rec.tm_min ^ ":" ^ fill_zero date_rec.tm_sec in
  let divider = "─" in
  print_across_screen divider;
  ANSITerminal.(print_string [green] ("| "^name));
  let time_length = String.length time in
  set_and_print (get_width() - time_length - 1) (get_height()) [] (time ^ " |")
  (* print_across_screen divider *)

(**
 * [render_message msg] takes in a message [msg] and displays it
 *)
let render_message msg =
  let open Yojson.Basic.Util in
  let msg_type = get_member_string msg "message_type" in
  let user_id = get_member_string msg "user_id" in
  let time_stamp = get_member_string msg "time_stamp" in
  let mess = msg |> member "message" in
  if msg_type = "simple" then begin
    print_meta_data user_id time_stamp;
    ANSITerminal.(print_string [] (get_member_string mess "content"));
    print_newline ()
  end
  else if msg_type = "reminder" then begin
    print_meta_data user_id time_stamp;
    ANSITerminal.(print_string [] ((get_member_string mess "time") ^ " reminder: " ^ (get_member_string mess "content")));
    print_newline ()
  end
  else begin
    print_meta_data user_id time_stamp;
    ANSITerminal.(print_string [] (get_member_string mess "content"));
    print_newline ();
    print_votes (get_member_list mess "options");
    print_newline ()
  end

(**
 * [render_channel_messages_helper lst] takes in a list of messages [lst] and
 * displays each message one by one
 *)
let rec render_channel_messages_helper lst =
  match lst with
  | [] -> ()
  | h::t -> render_message h; render_channel_messages_helper t

(**
 * [render_channel_messages status_message curr_org curr_channel resp_obj] takes
 * in data about a channel's messages and info about the channel itself and
 * displays them on the screen.
 *)
let render_channel_messages status_message curr_org curr_channel resp_obj =
  let () =
    if (String.length curr_channel >= 14) && (String.sub curr_channel 0 14) =
    "directmessage@" then
      (ANSITerminal.(print_string [magenta]
      ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ^
      "~~~~~~~~\n" ^ (status_message) ^ "\nCurrent organization: " ^ curr_org ^
      "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ^
      "~~~~~~~~~\n")))
    else
      (ANSITerminal.(print_string [magenta]
      ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ^
      "~~~~~~~~\n" ^ (status_message) ^ "\nCurrent organization: " ^ curr_org ^
      " | Current channel: "^curr_channel ^"\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ^
      "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")))
  in
  let open Yojson.Basic.Util in
  render_channel_messages_helper (json_to_list (resp_obj |> member "messages"));
  flush_all ()


