
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

let get_width _ =
  fst (ANSITerminal.size())
let get_height _ =
  snd (ANSITerminal.size())
let cursor_x _ =
  fst (ANSITerminal.pos_cursor())
let cursor_y _ =
  snd (ANSITerminal.pos_cursor())

let rec gen_line str_dup str count =
  if count = 0 then str
  else gen_line str_dup (str ^ str_dup) (count - 1)

let print_across_screen str_dup =
  ANSITerminal.(print_string [] (gen_line str_dup "" (get_width ())));
  print_newline() (* WTF WHY DOES THIS LINE FIX FORMATTING??!*)

(* let print_linebreak _ =
  print_newline();
  print_across_screen "_";
  print_newline() *)

let print_newline _ =
  ANSITerminal.(print_string [] "\n")

(* let get_current_org state =
  match state.current_org with
  | None -> "Not part of organization"
  | Some i -> i
 *)
let rec render_channels_list_helper channels_lst=
  match channels_lst with
  | [] -> ()
  | h::[] -> ANSITerminal.(print_string [green] (" | " ^ h ^ " | "));
  | h::t ->
  ANSITerminal.(print_string [green] (" | " ^ h));
  render_channels_list_helper t

let render_channels_list curr_org resp_obj =
  ANSITerminal.(print_string [blue] ("Current organization: " ^ curr_org));
  print_newline();
  ANSITerminal.(print_string [green] ("Public Channels -> "));
  render_channels_list_helper (get_member_list_of_string resp_obj "team_channels");
  print_newline();
  ANSITerminal.(print_string [green] ("Private Channels -> "));
  render_channels_list_helper (get_member_list_of_string resp_obj "private_channels");
  print_newline();
  flush_all ()

let render_organizations_list resp_obj =
  ANSITerminal.(print_string [blue] ("Organizations list: "));
  print_newline();
  render_channels_list_helper (get_member_list_of_string resp_obj "organizations");
  print_newline();
  flush_all ()


let rec print_votes lst =
  match lst with
  | [] -> ()
  | h::[] ->
    let choice = get_member_string h "name" in
    let count = get_member_int h "count" in
    ANSITerminal.(print_string [] (choice ^ ": " ^ (string_of_int count)))
  | h::t ->
    let choice = get_member_string h "name" in
    let count = get_member_int h "count" in
    ANSITerminal.(print_string [] (choice ^ ": " ^ (string_of_int count) ^ "\n"));
    print_votes t

let set_and_print x y styles text =
  (* ANSITerminal.save_cursor(); *)
  ANSITerminal.set_cursor x y;
  ANSITerminal.(print_string [green] text)
  (* ANSITerminal.restore_cursor() *)

let print_meta_data name time =
  let divider = "─" in
  print_across_screen divider;
  ANSITerminal.(print_string [green] ("| "^name));
  let time_length = String.length time in
  set_and_print (get_width() - time_length - 1) (get_height()) [] (time ^ " |");
  print_across_screen divider


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
    ANSITerminal.(print_string [] ("Reminder set for " ^ (get_member_string mess "content") ^" for " ^ (get_member_string mess "time")));
    print_newline ()
  end
  else begin
    print_meta_data user_id time_stamp;
    ANSITerminal.(print_string [] (get_member_string mess "content"));
    print_newline ();
    print_votes (get_member_list mess "options");
    print_newline ()
  end

let rec render_channel_messages_helper lst =
  match lst with
  | [] -> ()
  | h::t -> render_message h; render_channel_messages_helper t

let render_channel_messages resp_obj =
  let open Yojson.Basic.Util in
  render_channel_messages_helper (json_to_list (resp_obj |> member "messages"));
  flush_all ()


