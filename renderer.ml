
open Main

let get_current_org state =
  match state.current_org with
  | None -> "Not part of organization"
  | Some i -> i

let rec render_channels_list_helper lst =
  match channels_lst with
  | [] -> ()
  | h::t ->
  ANSITerminal.(print_string [green] (" | " ^ h ^ " | "));
  render_channels_list_helper state t

let render_channels_list state channels_lst =
  ANSITerminal.(print_string [blue] ("Current organization: " ^ get_current_org state));
  render_channels_list_helper channels_lst

let print_linebreak _=
  ANSITerminal.(print_string "___________________________________")

let rec print_votes lst =
  match lst with
  | [] -> ()
  | (choice, count)::t -> ANSITerminal.(print_string (choice ^ ": " ^ (string_of_int count) ^ ", "));
  print_votes t

let render_message msg =
  match msg.body with
  | SimpleMessage text ->
    ANSITerminal.(print_string text);
    ANSITerminal.(print_string [green] (msg.user_id ^ "     " ^ msg.timestamp))
    print_linebreak ();
  | ReminderMessage (text, dur) ->
    ANSITerminal.(print_string (text ^ " set at " ^ (string_of_int dur)));
    ANSITerminal.(print_string [green] (msg.user_id ^ "     " ^ msg.timestamp))
    print_linebreak ();
  | PollMessage (text, vote_opts) ->
    ANSITerminal.(print_string text);
    print_votes vote_opts;
    ANSITerminal.(print_string [green] (msg.user_id ^ "     " ^ msg.timestamp))
    print_linebreak ();

let rec render_channel_messages state messages_lst =
  match messages_lst with
  | [] -> ()
  | h::t -> render_message h; render_channel_messages state t
