type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CBack
  | CIllegal

type screen =
  | Messages
  | Channels
  | Organizations


let parse_string str =
  let first_space_index = String.index str ' ' in
  let keyword = String.sub str 0 first_space_index in
  let str_length = String.length str in
  let text = String.sub str (first_space_index + 1) (str_length - first_space_index + 1) in
  (keyword, text)

let parse_string_to_list str =
  Str.split (Str.regexp " ") str

let parse_message_string str =
  let first_space_index = String.index str ' ' in
  let keyword = String.sub str 0 first_space_index in
  let str_length = String.length str in
  let text = String.sub str (first_space_index + 1) (str_length - first_space_index + 1) in
  let text_length = String.length text in
  if keyword = "set_reminder" then
    let second_space_index = String.index text ' ' in
    let duration = String.sub text 0 second_space_index in
    let reminder = String.sub text (second_space_index + 1) (text_length - second_space_index + 1) in
    CReminderMessage (reminder, (int_of_string duration))
  else if keyword = "set_poll" then
    (* let left_bracket_index = String.index text '[' in *)
    let right_bracket_index = String.index text ']' in
    let lst_string = String.sub text 1 (right_bracket_index) in
    let question = String.sub text (right_bracket_index + 2) (text_length - right_bracket_index -2) in
    CPollMessage (question, (parse_string_to_list lst_string))
  else CSimpleMessage text (* default to normal message *)

let parse_messages_screen str =
  parse_message_string str

let parse_channels_screen str =
  let (keyword, text) = parse_string str in
  if keyword = "create" then CCreate text
  else if keyword = "delete" then CDelete text
  else CSwitch text

let parse_organizations_screen str =
  let (keyword, text) = parse_string str in
  if keyword = "create" then CCreate text
  else if keyword = "delete" then CDelete text
  else CSwitch text



(** text_to_message [cmd] is a command representing that message. *)
let text_to_message str screen=
  if str = "back" then CBack
  else
  match screen with
  | Messages -> parse_messages_screen str
  | Channels -> parse_channels_screen str
  | Organizations -> parse_organizations_screen str