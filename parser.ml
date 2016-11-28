(* CInvite, #invite person organizatino
  CVote, #vote choice1 poll *)

type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CHelp
  | CBack
  | CIllegal
  | CLogout
  | CInvite of string * string
  | CScrollUp
  | CScrollDown
  | CLeave of string * string
  | CVote of string * string

type screen =
  | Messages
  | Channels
  | Organizations


let parse_string str =
  let lst = Str.split (Str.regexp " ") str in
  match lst with
  | [] -> failwith "Blank string"
  | h::[] -> (h, "")
  | h::t -> begin
    let first_space_index = String.index str ' ' in
    let keyword = String.sub str 0 first_space_index in
    let str_length = String.length str in
    let text = String.sub str (first_space_index + 1) (str_length - first_space_index - 1) in
    (keyword, text)
    end

let parse_string_to_list str =
  Str.split (Str.regexp ",") str

let parse_message_string str =
  try
    let first_space_index = String.index str ' ' in
    let keyword = String.sub str 0 first_space_index in
    let str_length = String.length str in
    let text = String.sub str (first_space_index + 1) (str_length - first_space_index - 1) in
    let text_length = String.length text in
    if keyword = "#set_reminder" then
      let second_space_index = String.index text ' ' in
      let duration = String.sub text 0 second_space_index in
      let reminder = String.sub text (second_space_index + 1) (text_length - second_space_index - 1) in
      try
      let dur = int_of_string duration in
      if reminder <> "" then
      CReminderMessage (reminder, dur)
      else
      CIllegal
      with
      | _ -> CIllegal
    else if keyword = "#set_poll" then
      (* let left_bracket_index = String.index text '[' in *)
      let right_bracket_index = String.index text ']' in
      let lst_string = String.sub text 1 (right_bracket_index - 1) in
      let question = String.sub text (right_bracket_index + 2) (text_length - right_bracket_index - 2) in
      CPollMessage (question, (parse_string_to_list lst_string))
    else if keyword = "#vote" then begin
      let (h, t) = parse_string text in
      if h <> "" && t <> "" then CVote (h, t)
      else CIllegal
    end
    else CSimpleMessage str (* default to normal message *)
  with
  | _ -> if String.get str 0 = '#' then CIllegal else CSimpleMessage str

let parse_messages_screen str =
  parse_message_string str

let parse_channels_screen str =
  let (keyword, text) = parse_string str in
  if keyword = "#create" then (if text <> "" then CCreate text else CIllegal)
  else if keyword = "#delete" then (if text <> "" then CDelete text else CIllegal)
  else CSwitch str

let parse_organizations_screen str =
  let (keyword, text) = parse_string str in
  if keyword = "#create" then (if text <> "" then CCreate text else CIllegal)
  else if keyword = "#delete" then (if text <> "" then CDelete text else CIllegal)
  else if keyword = "#invite" then begin
    let (h, t) = parse_string text in
    if h <> "" && t <> "" then CInvite (h, t)
    else CIllegal
  end
  else if keyword = "#leave" then begin
    let (h, t) = parse_string text in
    if h <> "" && t <> "" then CLeave (h, t)
    else CIllegal
  end
  else CSwitch str



(** text_to_message [cmd] is a command representing that message. *)
let text_to_message str screen=
  let str = String.trim str in
  if str = "" then CIllegal
  else if str = "#back" then CBack
  else if str = "#help" then CHelp
  else if str = "#logout" then CLogout
  else if str = "#scrollup" then CScrollUp
  else if str = "#scrolldown" then CScrollDown
  else
  match screen with
  | Messages -> parse_messages_screen str
  | Channels -> parse_channels_screen str
  | Organizations -> parse_organizations_screen str