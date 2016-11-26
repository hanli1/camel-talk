open Yojson
open Client
open Parser

type current_state =
{
  mutable current_org : string option;
  mutable current_channel : string option;
  mutable current_user : string;
  mutable current_screen : Parser.screen
}

type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CBack
  | CIllegal

let rec main st =
	let userinput = read_line () in
  match text_to_message userinput st.current_screen with (*doesn't perform repainting*)
  | CIllegal ->
  	ANSITerminal.(print_string [Bold; blue] "Illegal command"); main st
  | CCreate s -> (
    match st.current_screen with
    | Organizations -> 
        let resp = Client.create_organization st.current_user s in
        if resp.status = "success" then main st else 
        ANSITerminal.(print_string [Bold; blue] resp.message); main st
    | Channels -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> 
        let resp = Client.create_channel st.current_user s o in
        if resp.status = "success" then main st else
        ANSITerminal.(print_string [Bold; blue] resp.message); main st
    )
    | Messages -> failwith "shouldn't happen"
  )
  | CDelete s -> (
    match st.current_screen with
    | Organizations -> 
        let resp = Client.delete_organization st.current_user s in (
        match resp.status with
        | "fail" -> ANSITerminal.(print_string [Bold; blue] resp.message); 
          main st
        | "success" -> if not (Some s = st.current_org) then 
        main st
        else st.current_org <- None; main st
      )
    | Channels -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> 
        let resp = Client.delete_channel st.current_user s o in
        (
        match resp.status with
        | "fail" -> ANSITerminal.(print_string [Bold; blue] resp.message); 
          main st
        | "success" -> if not (Some s = st.current_channel) then 
        main st
        else st.current_channel <- None; main st
      )
    )
    | Messages -> failwith "shouldn't happen"
  )
  | CSwitch s -> (
    match st.current_screen with
    | Organizations -> (
        let resp = Client.get_channels st.current_user s in
        match (fst resp) with
        | "fail" ->
        ANSITerminal.(print_string [Bold; blue] "Not a valid switch"); main st
        | "success" ->
        st.current_org <- Some s;
        main st
      )
    | Channels -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> (
        let resp = Client.get_messages st.current_user s o 0 in (*int is the index, this is probably wrong?*)
        match (fst resp) with 
        | "fail" ->
        ANSITerminal.(print_string [Bold; blue] "Not a valid switch"); main st
        | "success" ->
        st.current_channel <- Some s;
        main st
      )
    )
    | Messages -> failwith "shouldn't happen"
  )
  | CSimpleMessage s -> (
    match st.current_screen with
    | Messages -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> (
        match st.current_channel with
        | None -> failwith "shouldn't happen"
        | Some c -> send_message_simple st.current_user c o
          (`Assoc [("Content", `String s)]); main st
      )
    ) 
    | _ -> failwith "shouldn't happen"
  )
  | CReminderMessage (s, i) -> (
    match st.current_screen with
    | Messages -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> (
        match st.current_channel with
        | None -> failwith "shouldn't happen"
        | Some c -> send_message_reminder st.current_user c o
          (`Assoc [("Content", `String s);("Time", `Int i)]); main st
      )
    ) 
    | _ -> failwith "shouldn't happen"
  )
  | CPollMessage (s, xs) -> (
    match st.current_screen with
    | Messages -> (
      match st.current_org with
      | None -> failwith "shouldn't happen"
      | Some o -> (
        match st.current_channel with
        | None -> failwith "shouldn't happen"
        | Some c -> send_message_poll st.current_user c o
          (`Assoc [("Content", `String s);("Options", `List (
            List.map (fun x -> `String x) xs))]); main st
      )
    ) 
    | _ -> failwith "shouldn't happen"
  )
  | CBack -> (
    match st.current_screen with
    | Organizations -> 
    ANSITerminal.(print_string [Bold; blue] 
      "Can't go out of organization screen."); main st
    | Channels ->
    st.current_screen <- Organizations;
    st.current_channel <- None; main st
    | Messages ->
    st.current_screen <- Channels; main st
  )

and login () =
  ANSITerminal.(print_string [Bold; blue]
  	"\nTo log in, type in your username, then password.");
  ANSITerminal.(print_string [Bold; green]
  	"\nUsername: ");
  let username = read_line () in
  ANSITerminal.(print_string [Bold; green]
  	"Password: ");
  let password = read_line () in
  if (login_user username password).status = "success" then
	main
  {
    current_org = None;
    current_channel = None;
    current_user = username;
    current_screen = Parser.Organizations
  }
  else
    ANSITerminal.(print_string [Bold; blue]
  	"\nNot a valid username and password pair.\nWant to register? y/n ");
  ANSITerminal.(print_string [Blink] "> ");
  if (read_line ()) = "y" then register () else login ()


and register () =
  ANSITerminal.(print_string [Bold; blue]
  	"\nTo register, type in your desired username, then desired password.");
  ANSITerminal.(print_string [Bold; green]
  	"\nUsername: ");
  let username = read_line () in
  ANSITerminal.(print_string [Bold; green]
  	"Password: ");
  let password = read_line () in
  let clientpass = register_user username password in
  if clientpass.status = "success" then
  let _ = ANSITerminal.(print_string [Bold; green]
  	"Success!") in
  main
  {
     current_org = None;
     current_channel = None;
     current_user = username;
     current_screen = Organizations
  }
  else
  ANSITerminal.(print_string [Bold; blue]
  	"An error occured. Please register again.");
    register ()


and draw_update c = ()

let () =
ANSITerminal.resize 80 34;
	print_string
"                         MMMMMMM                     MMMMMMMMMMMMMM
                      MMMMMMMMMMMM                MMMMMMMMMM MMMMMMMM
                    MMMMMMMMMMMMMMMM                MMMMMMMMMMMMMMMMM
               MMMMMMMMMMMMMMMMMMMMMM              MMMMMMMMMMMMM   MM
            MMMMMMMMMMMMMMMMMMMMMMMMMMM            MMMMMMMMMMM
          MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM        MMMMMMMMMMMM
        MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM    MMMMMMMMMMMMM
        MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
       MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
       MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
       MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
      MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
     MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
    MMMMMMMM  MMMMMMMM  MMMMMMMMMMMMMMMMMMMM
   MMMMM  M   MMMMMMM       MNMMMMMMMMMMMMMM
 MMMMM  MMMM  MMMMMM          MMMMM  MMMMMM
MMMMMM  MMM  MMMMMM           MMMMM   MMMMM
MMMMM     M  MMMMM            MMMM     MMMM
MMMM        MMMMM           MMMMMM      MMMMM
 MMM       MMMMMMM          NMMMMM      MMMMM
 MM         MMMM             MMMM         MMM
MMMM         MMMM           MMMM           MM
 MMM          MMM           MMM            MMM
 MMMM         MMMM         MMM              MMM
 MMMMM         MMM        MMMM               MMM
   MMMMMM      MMMM        MMMM              MMMMMM
      MMM       MMMMM       MMMM              MMMMMM
                  MMMMM                        MMMMM
                    MMMMM";
  ANSITerminal.(print_string [green]
    "\n\nHello! and Welcome to Camel Talk.\n");
  ANSITerminal.(print_string [Bold; blue]
  	"Would you like to log in or register?");
  ANSITerminal.(print_string [Bold; blue]
  	"\nType \"login\" to Log in, or \"register\" to Register\n");
  ANSITerminal.(print_string [Blink] "> ");
  if (read_line ()) = "login" then login () else register ()