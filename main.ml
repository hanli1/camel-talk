open Yojson
open Client
open Parser
open Renderer
open Lwt

type current_state = {
  mutable current_org : string option;
  mutable current_channel : string option;
  mutable current_user : string;
  mutable current_screen : Parser.screen;
  mutable logged_out : bool;
  mutable current_line : int;
  mutable message : string
}

type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CBack
  | CHelp
  | CIllegal
  | CLogout
  | CInvite of string * string
  | CScrollUp
  | CScrollDown
  | CLeave of string * string
  | CVote of string * string

let current_input_stack = ref []

(**
 * [escape_char c] escapes the character [c] and provides compatibility
 * with JSON strings
 *)
let escape_char c =
  if c = '\\' then
    "\\"
  else if c = '\'' then
   "'"
  else if c = '\\' then
   "\\"
  else
    Char.escaped c

(**
 * [escape_str s] escapes the string [s] and provides compatibility with
 * JSON strings
 *)
let escape_str s =
  if s = "\\" then
    "\\\\"
  else if s = "\"" then
    "\\\""
  else if s = "\\'" then
    "'"
  else
     s

let rec main (st : current_state) : (unit Lwt.t) =
	Lwt_io.read_char (Lwt_io.stdin) >>=
  (
  fun c ->
  (
  if c = '\127' then
    match !current_input_stack with
    | [] -> main st
    | h::t ->
      (current_input_stack := t;
      main st)
  else if c != '\n' then
    (current_input_stack := (escape_char c)::!current_input_stack;
    main st)
  else
    let input_stack = List.map escape_str !current_input_stack in
    let s = String.concat "" (List.rev input_stack) in
    let () = (current_input_stack := []) in
    match text_to_message s st.current_screen with (*doesn't perform repainting*)
    | CIllegal ->
    	(st.message <- "Illegal command"); main st
    | CCreate s -> (
      (
      match st.current_screen with
      | Organizations -> (
          let resp = Client.create_organization st.current_user s in
          (st.message <- resp.message); main st
      )
      | Channels -> (
        match st.current_org with
        | None -> failwith "shouldn't happen"
        | Some o ->
          let resp = Client.create_channel st.current_user o s in
          (st.message <- resp.message); main st
      )
      | Messages -> failwith "shouldn't happen"
      )
    )
    | CDelete s -> (
      match st.current_screen with
      | Organizations ->
          let resp = Client.delete_organization st.current_user s in
          (st.message <- resp.message);
          (
            match resp.status with
            | "failure" -> main st
            | "success" ->
              if not (Some s = st.current_org) then main st
              else (st.current_org <- None;
              main st)
            | _ -> failwith "unknown response"
          )
      | Channels -> (
        match st.current_org with
        | None -> failwith "shouldn't happen"
        | Some o ->
          let resp = Client.delete_channel st.current_user o s in
          (st.message <- resp.message);
          (
            match resp.status with
            | "failure" -> main st
            | "success" -> (
              if not (Some s = st.current_channel) then
                main st
              else (
                st.current_channel <- None;
                main st
              )
            )
            | _ -> failwith "unknown response"
        )
      )
      | Messages -> failwith "shouldn't happen"
    )
    | CSwitch s -> (
      match st.current_screen with
      | Organizations -> (
          let resp = Client.get_channels st.current_user s in
          match (fst resp) with
          | "failure" -> (
            (st.message <- "Not a valid switch"); main st
          )
          | "success" -> (
            (st.message <- ("Switched into "^s));
            st.current_org <- Some s;
            st.current_screen <- Channels;
            main st
          )
          | _ -> failwith "unknown response"
        )
      | Channels -> (
        match st.current_org with
        | None -> failwith "shouldn't happen"
        | Some o -> (
          let resp = Client.get_messages st.current_user s o 0 in
          match (fst resp) with
          | "failure" -> (
            (st.message <- "Not a valid switch"); main st
          )
          | "success" -> (
            (st.message <- ("Switched into "^o));
            st.current_channel <- Some s;
            st.current_screen <- Messages;
            main st
          )
          | _ -> failwith "unknown response"
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
          | Some c -> (send_message_simple st.current_user c o
            (`Assoc [("content", `String s)]); main st)
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
          | Some c -> (send_message_reminder st.current_user c o
            (`Assoc [("content", `String s);("time", `Int i)]); main st)
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
            (`Assoc [
              ("content", `String s);
              ("option", `List (List.map (fun x -> `String x) xs))
            ]);
            main st
        )
      )
      | _ -> failwith "shouldn't happen"
    )
    | CBack -> (
      match st.current_screen with
      | Organizations ->
        (st.message <- "Can't go out of organization screen."); main st
      | Channels ->
        st.current_screen <- Organizations;
        st.current_channel <- None;
        main st
      | Messages ->
        st.current_screen <- Channels;
        main st
    )
    | CLogout -> Lwt.return ()
    | CInvite (user_to_join, orgid) -> (
      let resp = invite user_to_join orgid st.current_user in
      (st.message <- resp.message); main st
    )
    | CLeave (user_to_leave, orgid) -> (
      let resp = leave user_to_leave orgid st.current_user in
      (st.message <- resp.message); main st
    )
    | CScrollUp -> st.current_line <- st.current_line + 10 ; main st
    | CScrollDown -> st.current_line <- st.current_line - 10 ; main st
    | CHelp ->
      (st.message <-
      "Help info:
      While in ORGANIZATION/CHANNEL screen:
      #create <name> : creates a new organization/channel.
      Must be unique.
      #delete <name> : deletes an existing organization/channel.
      Must be an admin.
      <existing organization name> : switches into organization/channel.
      #invite <username> <org name> : invites another user to join
      the specified organization. (only for organization screen)
      #leave <username> <organization name> : kicks out the user from the
      specified organization. (only for organization screen)
      #logout : logs out
      #back : goes out of channel screen into the organization screen

      While in MESSAGE screen:
      <anything not starting with #> : sends a message containing only text
      #set_reminder <text> <time> : sets a reminder that sends a message with
      the specified text
      after the specified amount of time has elapsed.
      #set_poll [<option1>;<option2>...] <question> : sets a poll with options
      that other users of the channel can vote on.
      #vote <pollname> <optionname> : votes on an option from an existing poll.
      #scrollup, #scrolldown: scrolls the message list up or down respectively.
      #back, #logout: same as above."
      ); main st
    | _ -> failwith "unimplemented command"
    )
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
  if (login_user username password).status = "success" then (
    ANSITerminal.(print_string [Bold; green]
    "Success!\n");
    flush_all ();
    let st =  {
       current_org = None;
       current_channel = None;
       current_user = username;
       current_screen = Organizations;
       logged_out = false;
       current_line = 0;
       message = "Hello, "^username^". Type \"#help\" to see commands."
    } in
    run_app_threads st
    )
  else
    ANSITerminal.(print_string [Bold; blue]
  	"\nNot a valid username and password pair\nWant to register? (y/n) Or, type \"exit\" to exit\n");
    ANSITerminal.(print_string [Blink] "> ");
    match read_line () with
    | "exit" -> exit 0
    | "y" -> register ()
    | _ -> login ()


and register () =
  ANSITerminal.(print_string [Bold; blue]
  	"\nTo register, type in your desired username, then desired password.");
  ANSITerminal.(print_string [Bold; green] "\nUsername: ");
  let username = read_line () in
    ANSITerminal.(print_string [Bold; green] "Password: ");
  let password = read_line () in
  let clientpass = register_user username password in
  if clientpass.status = "success" then (
    ANSITerminal.(print_string [Bold; green] "Success!\n");
    flush_all ();
    let st =  {
       current_org = None;
       current_channel = None;
       current_user = username;
       current_screen = Organizations;
       logged_out = false;
       current_line = 0;
       message = "Hello, "^username^". Type \"#help\" to see commands."
    } in
    run_app_threads st
  )
  else
    ANSITerminal.(print_string [Bold; blue]
      "An error occured. Please register again.");
    register ()

(**
 * [run_app_threads st] starts up the threads necessary for the application
 * loop to function properly (each thread has access to the application
 * state [st])
 *)
and run_app_threads st =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {termio with
  Unix.c_icanon = false; Unix.c_echo = false} in
  let () = ((Lwt_main.run (Lwt.pick [draw_update st; main st]))) in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {termio with
  Unix.c_icanon = true; Unix.c_echo = true}


and draw_update c =
  let print_persist () =
  ANSITerminal.(print_string []
" .d8888b.                         888 888             888 888
d88P  Y88b                        888 888             888 888
888    888                        888 888             888 888
888         8888b.  88888b.d88b.  888 888888  8888b.  888 888  888
888             88b 888  888  88b 888 888         88b 888 888 .88P
888    888 .d888888 888  888  888 888 888    .d888888 888 888888K
Y88b  d88P 888  888 888  888  888 888 Y88b.  888  888 888 888  88b
  Y8888P    Y888888 888  888  888 888   Y888  Y888888 888 888  888



Y88b     .d8888b.   d888    d888   .d8888b.     d88P
 Y88b   d88P  Y88b d8888   d8888  d88P  Y88b   d88P
  Y88b       .d88P   888     888  888    888  d88P
   Y88b     8888     888     888  888    888 d88P
   d88P       Y8b.   888     888  888    888 Y88b
  d88P  888    888   888     888  888    888  Y88b
 d88P   Y88b  d88P   888     888  Y88b  d88P   Y88b
d88P      Y8888P   8888888 8888888  Y8888P      Y88b   \n\n\n\n\n\n\n"
  );
  ANSITerminal.(print_string [magenta]
  ("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"^
  (c.message)
  ^"\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"))
  in
  Lwt_unix.sleep 0.1 >>= (fun () ->
  match c.current_screen with
  | Organizations ->
      if c.logged_out then Lwt.return ()
      else
        let response_json = snd (get_user_organizations c.current_user) in
        (ANSITerminal.(erase Above);
        ANSITerminal.(move_cursor (-100) 0);
        print_persist ();
        render_organizations_list response_json;
        ANSITerminal.(print_string [] (String.concat "" (List.rev
        !current_input_stack)));
        flush_all ();
        draw_update c)
  | Channels ->
      if c.logged_out then Lwt.return ()
      else (
        match c.current_org with
        | None -> failwith "shouldn't happen"
        | Some o ->
          let response_json = snd (get_channels c.current_user o) in
          (ANSITerminal.(erase Above);
          ANSITerminal.(move_cursor (-100) 0);
          print_persist ();
          render_channels_list o response_json;
          ANSITerminal.(print_string [] (String.concat ""
          (List.rev !current_input_stack)));
          flush_all ();
          draw_update c)
      )
  | Messages ->
      if c.logged_out then Lwt.return ()
      else (
        match c.current_org with
        | None -> failwith "shouldn't happen"
        | Some o -> (
          match c.current_channel with
          | None -> failwith "shouldn't happen"
          | Some ch ->
            let response_json = snd (get_messages c.current_user ch o 0) in
            (ANSITerminal.(erase Above);
            ANSITerminal.(move_cursor (-100) 0);
            render_channel_messages response_json;
            ANSITerminal.(print_string [] (String.concat "" (List.rev
            !current_input_stack)));
            flush_all ();
            draw_update c)
        )
      )
  )

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
  (let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN {termio with Unix.c_icanon = true;
  Unix.c_echo = true;});
  if (read_line ()) = "login" then login () else register ()
