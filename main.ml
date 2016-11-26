open Yojson
open Client

type current_state =
{
  mutable current_org : string option;
  mutable current_channel : string option;
  mutable current_user : string
}

type command =
  | CCreate of string
  | CDelete of string
  | CSwitch of string
  | CSimpleMessage of string
  | CReminderMessage of string * int
  | CPollMessage of string * string list
  | CIllegal

type screen =
  | Messages
  | Channels
  | Organizations

let rec main () =
(* 	let _ = print_string (string_of_int (fst (ANSITerminal.size ()))) in
	let _ = print_string (string_of_int (snd (ANSITerminal.size ()))) in *)
	let userinput = read_line () in ()(* in
match text_to_message userinput with
| CIllegal ->
	print_endline "Illegal command"; *)

and login () =
  ANSITerminal.(print_string [Bold; blue]
  	"\nTo log in, type in your username, then password.");
  ANSITerminal.(print_string [Bold; green]
  	"\nUsername: ");
  let username = read_line () in
  ANSITerminal.(print_string [Bold; green]
  	"Password: ");
  let password = read_line () in
  if (login_user username password).status then
	main ()
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
  if clientpass.status then
  let _ = ANSITerminal.(print_string [Bold; green]
  	"Success!") in
    main ()
  else
  ANSITerminal.(print_string [Bold; blue]
  	"An error occured. Please register again.");
    register ()


and draw_update c = ()

let () =
ANSITerminal.resize 80 34;
	print_string
"                         MMMMMMM                     MMMMMMMMMMMMMM
                      MMMMMMMMMMMM                MMMMMMMMMMMMMMMMMMM
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