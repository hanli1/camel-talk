open DataOperations
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util
open Str

(**
 * all_data is a data structure containing all server side data in memory
 *)
let all_data = load_data ()

type response_record = {
  status_code : int;
  response_body : string;
}

type request_record = {
  request_info : Request.t;
  request_body : string;
}

(**
 * [print_helper] logs the string [s] to a file
 *)
let print_helper s =
  (let oc = open_out_gen [Open_creat; Open_append] 0o777 "server_log.txt" in
  Printf.fprintf oc "%s\n" s;
  close_out oc;
  ())

(**
 * [starts_with] is true if the string [s] begins with the string [suffix]; 
 * false otherwise
 *)
let starts_with s prefix =
  let prefix_length = String.length prefix in 
  (String.sub s 0 prefix_length) = prefix

(**
 * [serialize_message] is a string that represents the serialization of the 
 * given record [m] of type message
 *)
let serialize_message m =
  let message_json_string =
    match m.body with
    | SimpleMessage c ->
      "{\"content\":\"" ^ c ^ "\"}"
    | ReminderMessage (c,timestamp) ->
      "{\"content\":\"" ^ c ^ "\", \"time\":\"" ^ (string_of_int timestamp) ^ 
      "\"}"
    | PollMessage (c, options_list) ->
      let string_options_list = List.map (fun option_pair -> "{\"option\":\"" ^
      (fst option_pair) ^ "\",\"count\":\"" ^ (string_of_int (snd option_pair)) 
      ^ "\"}") options_list in
      "{\"content\":\"" ^ c ^ "\", \"options\":" ^ "[" ^(String.concat "," 
      string_options_list) ^ "]}"
  in
  let message_type =   
    match m.body with
    | SimpleMessage c -> "simple"
    | ReminderMessage (c,timestamp) -> "reminder"
    | PollMessage (c, options_list) -> "poll"
  in
  "{\"message\":\"" ^ message_json_string ^ "\"," ^ "\"message_type\":\"" ^ 
  message_type ^ "\"," ^ "\"user_id\":\"" ^ m.user_id ^ "\"," ^ 
  "\"time_stamp\":\"" ^ (string_of_int m.timestamp) ^ "\"}"

let send_message_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let user_id = json_body |> member "user_id" |> to_string in
      let channel_id = json_body |> member "channel_id" |> to_string in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let message_type = json_body |> member "message_type" |> to_string in
      let message_record =
        if message_type = "simple" then
          let content = json_body |> member "message" |> member "content" |> 
          to_string in
          SimpleMessage (content)
        else if message_type = "poll" then
          let content = json_body |> member "message" |> member "content" |> 
          to_string in
          let reminder_time = json_body |> member "message" |> member "time" |>
          to_string |> int_of_string in
          ReminderMessage (content, reminder_time)
        else if message_type = "reminder" then
          let content = json_body |> member "message" |> member "content" |> 
          to_string in
          let options_list = List.map (fun option_json -> (option_json |> 
          member "option" |> to_string, option_json |> member "count" |> 
          to_int)) (json_body |> member "message" |> member "options" |> 
          to_list) in
          PollMessage (content, options_list)
        else 
          raise (Failure "Wrong format for the body of this request")
        in
        if (add_message all_data organization_id channel_id user_id 
        message_record) = true then
          {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
          ":\"successfully added message\"}"}
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"Unable to add message with given parameters in request\"}"}
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let register_user_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let user_id = json_body |> member "user_id" |> to_string in
      let password = json_body |> member "password" |> to_string in
      if (add_user all_data user_id password) = true then
        {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
        ":\"successfully added new user\"}"}
      else
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Unable to register new user due to invalid username/password " ^ 
        "or because username already exists.\"}"}    
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}        
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let login_user_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let user_id = json_body |> member "user_id" |> to_string in
      let password = json_body |> member "password" |> to_string in
      match (get_user_data all_data user_id) with 
      | Some u ->
        if u.password = password then
          {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
          ":\"successfully authenticated\"}"} 
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"Password is incorrect for this user\"}"} 
      | None ->
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"User id does not exist.\"}"} 
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let create_organization_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      if (add_org all_data organization_id user_id = true) then
        {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
        ":\"Organization successfully created\"}"}         
      else
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Unable to create organization because the name is invalid or " ^ 
        "an organization with that name already exists.\"}"}       
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let delete_organization_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      match (get_org_data all_data organization_id) with 
      | Some o ->
        if o.admin = user_id then
          {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
          ":\"Organization successfully deleted\"}"} 
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"The user is not the admin of this organization\"}"} 
      | None ->
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Organization does not exist\"}"} 
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let create_channel_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      let channel_id = json_body |> member "channel_id" |> to_string in
      if (starts_with channel_id "directmessage") then
        let channel_list = split (regexp_string "@") channel_id in
        let first_user = List.nth channel_list 1 in
        let second_user = List.nth channel_list 2 in
        if (add_channel all_data organization_id channel_id user_id false) = 
        true then
          if ((join_channel all_data channel_id first_user organization_id) = 
          true) && ((join_channel all_data channel_id second_user 
          organization_id) = true) then
            {status_code=200;response_body="{\"status\":\"success\"," ^ 
            "\"message\":\"Direct message channel between the two users " ^ 
            "successfully created\"}"}  
          else
             {status_code=200;response_body="{\"status\":\"failure\"," ^ 
             "\"message\":\"Direct message channel between the two users " ^ 
             "could not be created\"}"}           
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"Direct message channel between the two users already exists\"}"}  
      else
        if (add_channel all_data organization_id channel_id user_id true) = 
        true then
          {status_code=200;response_body="{\"status\":\"success\",\"message\"" ^ 
          ":\"Channel in organization successfully created\"}"}  
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"Channel already exists\"}"}  
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let delete_channel_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "POST" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      let channel_id = json_body |> member "channel_id" |> to_string in
      if (starts_with channel_id "directmessage") then
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Direct message channels cannot be deleted\"}"}  
      else
        (
        match (get_org_data all_data organization_id) with 
        | Some o ->
          if o.admin = user_id then
            if (remove_channel all_data organization_id channel_id) = true 
            then
              {status_code=200;response_body="{\"status\":\"success\"," ^ 
              "\"message\":\"Channel in organization successfully created\"}"} 
            else
              {status_code=200;response_body="{\"status\":\"failure\"," ^ 
              "\"message\":\"Channel does not exist in this organization\"}"}             
          else
            {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
            ":\"The user is not the admin of this organization\"}"} 
        | None ->
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"Organization does not exist\"}"} 
        )
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let get_channels_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "GET" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      match (get_org_data all_data organization_id) with 
      | Some o ->
        if (List.mem user_id o.users) = true then
          let team_channels = List.filter (
            fun channel_name -> 
              let channel_record = get_channel_data all_data organization_id 
              channel_name in 
              match channel_record with
              | Some c -> if c.is_public = true then true else false 
              | None -> raise (Failure "Channel does not exist")
          ) o.channel_names 
          in
          let private_channels = List.filter (
            fun channel_name -> 
              let channel_record = get_channel_data all_data organization_id 
              channel_name in 
              match channel_record with
              | Some c -> if c.is_public = false then true else false 
              | None -> raise (Failure "Channel does not exist")
          ) o.channel_names 
          in
          {status_code=200;response_body="{\"status\":\"success\",\"team_" ^ 
            "channels\":" ^ "[" ^(String.concat "," team_channels) ^ "], \"" ^ 
            "private_channels\":" ^ "[" ^ (String.concat "," private_channels) 
            ^ "]}"}
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"User does not belong to this organization\"}"} 
      | None ->
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Organization does not exist\"}"} 
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}  
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

let get_messages_api request =
  let meth = request.request_info |> Request.meth |> Code.string_of_method in
  if meth = "GET" then
    try
      let json_body = Yojson.Basic.from_string request.request_body in
      let organization_id = json_body |> member "organization_id" |> to_string in
      let user_id = json_body |> member "user_id" |> to_string in 
      let channel_id = json_body |> member "channel_id" |> to_string in
      let start_index = json_body |> member "start_index" |> to_int in 
      match (get_channel_data all_data organization_id channel_id) with
      | Some c ->
        if (List.mem user_id c.users) = true then
          let rec get_10_messages cur_index end_index cur_message_list =
            if (cur_index > end_index) || (cur_index >= List.length c.messages) 
            then
              cur_message_list
            else
              let cur_message = List.nth c.messages cur_index in
              get_10_messages (cur_index + 1) end_index 
              ((serialize_message cur_message)::cur_message_list)
          in
          let messages_list = get_10_messages start_index (start_index + 9) [] in
          {status_code=200;response_body="{\"status\":\"success\",\"messages\":" ^
          "[" ^ (String.concat "," messages_list) ^ "]}"}
        else
          {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
          ":\"User does not belong to this channel\"}"}         
      | None ->
        {status_code=200;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Channel does not exist in this organization\"}"}  
    with
      | _ -> 
        {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
        ":\"Wrong format for the body of this request\"}"}     
  else
    {status_code=400;response_body="{\"status\":\"failure\",\"message\"" ^ 
    ":\"Wrong HTTP method for this request\"}"}

(**
 * [request_router] parses and routes an HTTP request specified by its 
 * request information [req] and request body [body] to the proper REST 
 * API function depending on the request's URI
 *)
let request_router _conn req body =
  let uri = req |> Request.uri |> Uri.to_string in
  let uri_path = req |> Request.uri |> Uri.path in
  let meth = req |> Request.meth |> Code.string_of_method in
  let headers = req |> Request.headers |> Header.to_string in
  body |> Cohttp_lwt_body.to_string >>= (
    fun body -> 
      (
      print_helper ("Request Information" ^ "\n\nUri: " ^ uri ^ "\nMethod: " 
      ^ meth ^ "\nHeaders: \n" ^ headers ^ "Body: " ^ body ^ "\n");
      let response_result =
        if uri_path = "/send_message" then
          send_message_api {request_info=req; request_body=body}
        else if uri_path = "/register_user" then
          register_user_api {request_info=req; request_body=body}
        else if uri_path = "/login_user" then
          login_user_api {request_info=req; request_body=body}
        else if uri_path = "/create_organization" then
          create_organization_api {request_info=req; request_body=body}
        else if uri_path = "/delete_organization" then
          delete_organization_api {request_info=req; request_body=body}
        else if uri_path = "/create_channel" then
          create_channel_api {request_info=req; request_body=body}
        else if uri_path = "/delete_channel" then
          delete_channel_api {request_info=req; request_body=body}
        else if uri_path = "/get_channels" then
          get_channels_api {request_info=req; request_body=body}
        else if uri_path = "/get_messages" then
          get_messages_api {request_info=req; request_body=body}
        else
          {status_code=404; response_body=
          "{\"error\":\"Invalid URI in request\"}"}
        in
        Server.respond_string ~status:(Code.status_of_code 
        response_result.status_code) ~body:response_result.response_body ()
      )
  )

(**
 * [server] creates an HTTP server through the Cohttp API
 *)
let server =
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback:request_router ())

(**
 * Initializes a thread thats runs the code in [server], which starts up the 
 * HTTP server 
 *)
let () = ignore (Lwt_main.run server)
