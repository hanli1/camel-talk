open DataOperations

val send_message_simple_api : Cohttp_Response -> string
val send_message_poll_api : Cohttp_Response -> string
val send_message_reminder_api : Cohttp_Response -> string

val register_user_api : Cohttp_Response -> string
val login_user_api : Cohttp_Response -> string

val create_organization_api : Cohttp_Response -> string
val delete_organization_api : Cohttp_Response -> string

val create_channel_api : Cohttp_Response -> string
val delete_channel_api : Cohttp_Response -> string

val get_channels_api : Cohttp_Response -> string
val get_messages_api : Cohttp_Response -> string