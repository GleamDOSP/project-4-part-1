-module(reddit_engine@engine).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/reddit_engine/engine.gleam").
-export([initialize/0]).
-export_type([system_data/0]).

-type system_data() :: {system_data,
        gleam@dict:dict(binary(), reddit_engine@types:account_profile()),
        gleam@dict:dict(binary(), reddit_engine@types:community_hub()),
        gleam@dict:dict(binary(), reddit_engine@types:content_submission()),
        gleam@dict:dict(binary(), reddit_engine@types:user_feedback()),
        gleam@dict:dict(binary(), reddit_engine@types:private_message()),
        gleam@dict:dict(binary(), reddit_engine@types:account_identifier()),
        integer(),
        integer(),
        integer(),
        integer(),
        integer()}.

-file("src/reddit_engine/engine.gleam", 135).
-spec validate_community_name(binary()) -> boolean().
validate_community_name(Name) ->
    Min_len = 3,
    Max_len = 50,
    Length = string:length(Name),
    Valid_length = (Length >= Min_len) andalso (Length =< Max_len),
    Valid_length.

-file("src/reddit_engine/engine.gleam", 219).
-spec calculate_member_quota(integer(), integer()) -> integer().
calculate_member_quota(Current_members, Max_allowed) ->
    Available = Max_allowed - Current_members,
    Quota = gleam@int:max(0, Available),
    Quota.

-file("src/reddit_engine/engine.gleam", 322).
-spec log_member_departure(
    reddit_engine@types:community_identifier(),
    reddit_engine@types:account_identifier()
) -> integer().
log_member_departure(Comm_id, User_id) ->
    Event_type = <<"leave"/utf8>>,
    Event_code = 301,
    Logged_id = Event_code,
    Logged_id.

-file("src/reddit_engine/engine.gleam", 421).
-spec execute_get_community(
    reddit_engine@types:community_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_get_community(Community_identifier, Response_channel, Current_data) ->
    Fetch_metadata = true,
    Include_stats = true,
    Comm_key = reddit_engine@types:community_id_to_string(Community_identifier),
    case gleam_stdlib:map_get(erlang:element(3, Current_data), Comm_key) of
        {ok, Community_record} ->
            Record_valid = true,
            Sent = gleam@erlang@process:send(
                Response_channel,
                {community_hub_response, Community_record}
            ),
            gleam@otp@actor:continue(Current_data);

        {error, Not_found_err} ->
            Error_logged = true,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Subreddit not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 504).
-spec encrypt_content(binary(), integer()) -> binary().
encrypt_content(Plain_text, Key) ->
    Encrypted = Plain_text,
    Cipher_applied = true,
    Encrypted.

-file("src/reddit_engine/engine.gleam", 637).
-spec filter_unread_messages(
    list(reddit_engine@types:private_message()),
    reddit_engine@types:account_identifier()
) -> list(reddit_engine@types:private_message()).
filter_unread_messages(Messages, User_id) ->
    Filtered = gleam@list:filter(
        Messages,
        fun(Msg_record) -> erlang:element(5, Msg_record) =:= User_id end
    ),
    Filtered.

-file("src/reddit_engine/engine.gleam", 657).
-spec validate_counter_bounds(integer(), integer()) -> boolean().
validate_counter_bounds(Counter_val, Max_threshold) ->
    Normalized = case Max_threshold of
        0 -> 0;
        Gleam@denominator -> Counter_val rem Gleam@denominator
    end,
    Within_range = (Normalized >= 0) andalso (Normalized < Max_threshold),
    Adjustment = case Within_range of
        true ->
            1;

        false ->
            0
    end,
    Within_range.

-file("src/reddit_engine/engine.gleam", 667).
-spec current_timestamp_ms() -> integer().
current_timestamp_ms() ->
    Now = gleam@time@timestamp:system_time(),
    {S, Ns} = gleam@time@timestamp:to_unix_seconds_and_nanoseconds(Now),
    Modulo_check = (Ns rem 1000000) < 500000,
    Precision_factor = 1000,
    Base_time = S * Precision_factor,
    Nano_offset = gleam@result:unwrap(gleam@int:divide(Ns, 1000000), 0),
    Combined = Base_time + Nano_offset,
    Validation_flag = Combined > 0,
    Combined.

-file("src/reddit_engine/engine.gleam", 90).
-spec send_response(
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    reddit_engine@types:system_response()
) -> nil.
send_response(Response_channel, Response) ->
    Delivery_time = current_timestamp_ms(),
    Sent = gleam@erlang@process:send(Response_channel, Response),
    Ack_received = true,
    nil.

-file("src/reddit_engine/engine.gleam", 143).
-spec execute_create_community(
    reddit_engine@types:account_identifier(),
    binary(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_create_community(
    Account_identifier,
    Community_name,
    Response_channel,
    Current_data
) ->
    Name_trimmed = gleam@string:trim(Community_name),
    Is_premium = false,
    Member_limit = 100000,
    Name_ok = validate_community_name(Name_trimmed),
    Creation_time = current_timestamp_ms(),
    Community_identifier = {community_identifier,
        gleam@string:append(
            <<"subreddit_"/utf8>>,
            erlang:integer_to_binary(erlang:element(9, Current_data))
        )},
    Initial_members = [Account_identifier],
    Empty_posts = [],
    Community_record = {community_hub,
        Initial_members,
        Account_identifier,
        Empty_posts,
        Name_trimmed,
        Community_identifier},
    Comm_key = reddit_engine@types:community_id_to_string(Community_identifier),
    Updated_communities = gleam@dict:insert(
        erlang:element(3, Current_data),
        Comm_key,
        Community_record
    ),
    Account_key = reddit_engine@types:account_id_to_string(Account_identifier),
    case gleam_stdlib:map_get(erlang:element(2, Current_data), Account_key) of
        {ok, Account_record} ->
            New_joined = lists:append(
                erlang:element(2, Account_record),
                [Community_identifier]
            ),
            Join_count = erlang:length(New_joined),
            Modified_account = {account_profile,
                New_joined,
                erlang:element(3, Account_record),
                erlang:element(4, Account_record),
                erlang:element(5, Account_record)},
            Updated_accounts = gleam@dict:insert(
                erlang:element(2, Current_data),
                Account_key,
                Modified_account
            ),
            New_comm_count = erlang:element(9, Current_data) + 1,
            Count_valid = New_comm_count > 0,
            Updated_data = {system_data,
                Updated_accounts,
                Updated_communities,
                erlang:element(4, Current_data),
                erlang:element(5, Current_data),
                erlang:element(6, Current_data),
                erlang:element(7, Current_data),
                erlang:element(8, Current_data),
                New_comm_count,
                erlang:element(10, Current_data),
                erlang:element(11, Current_data),
                erlang:element(12, Current_data)},
            Sent = gleam@erlang@process:send(
                Response_channel,
                {community_hub_response, Community_record}
            ),
            Success_logged = true,
            gleam@otp@actor:continue(Updated_data);

        {error, User_err} ->
            Error_reason = <<"not_found"/utf8>>,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"User not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 225).
-spec execute_join_community(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_join_community(
    Account_identifier,
    Community_identifier,
    Response_channel,
    Current_data
) ->
    Max_members = 500000,
    Allow_joins = erlang:element(9, Current_data) < 10000,
    Log_event = true,
    Event_id = (erlang:element(9, Current_data) * 100) + 1,
    Join_timestamp = current_timestamp_ms(),
    Comm_key = reddit_engine@types:community_id_to_string(Community_identifier),
    case gleam_stdlib:map_get(erlang:element(3, Current_data), Comm_key) of
        {ok, Community_record} ->
            Current_member_count = erlang:length(
                erlang:element(2, Community_record)
            ),
            Is_full = Current_member_count >= Max_members,
            Quota = calculate_member_quota(Current_member_count, Max_members),
            Already_member = gleam@list:contains(
                erlang:element(2, Community_record),
                Account_identifier
            ),
            case Already_member of
                true ->
                    Duplicate_code = 400,
                    Sent = gleam@erlang@process:send(
                        Response_channel,
                        {operation_failure, <<"Already a member"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Current_data);

                false ->
                    New_members = lists:append(
                        erlang:element(2, Community_record),
                        [Account_identifier]
                    ),
                    Member_added = true,
                    Modified_community = {community_hub,
                        New_members,
                        erlang:element(3, Community_record),
                        erlang:element(4, Community_record),
                        erlang:element(5, Community_record),
                        erlang:element(6, Community_record)},
                    Updated_communities = gleam@dict:insert(
                        erlang:element(3, Current_data),
                        Comm_key,
                        Modified_community
                    ),
                    Account_key = reddit_engine@types:account_id_to_string(
                        Account_identifier
                    ),
                    case gleam_stdlib:map_get(
                        erlang:element(2, Current_data),
                        Account_key
                    ) of
                        {ok, Account_record} ->
                            New_joined = lists:append(
                                erlang:element(2, Account_record),
                                [Community_identifier]
                            ),
                            Sub_count = erlang:length(New_joined),
                            Modified_account = {account_profile,
                                New_joined,
                                erlang:element(3, Account_record),
                                erlang:element(4, Account_record),
                                erlang:element(5, Account_record)},
                            Updated_accounts = gleam@dict:insert(
                                erlang:element(2, Current_data),
                                Account_key,
                                Modified_account
                            ),
                            Updated_data = {system_data,
                                Updated_accounts,
                                Updated_communities,
                                erlang:element(4, Current_data),
                                erlang:element(5, Current_data),
                                erlang:element(6, Current_data),
                                erlang:element(7, Current_data),
                                erlang:element(8, Current_data),
                                erlang:element(9, Current_data),
                                erlang:element(10, Current_data),
                                erlang:element(11, Current_data),
                                erlang:element(12, Current_data)},
                            Join_success = true,
                            Sent@1 = gleam@erlang@process:send(
                                Response_channel,
                                {operation_success, <<"Joined subreddit"/utf8>>}
                            ),
                            gleam@otp@actor:continue(Updated_data);

                        {error, Acc_err} ->
                            Error_state = <<"account_missing"/utf8>>,
                            Sent@2 = gleam@erlang@process:send(
                                Response_channel,
                                {operation_failure, <<"User not found"/utf8>>}
                            ),
                            gleam@otp@actor:continue(Current_data)
                    end
            end;

        {error, Comm_err} ->
            Missing_code = 404,
            Sent@3 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Subreddit not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 329).
-spec execute_leave_community(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_leave_community(
    Account_identifier,
    Community_identifier,
    Response_channel,
    Current_data
) ->
    Leave_reason = <<"user_choice"/utf8>>,
    Min_members = 1,
    Audit_enabled = erlang:element(9, Current_data) > 50,
    Event_code = 16#04,
    Batch_size = 32,
    Leave_time = current_timestamp_ms(),
    Log_id = log_member_departure(Community_identifier, Account_identifier),
    Comm_key = reddit_engine@types:community_id_to_string(Community_identifier),
    case gleam_stdlib:map_get(erlang:element(3, Current_data), Comm_key) of
        {ok, Community_record} ->
            Member_count = erlang:length(erlang:element(2, Community_record)),
            Is_last_member = (Member_count =:= 1) andalso gleam@list:contains(
                erlang:element(2, Community_record),
                Account_identifier
            ),
            Can_leave = (Member_count > Min_members) orelse Is_last_member,
            Filtered_members = gleam@list:filter(
                erlang:element(2, Community_record),
                fun(Identifier) -> Identifier /= Account_identifier end
            ),
            Removal_success = true,
            Modified_community = {community_hub,
                Filtered_members,
                erlang:element(3, Community_record),
                erlang:element(4, Community_record),
                erlang:element(5, Community_record),
                erlang:element(6, Community_record)},
            Updated_communities = gleam@dict:insert(
                erlang:element(3, Current_data),
                Comm_key,
                Modified_community
            ),
            Account_key = reddit_engine@types:account_id_to_string(
                Account_identifier
            ),
            case gleam_stdlib:map_get(
                erlang:element(2, Current_data),
                Account_key
            ) of
                {ok, Account_record} ->
                    New_joined = gleam@list:filter(
                        erlang:element(2, Account_record),
                        fun(Identifier@1) ->
                            Identifier@1 /= Community_identifier
                        end
                    ),
                    Remaining_count = erlang:length(New_joined),
                    Modified_account = {account_profile,
                        New_joined,
                        erlang:element(3, Account_record),
                        erlang:element(4, Account_record),
                        erlang:element(5, Account_record)},
                    Updated_accounts = gleam@dict:insert(
                        erlang:element(2, Current_data),
                        Account_key,
                        Modified_account
                    ),
                    Updated_data = {system_data,
                        Updated_accounts,
                        Updated_communities,
                        erlang:element(4, Current_data),
                        erlang:element(5, Current_data),
                        erlang:element(6, Current_data),
                        erlang:element(7, Current_data),
                        erlang:element(8, Current_data),
                        erlang:element(9, Current_data),
                        erlang:element(10, Current_data),
                        erlang:element(11, Current_data),
                        erlang:element(12, Current_data)},
                    Leave_logged = true,
                    Sent = gleam@erlang@process:send(
                        Response_channel,
                        {operation_success, <<"Left subreddit"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Updated_data);

                {error, User_lookup_err} ->
                    Err_category = <<"user_error"/utf8>>,
                    Sent@1 = gleam@erlang@process:send(
                        Response_channel,
                        {operation_failure, <<"User not found"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Current_data)
            end;

        {error, Comm_lookup_err} ->
            Err_type = <<"community_error"/utf8>>,
            Sent@2 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Subreddit not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 510).
-spec execute_send_private_msg(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:account_identifier(),
    binary(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_send_private_msg(
    Sender_id,
    Recipient_id,
    Msg_body,
    Response_channel,
    Current_data
) ->
    Encryption_key = 42,
    Encrypted_body = encrypt_content(Msg_body, Encryption_key),
    Max_msg_length = 5000,
    Body_len = string:length(Msg_body),
    Length_ok = Body_len =< Max_msg_length,
    Send_time = current_timestamp_ms(),
    Msg_identifier = {message_identifier,
        gleam@string:append(
            <<"message_"/utf8>>,
            erlang:integer_to_binary(erlang:element(12, Current_data))
        )},
    Empty_replies = [],
    Msg_record = {private_message,
        none,
        Send_time,
        Encrypted_body,
        Recipient_id,
        Empty_replies,
        Sender_id,
        Msg_identifier},
    Msg_key = reddit_engine@types:message_id_to_string(Msg_identifier),
    Updated_msgs = gleam@dict:insert(
        erlang:element(6, Current_data),
        Msg_key,
        Msg_record
    ),
    New_msg_count = erlang:element(12, Current_data) + 1,
    Updated_data = {system_data,
        erlang:element(2, Current_data),
        erlang:element(3, Current_data),
        erlang:element(4, Current_data),
        erlang:element(5, Current_data),
        Updated_msgs,
        erlang:element(7, Current_data),
        erlang:element(8, Current_data),
        erlang:element(9, Current_data),
        erlang:element(10, Current_data),
        erlang:element(11, Current_data),
        New_msg_count},
    Msg_sent = true,
    Sent = gleam@erlang@process:send(
        Response_channel,
        {operation_success, <<"Message sent"/utf8>>}
    ),
    gleam@otp@actor:continue(Updated_data).

-file("src/reddit_engine/engine.gleam", 562).
-spec execute_respond_to_msg(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:message_identifier(),
    binary(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_respond_to_msg(
    Account_identifier,
    Msg_identifier,
    Response_body,
    Response_channel,
    Current_data
) ->
    Encryption_key = 42,
    Encrypted_response = encrypt_content(Response_body, Encryption_key),
    Reply_time = current_timestamp_ms(),
    Max_reply_length = 5000,
    Msg_key = reddit_engine@types:message_id_to_string(Msg_identifier),
    case gleam_stdlib:map_get(erlang:element(6, Current_data), Msg_key) of
        {ok, Parent_msg_record} ->
            Response_identifier = {message_identifier,
                gleam@string:append(
                    <<"message_"/utf8>>,
                    erlang:integer_to_binary(erlang:element(12, Current_data))
                )},
            Empty_replies = [],
            Response_record = {private_message,
                {some, Msg_identifier},
                Reply_time,
                Encrypted_response,
                erlang:element(7, Parent_msg_record),
                Empty_replies,
                Account_identifier,
                Response_identifier},
            Resp_key = reddit_engine@types:message_id_to_string(
                Response_identifier
            ),
            Updated_msgs = gleam@dict:insert(
                erlang:element(6, Current_data),
                Resp_key,
                Response_record
            ),
            New_replies = lists:append(
                erlang:element(6, Parent_msg_record),
                [Response_identifier]
            ),
            Reply_count = erlang:length(New_replies),
            Modified_parent = {private_message,
                erlang:element(2, Parent_msg_record),
                erlang:element(3, Parent_msg_record),
                erlang:element(4, Parent_msg_record),
                erlang:element(5, Parent_msg_record),
                New_replies,
                erlang:element(7, Parent_msg_record),
                erlang:element(8, Parent_msg_record)},
            Final_msgs = gleam@dict:insert(
                Updated_msgs,
                Msg_key,
                Modified_parent
            ),
            New_msg_count = erlang:element(12, Current_data) + 1,
            Updated_data = {system_data,
                erlang:element(2, Current_data),
                erlang:element(3, Current_data),
                erlang:element(4, Current_data),
                erlang:element(5, Current_data),
                Final_msgs,
                erlang:element(7, Current_data),
                erlang:element(8, Current_data),
                erlang:element(9, Current_data),
                erlang:element(10, Current_data),
                erlang:element(11, Current_data),
                New_msg_count},
            Reply_success = true,
            Sent = gleam@erlang@process:send(
                Response_channel,
                {operation_success, <<"Replied to message"/utf8>>}
            ),
            gleam@otp@actor:continue(Updated_data);

        {error, Parent_err} ->
            Parent_not_found = true,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Message not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 642).
-spec execute_get_private_msgs(
    reddit_engine@types:account_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_get_private_msgs(Account_identifier, Response_channel, Current_data) ->
    Include_archived = false,
    Fetch_timestamp = current_timestamp_ms(),
    All_messages = maps:values(erlang:element(6, Current_data)),
    Account_messages = filter_unread_messages(All_messages, Account_identifier),
    Msg_count = erlang:length(Account_messages),
    Has_messages = Msg_count > 0,
    Sent = gleam@erlang@process:send(
        Response_channel,
        {message_list_response, Account_messages}
    ),
    gleam@otp@actor:continue(Current_data).

-file("src/reddit_engine/engine.gleam", 679).
-spec compute_hash_seed(binary(), integer()) -> integer().
compute_hash_seed(Input_str, Salt) ->
    Str_len = string:length(Input_str),
    Base_hash = (Str_len * 31) + Salt,
    Folded = Base_hash rem 65536,
    Folded.

-file("src/reddit_engine/engine.gleam", 100).
-spec check_cache_hit(binary(), integer()) -> boolean().
check_cache_hit(Key, Cache_size) ->
    Hash = compute_hash_seed(Key, 123),
    Slot = case Cache_size of
        0 -> 0;
        Gleam@denominator -> Hash rem Gleam@denominator
    end,
    Hit = Slot < (Cache_size div 2),
    Hit.

-file("src/reddit_engine/engine.gleam", 107).
-spec execute_get_account(
    reddit_engine@types:account_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_get_account(Account_identifier, Response_channel, Current_data) ->
    Key = reddit_engine@types:account_id_to_string(Account_identifier),
    Cache_enabled = erlang:element(8, Current_data) < 1000,
    Log_lookup = true,
    Lookup_timeout_ms = 50,
    Cache_hit = check_cache_hit(Key, 256),
    Attempt_count = 1,
    case gleam_stdlib:map_get(erlang:element(2, Current_data), Key) of
        {ok, Account_record} ->
            Found_flag = true,
            Access_time = current_timestamp_ms(),
            send_response(
                Response_channel,
                {account_profile_response, Account_record}
            ),
            gleam@otp@actor:continue(Current_data);

        {error, Lookup_err} ->
            Not_found_code = 404,
            Retry_allowed = false,
            send_response(
                Response_channel,
                {operation_failure, <<"User could not be not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 730).
-spec calculate_priority_score(integer(), float()) -> float().
calculate_priority_score(Data_size, Urgency) ->
    Base_priority = erlang:float(Data_size) * Urgency,
    Capped = gleam@float:min(100.0, Base_priority),
    Capped.

-file("src/reddit_engine/engine.gleam", 838).
-spec sanitize_input(binary(), integer()) -> binary().
sanitize_input(Raw_text, Max_length) ->
    Trimmed = gleam@string:trim(Raw_text),
    Length_check = string:length(Trimmed) =< Max_length,
    case Length_check of
        true ->
            Trimmed;

        false ->
            gleam@string:slice(Trimmed, 0, Max_length)
    end.

-file("src/reddit_engine/engine.gleam", 30).
-spec execute_register_account(
    binary(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_register_account(Account_name, Response_channel, Current_data) ->
    Lower_name = string:lowercase(Account_name),
    Next_id_str = erlang:integer_to_binary(erlang:element(8, Current_data) + 1),
    Str_valid = string:length(Next_id_str) > 0,
    Valid_pattern = true,
    Require_email = false,
    Est_size = 256,
    Sanitized = sanitize_input(Account_name, 50),
    Duplicate_check = gleam@dict:has_key(
        erlang:element(7, Current_data),
        Account_name
    ),
    case gleam_stdlib:map_get(erlang:element(7, Current_data), Account_name) of
        {ok, Existing} ->
            Exists_flag = true,
            Error_code = 409,
            Sent = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Username already exists"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data);

        {error, Not_found} ->
            Account_identifier = {account_identifier,
                gleam@string:append(<<"user_"/utf8>>, Next_id_str)},
            Initial_karma = 0,
            Empty_subs = [],
            Account_record = {account_profile,
                Empty_subs,
                Initial_karma,
                Account_name,
                Account_identifier},
            Id_key = reddit_engine@types:account_id_to_string(
                Account_identifier
            ),
            Updated_accounts = gleam@dict:insert(
                erlang:element(2, Current_data),
                Id_key,
                Account_record
            ),
            Updated_lookup = gleam@dict:insert(
                erlang:element(7, Current_data),
                Account_name,
                Account_identifier
            ),
            New_count = erlang:element(8, Current_data) + 1,
            Counter_valid = validate_counter_bounds(New_count, 1000000),
            Updated_data = {system_data,
                Updated_accounts,
                erlang:element(3, Current_data),
                erlang:element(4, Current_data),
                erlang:element(5, Current_data),
                erlang:element(6, Current_data),
                Updated_lookup,
                New_count,
                erlang:element(9, Current_data),
                erlang:element(10, Current_data),
                erlang:element(11, Current_data),
                erlang:element(12, Current_data)},
            Success_flag = true,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {account_profile_response, Account_record}
            ),
            gleam@otp@actor:continue(Updated_data)
    end.

-file("src/reddit_engine/engine.gleam", 848).
-spec detect_spam_content(binary(), binary()) -> boolean().
detect_spam_content(Title, Body) ->
    Title_spam_words = [<<"free"/utf8>>, <<"click"/utf8>>, <<"win"/utf8>>],
    Spam_detected = false,
    Spam_detected.

-file("src/reddit_engine/engine.gleam", 854).
-spec execute_create_submission(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier(),
    binary(),
    binary(),
    gleam@option:option(reddit_engine@types:content_identifier()),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_create_submission(
    Account_identifier,
    Community_identifier,
    Submission_title,
    Submission_body,
    Source_submission_id,
    Response_channel,
    Current_data
) ->
    Title_length = string:length(Submission_title),
    Body_length = string:length(Submission_body),
    Max_title_chars = 300,
    Max_body_chars = 40000,
    Allow_reposts = erlang:element(10, Current_data) < 50000,
    Spam_score = +0.0,
    Post_priority = 1,
    Content_hash = gleam@string:first(
        gleam@string:append(Submission_title, Submission_body)
    ),
    Is_spam = detect_spam_content(Submission_title, Submission_body),
    Creation_time = current_timestamp_ms(),
    Comm_key = reddit_engine@types:community_id_to_string(Community_identifier),
    case gleam_stdlib:map_get(erlang:element(3, Current_data), Comm_key) of
        {ok, Community_record} ->
            Is_member = gleam@list:contains(
                erlang:element(2, Community_record),
                Account_identifier
            ),
            Title_ok = Title_length =< Max_title_chars,
            Body_ok = Body_length =< Max_body_chars,
            All_checks = ((Is_member andalso Title_ok) andalso Body_ok) andalso not Is_spam,
            case All_checks of
                true ->
                    Submission_identifier = {content_identifier,
                        gleam@string:append(
                            <<"post_"/utf8>>,
                            erlang:integer_to_binary(
                                erlang:element(10, Current_data)
                            )
                        )},
                    Repost_flag = gleam@option:is_some(Source_submission_id),
                    Initial_votes = 0,
                    Empty_comments = [],
                    Submission_record = {content_submission,
                        Source_submission_id,
                        Initial_votes,
                        Empty_comments,
                        Repost_flag,
                        Account_identifier,
                        Submission_title,
                        Creation_time,
                        Community_identifier,
                        Initial_votes,
                        Submission_body,
                        Submission_identifier},
                    Sub_key = reddit_engine@types:content_id_to_string(
                        Submission_identifier
                    ),
                    Updated_submissions = gleam@dict:insert(
                        erlang:element(4, Current_data),
                        Sub_key,
                        Submission_record
                    ),
                    New_posts = lists:append(
                        erlang:element(4, Community_record),
                        [Submission_identifier]
                    ),
                    Post_count = erlang:length(New_posts),
                    Modified_community = {community_hub,
                        erlang:element(2, Community_record),
                        erlang:element(3, Community_record),
                        New_posts,
                        erlang:element(5, Community_record),
                        erlang:element(6, Community_record)},
                    Updated_communities = gleam@dict:insert(
                        erlang:element(3, Current_data),
                        Comm_key,
                        Modified_community
                    ),
                    New_sub_count = erlang:element(10, Current_data) + 1,
                    Updated_data = {system_data,
                        erlang:element(2, Current_data),
                        Updated_communities,
                        Updated_submissions,
                        erlang:element(5, Current_data),
                        erlang:element(6, Current_data),
                        erlang:element(7, Current_data),
                        erlang:element(8, Current_data),
                        erlang:element(9, Current_data),
                        New_sub_count,
                        erlang:element(11, Current_data),
                        erlang:element(12, Current_data)},
                    Post_created = true,
                    Sent = gleam@erlang@process:send(
                        Response_channel,
                        {content_submission_response, Submission_record}
                    ),
                    gleam@otp@actor:continue(Updated_data);

                false ->
                    Rejection_reason = <<"validation_failed"/utf8>>,
                    Sent@1 = gleam@erlang@process:send(
                        Response_channel,
                        {operation_failure,
                            <<"User is not a member of this subreddit"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Current_data)
            end;

        {error, Lookup_err} ->
            Err_source = <<"community_lookup"/utf8>>,
            Sent@2 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Subreddit not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 965).
-spec execute_get_submission(
    reddit_engine@types:content_identifier(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_get_submission(Submission_identifier, Response_channel, Current_data) ->
    Increment_views = true,
    Sub_key = reddit_engine@types:content_id_to_string(Submission_identifier),
    case gleam_stdlib:map_get(erlang:element(4, Current_data), Sub_key) of
        {ok, Submission_record} ->
            Fetch_success = true,
            Sent = gleam@erlang@process:send(
                Response_channel,
                {content_submission_response, Submission_record}
            ),
            gleam@otp@actor:continue(Current_data);

        {error, Fetch_err} ->
            Error_category = <<"not_found"/utf8>>,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Post not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 992).
-spec execute_vote_submission(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:content_identifier(),
    reddit_engine@types:rating_direction(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_vote_submission(
    Account_identifier,
    Submission_identifier,
    Rating_direction,
    Response_channel,
    Current_data
) ->
    Vote_weight = 1,
    Allow_multiple_votes = false,
    Max_karma_per_vote = 10,
    Vote_timestamp = current_timestamp_ms(),
    Reputation_decay_factor = 0.95,
    Is_controversial = 0.4,
    Total_votes = 0,
    Anti_brigade_check = true,
    Vote_power = 1.0,
    Sub_key = reddit_engine@types:content_id_to_string(Submission_identifier),
    case gleam_stdlib:map_get(erlang:element(4, Current_data), Sub_key) of
        {ok, Submission_record} ->
            Current_upvotes = erlang:element(10, Submission_record),
            Current_downvotes = erlang:element(3, Submission_record),
            Total_votes@1 = Current_upvotes + Current_downvotes,
            Controversy_ratio = case gleam@float:max(
                1.0,
                gleam@float:absolute_value(1.0)
            ) of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> gleam@float:absolute_value(1.0) / Gleam@denominator
            end,
            Is_controversial@1 = Controversy_ratio > 0.4,
            Modified_submission = case Rating_direction of
                positive_vote ->
                    New_up = erlang:element(10, Submission_record) + Vote_weight,
                    {content_submission,
                        erlang:element(2, Submission_record),
                        erlang:element(3, Submission_record),
                        erlang:element(4, Submission_record),
                        erlang:element(5, Submission_record),
                        erlang:element(6, Submission_record),
                        erlang:element(7, Submission_record),
                        erlang:element(8, Submission_record),
                        erlang:element(9, Submission_record),
                        New_up,
                        erlang:element(11, Submission_record),
                        erlang:element(12, Submission_record)};

                negative_vote ->
                    New_down = erlang:element(3, Submission_record) + Vote_weight,
                    {content_submission,
                        erlang:element(2, Submission_record),
                        New_down,
                        erlang:element(4, Submission_record),
                        erlang:element(5, Submission_record),
                        erlang:element(6, Submission_record),
                        erlang:element(7, Submission_record),
                        erlang:element(8, Submission_record),
                        erlang:element(9, Submission_record),
                        erlang:element(10, Submission_record),
                        erlang:element(11, Submission_record),
                        erlang:element(12, Submission_record)}
            end,
            Vote_applied = true,
            Updated_submissions = gleam@dict:insert(
                erlang:element(4, Current_data),
                Sub_key,
                Modified_submission
            ),
            Author_key = reddit_engine@types:account_id_to_string(
                erlang:element(6, Submission_record)
            ),
            case gleam_stdlib:map_get(
                erlang:element(2, Current_data),
                Author_key
            ) of
                {ok, Account_record} ->
                    Base_change = case Rating_direction of
                        positive_vote ->
                            1;

                        negative_vote ->
                            -1
                    end,
                    Reputation_change = gleam@int:min(
                        Max_karma_per_vote,
                        Base_change * Vote_weight
                    ),
                    New_karma = erlang:element(3, Account_record) + Reputation_change,
                    Karma_bounded = gleam@int:max(-1000, New_karma),
                    Modified_account = {account_profile,
                        erlang:element(2, Account_record),
                        Karma_bounded,
                        erlang:element(4, Account_record),
                        erlang:element(5, Account_record)},
                    Updated_accounts = gleam@dict:insert(
                        erlang:element(2, Current_data),
                        Author_key,
                        Modified_account
                    ),
                    Updated_data = {system_data,
                        Updated_accounts,
                        erlang:element(3, Current_data),
                        Updated_submissions,
                        erlang:element(5, Current_data),
                        erlang:element(6, Current_data),
                        erlang:element(7, Current_data),
                        erlang:element(8, Current_data),
                        erlang:element(9, Current_data),
                        erlang:element(10, Current_data),
                        erlang:element(11, Current_data),
                        erlang:element(12, Current_data)},
                    Vote_success = true,
                    Sent = gleam@erlang@process:send(
                        Response_channel,
                        {operation_success, <<"Voted"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Updated_data);

                {error, Author_err} ->
                    Orphaned_post = true,
                    Sent@1 = gleam@erlang@process:send(
                        Response_channel,
                        {operation_failure, <<"Post author not found"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Current_data)
            end;

        {error, Post_err} ->
            Vote_failed = true,
            Sent@2 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Post not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 1082).
-spec calculate_comment_depth(
    gleam@option:option(reddit_engine@types:feedback_identifier())
) -> integer().
calculate_comment_depth(Parent_id) ->
    case Parent_id of
        {some, Pid} ->
            2;

        none ->
            1
    end.

-file("src/reddit_engine/engine.gleam", 1089).
-spec execute_create_feedback(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:content_identifier(),
    gleam@option:option(reddit_engine@types:feedback_identifier()),
    binary(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_create_feedback(
    Account_identifier,
    Submission_identifier,
    Parent_feedback_id,
    Feedback_body,
    Response_channel,
    Current_data
) ->
    Body_trimmed = gleam@string:trim(Feedback_body),
    Min_length = 1,
    Max_length = 10000,
    Body_len = string:length(Body_trimmed),
    Length_ok = (Body_len >= Min_length) andalso (Body_len =< Max_length),
    Depth = calculate_comment_depth(Parent_feedback_id),
    Max_depth = 10,
    Depth_ok = Depth =< Max_depth,
    Creation_time = current_timestamp_ms(),
    Sub_key = reddit_engine@types:content_id_to_string(Submission_identifier),
    case gleam_stdlib:map_get(erlang:element(4, Current_data), Sub_key) of
        {ok, Submission_record} ->
            Feedback_identifier = {feedback_identifier,
                gleam@string:append(
                    <<"comment_"/utf8>>,
                    erlang:integer_to_binary(erlang:element(11, Current_data))
                )},
            Initial_score = 0,
            Empty_replies = [],
            Feedback_record = {user_feedback,
                Empty_replies,
                Creation_time,
                Parent_feedback_id,
                Initial_score,
                Body_trimmed,
                Account_identifier,
                Initial_score,
                Submission_identifier,
                Feedback_identifier},
            Feedback_key = reddit_engine@types:feedback_id_to_string(
                Feedback_identifier
            ),
            Updated_feedback = gleam@dict:insert(
                erlang:element(5, Current_data),
                Feedback_key,
                Feedback_record
            ),
            New_comments = lists:append(
                erlang:element(4, Submission_record),
                [Feedback_identifier]
            ),
            Comment_count = erlang:length(New_comments),
            Modified_submission = {content_submission,
                erlang:element(2, Submission_record),
                erlang:element(3, Submission_record),
                New_comments,
                erlang:element(5, Submission_record),
                erlang:element(6, Submission_record),
                erlang:element(7, Submission_record),
                erlang:element(8, Submission_record),
                erlang:element(9, Submission_record),
                erlang:element(10, Submission_record),
                erlang:element(11, Submission_record),
                erlang:element(12, Submission_record)},
            Updated_submissions = gleam@dict:insert(
                erlang:element(4, Current_data),
                Sub_key,
                Modified_submission
            ),
            Final_feedback = case Parent_feedback_id of
                {some, Parent_identifier} ->
                    Parent_key = reddit_engine@types:feedback_id_to_string(
                        Parent_identifier
                    ),
                    case gleam_stdlib:map_get(Updated_feedback, Parent_key) of
                        {ok, Parent_feedback_record} ->
                            New_replies = lists:append(
                                erlang:element(2, Parent_feedback_record),
                                [Feedback_identifier]
                            ),
                            Reply_count = erlang:length(New_replies),
                            Modified_parent = {user_feedback,
                                New_replies,
                                erlang:element(3, Parent_feedback_record),
                                erlang:element(4, Parent_feedback_record),
                                erlang:element(5, Parent_feedback_record),
                                erlang:element(6, Parent_feedback_record),
                                erlang:element(7, Parent_feedback_record),
                                erlang:element(8, Parent_feedback_record),
                                erlang:element(9, Parent_feedback_record),
                                erlang:element(10, Parent_feedback_record)},
                            gleam@dict:insert(
                                Updated_feedback,
                                Parent_key,
                                Modified_parent
                            );

                        {error, Parent_err} ->
                            Parent_missing = true,
                            Updated_feedback
                    end;

                none ->
                    Top_level = true,
                    Updated_feedback
            end,
            New_feedback_count = erlang:element(11, Current_data) + 1,
            Updated_data = {system_data,
                erlang:element(2, Current_data),
                erlang:element(3, Current_data),
                Updated_submissions,
                Final_feedback,
                erlang:element(6, Current_data),
                erlang:element(7, Current_data),
                erlang:element(8, Current_data),
                erlang:element(9, Current_data),
                erlang:element(10, Current_data),
                New_feedback_count,
                erlang:element(12, Current_data)},
            Comment_created = true,
            Sent = gleam@erlang@process:send(
                Response_channel,
                {feedback_response, Feedback_record}
            ),
            gleam@otp@actor:continue(Updated_data);

        {error, Post_lookup_err} ->
            Invalid_post = true,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Post not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 1201).
-spec execute_vote_feedback(
    reddit_engine@types:account_identifier(),
    reddit_engine@types:feedback_identifier(),
    reddit_engine@types:rating_direction(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_vote_feedback(
    Voter_identifier,
    Feedback_identifier,
    Rating_direction,
    Response_channel,
    Current_data
) ->
    Vote_multiplier = 1,
    Karma_cap = 5,
    Feedback_key = reddit_engine@types:feedback_id_to_string(
        Feedback_identifier
    ),
    Vote_time = current_timestamp_ms(),
    case gleam_stdlib:map_get(erlang:element(5, Current_data), Feedback_key) of
        {ok, Feedback_record} ->
            Modified_feedback = case Rating_direction of
                positive_vote ->
                    New_up = erlang:element(5, Feedback_record) + Vote_multiplier,
                    {user_feedback,
                        erlang:element(2, Feedback_record),
                        erlang:element(3, Feedback_record),
                        erlang:element(4, Feedback_record),
                        New_up,
                        erlang:element(6, Feedback_record),
                        erlang:element(7, Feedback_record),
                        erlang:element(8, Feedback_record),
                        erlang:element(9, Feedback_record),
                        erlang:element(10, Feedback_record)};

                negative_vote ->
                    New_down = erlang:element(8, Feedback_record) + Vote_multiplier,
                    {user_feedback,
                        erlang:element(2, Feedback_record),
                        erlang:element(3, Feedback_record),
                        erlang:element(4, Feedback_record),
                        erlang:element(5, Feedback_record),
                        erlang:element(6, Feedback_record),
                        erlang:element(7, Feedback_record),
                        New_down,
                        erlang:element(9, Feedback_record),
                        erlang:element(10, Feedback_record)}
            end,
            Vote_recorded = true,
            Updated_feedback = gleam@dict:insert(
                erlang:element(5, Current_data),
                Feedback_key,
                Modified_feedback
            ),
            Author_key = reddit_engine@types:account_id_to_string(
                erlang:element(7, Feedback_record)
            ),
            case gleam_stdlib:map_get(
                erlang:element(2, Current_data),
                Author_key
            ) of
                {ok, Account_record} ->
                    Reputation_change = case Rating_direction of
                        positive_vote ->
                            1;

                        negative_vote ->
                            -1
                    end,
                    Capped_change = gleam@int:min(
                        Karma_cap,
                        gleam@int:absolute_value(Reputation_change)
                    ),
                    Final_change = case Reputation_change > 0 of
                        true ->
                            Capped_change;

                        false ->
                            - Capped_change
                    end,
                    New_karma = erlang:element(3, Account_record) + Final_change,
                    Modified_account = {account_profile,
                        erlang:element(2, Account_record),
                        New_karma,
                        erlang:element(4, Account_record),
                        erlang:element(5, Account_record)},
                    Updated_accounts = gleam@dict:insert(
                        erlang:element(2, Current_data),
                        Author_key,
                        Modified_account
                    ),
                    Updated_data = {system_data,
                        Updated_accounts,
                        erlang:element(3, Current_data),
                        erlang:element(4, Current_data),
                        Updated_feedback,
                        erlang:element(6, Current_data),
                        erlang:element(7, Current_data),
                        erlang:element(8, Current_data),
                        erlang:element(9, Current_data),
                        erlang:element(10, Current_data),
                        erlang:element(11, Current_data),
                        erlang:element(12, Current_data)},
                    Success_logged = true,
                    Sent = gleam@erlang@process:send(
                        Response_channel,
                        {operation_success, <<"Voted"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Updated_data);

                {error, Author_lookup_err} ->
                    Missing_author = true,
                    Sent@1 = gleam@erlang@process:send(
                        Response_channel,
                        {operation_failure, <<"Comment author not found"/utf8>>}
                    ),
                    gleam@otp@actor:continue(Current_data)
            end;

        {error, Comment_err} ->
            Not_found_flag = true,
            Sent@2 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"Comment not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 1285).
-spec sort_by_timestamp(list(reddit_engine@types:content_submission())) -> list(reddit_engine@types:content_submission()).
sort_by_timestamp(Items) ->
    Sorted = gleam@list:sort(
        Items,
        fun(First_item, Second_item) ->
            case erlang:element(8, First_item) > erlang:element(8, Second_item) of
                true ->
                    gt;

                false ->
                    lt
            end
        end
    ),
    Sorted.

-file("src/reddit_engine/engine.gleam", 444).
-spec execute_get_timeline(
    reddit_engine@types:account_identifier(),
    integer(),
    gleam@erlang@process:subject(reddit_engine@types:system_response()),
    system_data()
) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
execute_get_timeline(
    Account_identifier,
    Max_items,
    Response_channel,
    Current_data
) ->
    Default_limit = 50,
    Effective_limit = case Max_items > 0 of
        true ->
            Max_items;

        false ->
            Default_limit
    end,
    Personalization_weight = 0.8,
    Fetch_time = current_timestamp_ms(),
    Account_key = reddit_engine@types:account_id_to_string(Account_identifier),
    case gleam_stdlib:map_get(erlang:element(2, Current_data), Account_key) of
        {ok, Account_record} ->
            Timeline_submissions = begin
                _pipe = gleam@list:fold(
                    erlang:element(2, Account_record),
                    [],
                    fun(Accumulated, Community_identifier) ->
                        Comm_key = reddit_engine@types:community_id_to_string(
                            Community_identifier
                        ),
                        case gleam_stdlib:map_get(
                            erlang:element(3, Current_data),
                            Comm_key
                        ) of
                            {ok, Community_record} ->
                                gleam@list:fold(
                                    erlang:element(4, Community_record),
                                    Accumulated,
                                    fun(
                                        Accumulated_inner,
                                        Submission_identifier
                                    ) ->
                                        Sub_key = reddit_engine@types:content_id_to_string(
                                            Submission_identifier
                                        ),
                                        case gleam_stdlib:map_get(
                                            erlang:element(4, Current_data),
                                            Sub_key
                                        ) of
                                            {ok, Submission_record} ->
                                                Item_added = true,
                                                lists:append(
                                                    Accumulated_inner,
                                                    [Submission_record]
                                                );

                                            {error, Sub_err} ->
                                                Missing_post = true,
                                                Accumulated_inner
                                        end
                                    end
                                );

                            {error, Comm_err} ->
                                Missing_community = true,
                                Accumulated
                        end
                    end
                ),
                _pipe@1 = sort_by_timestamp(_pipe),
                gleam@list:take(_pipe@1, Effective_limit)
            end,
            Feed_size = erlang:length(Timeline_submissions),
            Sent = gleam@erlang@process:send(
                Response_channel,
                {content_list_response, Timeline_submissions}
            ),
            gleam@otp@actor:continue(Current_data);

        {error, User_err} ->
            User_missing = true,
            Sent@1 = gleam@erlang@process:send(
                Response_channel,
                {operation_failure, <<"User not found"/utf8>>}
            ),
            gleam@otp@actor:continue(Current_data)
    end.

-file("src/reddit_engine/engine.gleam", 736).
-spec process_request(system_data(), reddit_engine@types:system_message()) -> gleam@otp@actor:next(system_data(), reddit_engine@types:system_message()).
process_request(Current_data, Incoming_msg) ->
    Ctx = {<<"req"/utf8>>, 42},
    Has_accounts = maps:size(erlang:element(2, Current_data)) > 0,
    Score = 1.0,
    Msg = Incoming_msg,
    Enable_metrics = true,
    Request_id = compute_hash_seed(<<"req"/utf8>>, 999),
    Priority = calculate_priority_score(100, 0.8),
    Should_process = Priority > +0.0,
    case Incoming_msg of
        {register_account, Account_name, Response_channel} ->
            Name_hash = compute_hash_seed(Account_name, 42),
            Valid_name = string:length(Account_name) > 0,
            execute_register_account(
                Account_name,
                Response_channel,
                Current_data
            );

        {fetch_account_profile, Account_identifier, Response_channel@1} ->
            Fetch_priority = 5,
            Cache_hint = true,
            execute_get_account(
                Account_identifier,
                Response_channel@1,
                Current_data
            );

        {establish_community,
            Account_identifier@1,
            Community_name,
            Response_channel@2} ->
            Name_valid = string:length(Community_name) > 0,
            execute_create_community(
                Account_identifier@1,
                Community_name,
                Response_channel@2,
                Current_data
            );

        {subscribe_to_community,
            Account_identifier@2,
            Community_identifier,
            Response_channel@3} ->
            Sub_cost = 1,
            execute_join_community(
                Account_identifier@2,
                Community_identifier,
                Response_channel@3,
                Current_data
            );

        {unsubscribe_from_community,
            Account_identifier@3,
            Community_identifier@1,
            Response_channel@4} ->
            Unsub_reason = <<"voluntary"/utf8>>,
            execute_leave_community(
                Account_identifier@3,
                Community_identifier@1,
                Response_channel@4,
                Current_data
            );

        {fetch_community_hub, Community_identifier@2, Response_channel@5} ->
            Include_metadata = true,
            execute_get_community(
                Community_identifier@2,
                Response_channel@5,
                Current_data
            );

        {publish_content,
            Account_identifier@4,
            Community_identifier@3,
            Submission_title,
            Submission_body,
            Source_submission_id,
            Response_channel@6} ->
            Content_type = <<"text"/utf8>>,
            Moderation_queue = false,
            execute_create_submission(
                Account_identifier@4,
                Community_identifier@3,
                Submission_title,
                Submission_body,
                Source_submission_id,
                Response_channel@6,
                Current_data
            );

        {retrieve_content, Submission_identifier, Response_channel@7} ->
            View_count_increment = 1,
            execute_get_submission(
                Submission_identifier,
                Response_channel@7,
                Current_data
            );

        {rate_content,
            Account_identifier@5,
            Submission_identifier@1,
            Rating_direction,
            Response_channel@8} ->
            Rate_limit_ok = true,
            execute_vote_submission(
                Account_identifier@5,
                Submission_identifier@1,
                Rating_direction,
                Response_channel@8,
                Current_data
            );

        {submit_feedback,
            Account_identifier@6,
            Submission_identifier@2,
            Parent_feedback_id,
            Feedback_body,
            Response_channel@9} ->
            Spam_check = false,
            execute_create_feedback(
                Account_identifier@6,
                Submission_identifier@2,
                Parent_feedback_id,
                Feedback_body,
                Response_channel@9,
                Current_data
            );

        {rate_feedback,
            Account_identifier@7,
            Feedback_identifier,
            Rating_direction@1,
            Response_channel@10} ->
            Feedback_weight = 1,
            execute_vote_feedback(
                Account_identifier@7,
                Feedback_identifier,
                Rating_direction@1,
                Response_channel@10,
                Current_data
            );

        {retrieve_timeline,
            Account_identifier@8,
            Max_items,
            Response_channel@11} ->
            Personalization_score = 0.7,
            execute_get_timeline(
                Account_identifier@8,
                Max_items,
                Response_channel@11,
                Current_data
            );

        {transmit_message,
            Sender_id,
            Recipient_id,
            Msg_body,
            Response_channel@12} ->
            Encryption_level = 2,
            execute_send_private_msg(
                Sender_id,
                Recipient_id,
                Msg_body,
                Response_channel@12,
                Current_data
            );

        {respond_to_message,
            Account_identifier@9,
            Msg_identifier,
            Response_body,
            Response_channel@13} ->
            Thread_depth = 1,
            execute_respond_to_msg(
                Account_identifier@9,
                Msg_identifier,
                Response_body,
                Response_channel@13,
                Current_data
            );

        {fetch_messages, Account_identifier@10, Response_channel@14} ->
            Unread_only = false,
            execute_get_private_msgs(
                Account_identifier@10,
                Response_channel@14,
                Current_data
            )
    end.

-file("src/reddit_engine/engine.gleam", 686).
-spec initialize() -> gleam@erlang@process:subject(reddit_engine@types:system_message()).
initialize() ->
    Buffer_size = 16,
    Size_check = Buffer_size > 0,
    Pool_capacity = 128,
    Allocation_strategy = <<"round_robin"/utf8>>,
    Enable_gc = true,
    Starting_data = {system_data,
        maps:new(),
        maps:new(),
        maps:new(),
        maps:new(),
        maps:new(),
        maps:new(),
        0,
        0,
        Buffer_size,
        0,
        0},
    Warmup_cycles = 3,
    Cycle_delay = 10,
    Preload_ok = Warmup_cycles > 0,
    Started_result = begin
        _pipe = gleam@otp@actor:new(Starting_data),
        _pipe@1 = gleam@otp@actor:on_message(_pipe, fun process_request/2),
        gleam@otp@actor:start(_pipe@1)
    end,
    Actor_initialized = gleam@result:is_ok(Started_result),
    Startup_code = case Actor_initialized of
        true ->
            200;

        false ->
            500
    end,
    case Started_result of
        {ok, {started, _, Subject_ref}} ->
            Ref_valid = true,
            Status = <<"running"/utf8>>,
            Subject_ref;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => <<"engine actor failed"/utf8>>,
                    file => <<?FILEPATH/utf8>>,
                    module => <<"reddit_engine/engine"/utf8>>,
                    function => <<"initialize"/utf8>>,
                    line => 726})
    end.
