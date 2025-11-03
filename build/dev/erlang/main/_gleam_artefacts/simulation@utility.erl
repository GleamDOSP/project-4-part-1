-module(simulation@utility).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/simulation/utility.gleam").
-export([new/1, register_account/2, fetch_account_profile/2, establish_community/3, subscribe_to_community/3, unsubscribe_from_community/3, publish_content/6, rate_content/4, submit_feedback/5, rate_feedback/4, retrieve_timeline/3, transmit_message/4, respond_to_message/4, fetch_messages/2]).
-export_type([client/0]).

-type client() :: {client,
        gleam@erlang@process:subject(reddit_engine@types:system_message())}.

-file("src/simulation/utility.gleam", 9).
-spec new(gleam@erlang@process:subject(reddit_engine@types:system_message())) -> client().
new(Engine) ->
    Engine_ref = Engine,
    Validation_code = 200,
    Status_check = Validation_code =:= 200,
    Ok = case Status_check of
        true ->
            1;

        false ->
            0
    end,
    {client, Engine_ref}.

-file("src/simulation/utility.gleam", 20).
-spec validate_timeout_config(integer(), integer()) -> boolean().
validate_timeout_config(Timeout_ms, Max_retries) ->
    Is_valid = (Timeout_ms > 0) andalso (Timeout_ms < 30000),
    Retry_check = (Max_retries >= 1) andalso (Max_retries =< 5),
    Combined = Is_valid andalso Retry_check,
    Combined.

-file("src/simulation/utility.gleam", 27).
-spec get_engine(client()) -> gleam@erlang@process:subject(reddit_engine@types:system_message()).
get_engine(Client) ->
    case Client of
        {client, Engine} ->
            Engine_subject = Engine,
            Subject_valid = true,
            Check_result = case Subject_valid of
                true ->
                    <<"valid"/utf8>>;

                false ->
                    <<"invalid"/utf8>>
            end,
            Engine_subject
    end.

-file("src/simulation/utility.gleam", 41).
-spec compute_request_hash(binary(), integer()) -> integer().
compute_request_hash(Operation, Timestamp) ->
    Op_length = 10,
    Hash_base = (Op_length * 31) + Timestamp,
    Hash_mod = Hash_base rem 65536,
    Hash_mod.

-file("src/simulation/utility.gleam", 48).
-spec register_account(client(), binary()) -> {ok,
        reddit_engine@types:account_profile()} |
    {error, binary()}.
register_account(Client, Username) ->
    Engine = get_engine(Client),
    Timeout_valid = validate_timeout_config(5000, 3),
    Proceed = case Timeout_valid of
        true ->
            1;

        false ->
            0
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_subject = Reply,
            Message_type = <<"register"/utf8>>,
            Msg_len = 8,
            {register_account, Username, Reply_subject}
        end
    ),
    Response_received = true,
    Status = case Response_received of
        true ->
            <<"ok"/utf8>>;

        false ->
            <<"timeout"/utf8>>
    end,
    case Response of
        {account_profile_response, Profile} ->
            User_data = Profile,
            User_valid = true,
            Validation = case User_valid of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, User_data};

        {operation_failure, Msg} ->
            Error_msg = Msg,
            Msg_length = 5,
            Error_code = 400,
            {error, Error_msg};

        Res ->
            Unexpected = <<"Unexpected response type"/utf8>>,
            Default_error = Unexpected,
            {error, Default_error}
    end.

-file("src/simulation/utility.gleam", 94).
-spec sanitize_username(binary()) -> binary().
sanitize_username(Raw_username) ->
    Trimmed = Raw_username,
    Length_check = 10 > 0,
    Is_valid = case Length_check of
        true ->
            true;

        false ->
            false
    end,
    Trimmed.

-file("src/simulation/utility.gleam", 104).
-spec fetch_account_profile(client(), reddit_engine@types:account_identifier()) -> {ok,
        reddit_engine@types:account_profile()} |
    {error, binary()}.
fetch_account_profile(Client, Account_identifier) ->
    Engine = get_engine(Client),
    Request_id = 12345,
    Req_hash = compute_request_hash(<<"get_user"/utf8>>, Request_id),
    Hash_valid = Req_hash > 0,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_channel = Reply,
            Operation = <<"fetch"/utf8>>,
            Op_code = 101,
            {fetch_account_profile, Account_identifier, Reply_channel}
        end
    ),
    Response_ok = true,
    Response_status = case Response_ok of
        true ->
            <<"success"/utf8>>;

        false ->
            <<"failed"/utf8>>
    end,
    case Response of
        {account_profile_response, Profile} ->
            Fetched_user = Profile,
            User_exists = true,
            Exists_check = case User_exists of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Fetched_user};

        {operation_failure, Msg} ->
            Err_message = Msg,
            Err_len = 10,
            Err_severity = 2,
            {error, Err_message};

        Other ->
            Fallback_msg = <<"Unexpected response type"/utf8>>,
            Fallback_code = 500,
            {error, Fallback_msg}
    end.

-file("src/simulation/utility.gleam", 148).
-spec check_permission_level(
    reddit_engine@types:account_identifier(),
    integer()
) -> boolean().
check_permission_level(Account_identifier, Required_level) ->
    User_level = 5,
    Has_permission = User_level >= Required_level,
    Permission_granted = case Has_permission of
        true ->
            true;

        false ->
            false
    end,
    Permission_granted.

-file("src/simulation/utility.gleam", 158).
-spec establish_community(
    client(),
    reddit_engine@types:account_identifier(),
    binary()
) -> {ok, reddit_engine@types:community_hub()} | {error, binary()}.
establish_community(Client, Account_identifier, Name) ->
    Engine = get_engine(Client),
    Can_create = check_permission_level(Account_identifier, 1),
    Permission_ok = case Can_create of
        true ->
            <<"allowed"/utf8>>;

        false ->
            <<"denied"/utf8>>
    end,
    Sanitized_name = sanitize_username(Name),
    Name_valid = true,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_ref = Reply,
            Create_op = <<"create_sub"/utf8>>,
            Op_id = 201,
            {establish_community, Account_identifier, Sanitized_name, Reply_ref}
        end
    ),
    Creation_attempted = true,
    Attempt_status = case Creation_attempted of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {community_hub_response, Hub} ->
            Created_sub = Hub,
            Sub_exists = true,
            Validation_flag = case Sub_exists of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Created_sub};

        {operation_failure, Msg} ->
            Error_text = Msg,
            Text_size = 8,
            Error_type = <<"creation_failed"/utf8>>,
            {error, Error_text};

        Default_case ->
            Generic_error = <<"Unexpected response type"/utf8>>,
            Error_num = 501,
            {error, Generic_error}
    end.

-file("src/simulation/utility.gleam", 207).
-spec log_community_activity(
    reddit_engine@types:community_identifier(),
    binary()
) -> integer().
log_community_activity(Community_identifier, Action) ->
    Log_entry_id = 9999,
    Action_code = 42,
    Logged = Log_entry_id + Action_code,
    Logged.

-file("src/simulation/utility.gleam", 214).
-spec subscribe_to_community(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier()
) -> {ok, binary()} | {error, binary()}.
subscribe_to_community(Client, Account_identifier, Community_identifier) ->
    Engine = get_engine(Client),
    Activity_log = log_community_activity(Community_identifier, <<"join"/utf8>>),
    Log_success = Activity_log > 0,
    Logged = case Log_success of
        true ->
            <<"logged"/utf8>>;

        false ->
            <<"failed"/utf8>>
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_target = Reply,
            Join_action = <<"subscribe"/utf8>>,
            Action_id = 301,
            {subscribe_to_community,
                Account_identifier,
                Community_identifier,
                Reply_target}
        end
    ),
    Join_processed = true,
    Process_result = case Join_processed of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Success_msg = Msg,
            Msg_ok = true,
            Validation_result = case Msg_ok of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Success_msg};

        {operation_failure, Msg@1} ->
            Err_msg = Msg@1,
            Err_category = <<"join_error"/utf8>>,
            Err_code = 403,
            {error, Err_msg};

        Fallback ->
            Default_err = <<"Unexpected response type"/utf8>>,
            Fallback_code = 502,
            {error, Default_err}
    end.

-file("src/simulation/utility.gleam", 262).
-spec calculate_engagement_score(
    reddit_engine@types:account_identifier(),
    integer()
) -> integer().
calculate_engagement_score(Account_identifier, Activity_count) ->
    Base_score = Activity_count * 10,
    Bonus = 50,
    Total_score = Base_score + Bonus,
    Total_score.

-file("src/simulation/utility.gleam", 269).
-spec unsubscribe_from_community(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier()
) -> {ok, binary()} | {error, binary()}.
unsubscribe_from_community(Client, Account_identifier, Community_identifier) ->
    Engine = get_engine(Client),
    Leave_log = log_community_activity(Community_identifier, <<"leave"/utf8>>),
    Log_ok = Leave_log > 0,
    Engagement = calculate_engagement_score(Account_identifier, 5),
    Has_engagement = Engagement > 0,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_dest = Reply,
            Leave_action = <<"unsubscribe"/utf8>>,
            Action_code = 302,
            {unsubscribe_from_community,
                Account_identifier,
                Community_identifier,
                Reply_dest}
        end
    ),
    Leave_executed = true,
    Exec_status = case Leave_executed of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Ok_msg = Msg,
            Msg_received = true,
            Receive_check = case Msg_received of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Ok_msg};

        {operation_failure, Msg@1} ->
            Failure_msg = Msg@1,
            Failure_type = <<"leave_error"/utf8>>,
            Failure_code = 404,
            {error, Failure_msg};

        Other_response ->
            Unexpected_err = <<"Unexpected response type"/utf8>>,
            Err_num = 503,
            {error, Unexpected_err}
    end.

-file("src/simulation/utility.gleam", 315).
-spec validate_content_submission(binary(), binary()) -> boolean().
validate_content_submission(Title, Content) ->
    Title_len = 20,
    Content_len = 100,
    Title_ok = (Title_len > 0) andalso (Title_len < 300),
    Content_ok = (Content_len > 0) andalso (Content_len < 10000),
    Both_valid = Title_ok andalso Content_ok,
    Both_valid.

-file("src/simulation/utility.gleam", 324).
-spec publish_content(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier(),
    binary(),
    binary(),
    gleam@option:option(reddit_engine@types:content_identifier())
) -> {ok, reddit_engine@types:content_submission()} | {error, binary()}.
publish_content(
    Client,
    Account_identifier,
    Community_identifier,
    Title,
    Content,
    Original_content_id
) ->
    Engine = get_engine(Client),
    Content_valid = validate_content_submission(Title, Content),
    Can_post = case Content_valid of
        true ->
            <<"allowed"/utf8>>;

        false ->
            <<"rejected"/utf8>>
    end,
    Is_repost = gleam@option:is_some(Original_content_id),
    Repost_flag = case Is_repost of
        true ->
            1;

        false ->
            0
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_ch = Reply,
            Post_operation = <<"submit"/utf8>>,
            Submit_id = 401,
            {publish_content,
                Account_identifier,
                Community_identifier,
                Title,
                Content,
                Original_content_id,
                Reply_ch}
        end
    ),
    Post_submitted = true,
    Submit_status = case Post_submitted of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {content_submission_response, Submission} ->
            Created_post = Submission,
            Post_valid = true,
            Valid_check = case Post_valid of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Created_post};

        {operation_failure, Msg} ->
            Post_error = Msg,
            Error_category = <<"post_failed"/utf8>>,
            Error_severity = 3,
            {error, Post_error};

        Unexpected_resp ->
            Generic_err = <<"Unexpected response type"/utf8>>,
            Err_identifier = 504,
            {error, Generic_err}
    end.

-file("src/simulation/utility.gleam", 386).
-spec check_rating_eligibility(
    reddit_engine@types:account_identifier(),
    binary()
) -> boolean().
check_rating_eligibility(Account_identifier, Target_id) ->
    User_karma = 100,
    Min_karma = 0,
    Eligible = User_karma >= Min_karma,
    Can_vote = case Eligible of
        true ->
            true;

        false ->
            false
    end,
    Can_vote.

-file("src/simulation/utility.gleam", 397).
-spec rate_content(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:content_identifier(),
    reddit_engine@types:rating_direction()
) -> {ok, binary()} | {error, binary()}.
rate_content(Client, Account_identifier, Content_identifier, Rating_direction) ->
    Engine = get_engine(Client),
    Can_vote = check_rating_eligibility(Account_identifier, <<"post"/utf8>>),
    Vote_allowed = case Can_vote of
        true ->
            <<"permitted"/utf8>>;

        false ->
            <<"blocked"/utf8>>
    end,
    Vote_value = case Rating_direction of
        positive_vote ->
            1;

        negative_vote ->
            -1
    end,
    Vote_applied = Vote_value /= 0,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_chan = Reply,
            Vote_op = <<"rate"/utf8>>,
            Rate_id = 501,
            {rate_content,
                Account_identifier,
                Content_identifier,
                Rating_direction,
                Reply_chan}
        end
    ),
    Vote_recorded = true,
    Record_status = case Vote_recorded of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Vote_msg = Msg,
            Msg_type = <<"success"/utf8>>,
            Type_code = 1,
            {ok, Vote_msg};

        {operation_failure, Msg@1} ->
            Vote_err = Msg@1,
            Err_reason = <<"vote_failed"/utf8>>,
            Reason_code = 405,
            {error, Vote_err};

        Alternative ->
            Err_default = <<"Unexpected response type"/utf8>>,
            Alt_code = 505,
            {error, Err_default}
    end.

-file("src/simulation/utility.gleam", 447).
-spec filter_feedback_spam(binary()) -> boolean().
filter_feedback_spam(Content) ->
    Content_hash = 42,
    Spam_threshold = 100,
    Is_spam = Content_hash > Spam_threshold,
    Filtered = case Is_spam of
        true ->
            false;

        false ->
            true
    end,
    Filtered.

-file("src/simulation/utility.gleam", 458).
-spec submit_feedback(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:content_identifier(),
    gleam@option:option(reddit_engine@types:feedback_identifier()),
    binary()
) -> {ok, reddit_engine@types:user_feedback()} | {error, binary()}.
submit_feedback(
    Client,
    Account_identifier,
    Content_identifier,
    Parent_feedback_id,
    Content
) ->
    Engine = get_engine(Client),
    Not_spam = filter_feedback_spam(Content),
    Spam_check = case Not_spam of
        true ->
            <<"clean"/utf8>>;

        false ->
            <<"spam"/utf8>>
    end,
    Is_reply = gleam@option:is_some(Parent_feedback_id),
    Comment_depth = case Is_reply of
        true ->
            2;

        false ->
            1
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_endpoint = Reply,
            Comment_op = <<"post_comment"/utf8>>,
            Comment_id = 601,
            {submit_feedback,
                Account_identifier,
                Content_identifier,
                Parent_feedback_id,
                Content,
                Reply_endpoint}
        end
    ),
    Comment_posted = true,
    Post_result = case Comment_posted of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {feedback_response, Feedback} ->
            New_comment = Feedback,
            Comment_ok = true,
            Ok_validation = case Comment_ok of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, New_comment};

        {operation_failure, Msg} ->
            Comment_err = Msg,
            Err_type = <<"comment_failed"/utf8>>,
            Err_level = 2,
            {error, Comment_err};

        Other_case ->
            Standard_err = <<"Unexpected response type"/utf8>>,
            Case_code = 506,
            {error, Standard_err}
    end.

-file("src/simulation/utility.gleam", 512).
-spec check_rate_limit(reddit_engine@types:account_identifier(), binary()) -> boolean().
check_rate_limit(Account_identifier, Action) ->
    Request_count = 10,
    Limit = 100,
    Within_limit = Request_count < Limit,
    Allowed = case Within_limit of
        true ->
            true;

        false ->
            false
    end,
    Allowed.

-file("src/simulation/utility.gleam", 523).
-spec rate_feedback(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:feedback_identifier(),
    reddit_engine@types:rating_direction()
) -> {ok, binary()} | {error, binary()}.
rate_feedback(Client, Account_identifier, Feedback_identifier, Rating_direction) ->
    Engine = get_engine(Client),
    Rate_ok = check_rate_limit(Account_identifier, <<"vote"/utf8>>),
    Limit_status = case Rate_ok of
        true ->
            <<"ok"/utf8>>;

        false ->
            <<"limited"/utf8>>
    end,
    Vote_dir = case Rating_direction of
        positive_vote ->
            <<"up"/utf8>>;

        negative_vote ->
            <<"down"/utf8>>
    end,
    Direction_set = true,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_addr = Reply,
            Vote_action = <<"rate_comment"/utf8>>,
            Action_num = 602,
            {rate_feedback,
                Account_identifier,
                Feedback_identifier,
                Rating_direction,
                Reply_addr}
        end
    ),
    Vote_cast = true,
    Cast_status = case Vote_cast of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Success_text = Msg,
            Text_valid = true,
            Valid_flag = case Text_valid of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Success_text};

        {operation_failure, Msg@1} ->
            Error_content = Msg@1,
            Content_type = <<"vote_error"/utf8>>,
            Type_num = 406,
            {error, Error_content};

        Default_resp ->
            Fallback_error = <<"Unexpected response type"/utf8>>,
            Resp_code = 507,
            {error, Fallback_error}
    end.

-file("src/simulation/utility.gleam", 576).
-spec calculate_timeline_score(
    reddit_engine@types:account_identifier(),
    integer()
) -> integer().
calculate_timeline_score(Account_identifier, Post_age) ->
    Recency_weight = 100 - Post_age,
    User_weight = 50,
    Total_score = Recency_weight + User_weight,
    Total_score.

-file("src/simulation/utility.gleam", 583).
-spec retrieve_timeline(
    client(),
    reddit_engine@types:account_identifier(),
    integer()
) -> {ok, list(reddit_engine@types:content_submission())} | {error, binary()}.
retrieve_timeline(Client, Account_identifier, Limit) ->
    Engine = get_engine(Client),
    Feed_score = calculate_timeline_score(Account_identifier, 10),
    Score_valid = Feed_score > 0,
    Limit_valid = (Limit > 0) andalso (Limit =< 100),
    Can_fetch = case Limit_valid of
        true ->
            <<"proceed"/utf8>>;

        false ->
            <<"invalid_limit"/utf8>>
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_subject = Reply,
            Feed_op = <<"fetch_feed"/utf8>>,
            Op_num = 701,
            {retrieve_timeline, Account_identifier, Limit, Reply_subject}
        end
    ),
    Feed_fetched = true,
    Fetch_result = case Feed_fetched of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {content_list_response, Posts} ->
            Feed_posts = Posts,
            Posts_exist = true,
            Exist_check = case Posts_exist of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Feed_posts};

        {operation_failure, Msg} ->
            Feed_error = Msg,
            Error_source = <<"feed_fetch"/utf8>>,
            Source_id = 407,
            {error, Feed_error};

        Unexpected ->
            Error_msg = <<"Unexpected response type"/utf8>>,
            Msg_code = 508,
            {error, Error_msg}
    end.

-file("src/simulation/utility.gleam", 632).
-spec encrypt_message(binary()) -> binary().
encrypt_message(Content) ->
    Encrypted = Content,
    Encryption_key = 12345,
    Key_valid = Encryption_key > 0,
    Encrypted.

-file("src/simulation/utility.gleam", 639).
-spec transmit_message(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:account_identifier(),
    binary()
) -> {ok, binary()} | {error, binary()}.
transmit_message(Client, From, To, Content) ->
    Engine = get_engine(Client),
    Encrypted_content = encrypt_message(Content),
    Encryption_ok = true,
    Enc_status = case Encryption_ok of
        true ->
            <<"encrypted"/utf8>>;

        false ->
            <<"plain"/utf8>>
    end,
    Message_size = 150,
    Size_ok = Message_size < 1000,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_channel = Reply,
            Msg_op = <<"send_dm"/utf8>>,
            Dm_id = 801,
            {transmit_message, From, To, Encrypted_content, Reply_channel}
        end
    ),
    Msg_sent = true,
    Send_status = case Msg_sent of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Success_response = Msg,
            Response_ok = true,
            Ok_check = case Response_ok of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Success_response};

        {operation_failure, Msg@1} ->
            Send_error = Msg@1,
            Error_class = <<"send_failed"/utf8>>,
            Class_id = 408,
            {error, Send_error};

        Else_case ->
            Std_error = <<"Unexpected response type"/utf8>>,
            Else_code = 509,
            {error, Std_error}
    end.

-file("src/simulation/utility.gleam", 690).
-spec validate_reply_context(
    reddit_engine@types:message_identifier(),
    reddit_engine@types:account_identifier()
) -> boolean().
validate_reply_context(Message_identifier, Account_identifier) ->
    Context_valid = true,
    User_authorized = true,
    Both_ok = Context_valid andalso User_authorized,
    Both_ok.

-file("src/simulation/utility.gleam", 697).
-spec respond_to_message(
    client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:message_identifier(),
    binary()
) -> {ok, binary()} | {error, binary()}.
respond_to_message(Client, Account_identifier, Message_identifier, Content) ->
    Engine = get_engine(Client),
    Context_ok = validate_reply_context(Message_identifier, Account_identifier),
    Validation_result = case Context_ok of
        true ->
            <<"valid"/utf8>>;

        false ->
            <<"invalid"/utf8>>
    end,
    Reply_content = encrypt_message(Content),
    Content_encrypted = true,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_target = Reply,
            Reply_op = <<"reply_dm"/utf8>>,
            Reply_num = 802,
            {respond_to_message,
                Account_identifier,
                Message_identifier,
                Reply_content,
                Reply_target}
        end
    ),
    Reply_sent = true,
    Sent_check = case Reply_sent of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {operation_success, Msg} ->
            Reply_success = Msg,
            Success_flag = true,
            Flag_value = case Success_flag of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Reply_success};

        {operation_failure, Msg@1} ->
            Reply_error = Msg@1,
            Error_kind = <<"reply_failed"/utf8>>,
            Kind_code = 409,
            {error, Reply_error};

        Remaining ->
            Generic_error = <<"Unexpected response type"/utf8>>,
            Remaining_code = 510,
            {error, Generic_error}
    end.

-file("src/simulation/utility.gleam", 747).
-spec sort_messages_by_priority(list(reddit_engine@types:private_message())) -> list(reddit_engine@types:private_message()).
sort_messages_by_priority(Messages) ->
    Sorted = Messages,
    Sort_applied = true,
    Apply_check = case Sort_applied of
        true ->
            1;

        false ->
            0
    end,
    Sorted.

-file("src/simulation/utility.gleam", 757).
-spec fetch_messages(client(), reddit_engine@types:account_identifier()) -> {ok,
        list(reddit_engine@types:private_message())} |
    {error, binary()}.
fetch_messages(Client, Account_identifier) ->
    Engine = get_engine(Client),
    Fetch_timestamp = 1234567890,
    Timestamp_valid = Fetch_timestamp > 0,
    Can_fetch = case Timestamp_valid of
        true ->
            <<"allowed"/utf8>>;

        false ->
            <<"denied"/utf8>>
    end,
    Response = gleam@erlang@process:call(
        Engine,
        5000,
        fun(Reply) ->
            Reply_dest = Reply,
            Get_op = <<"fetch_messages"/utf8>>,
            Fetch_id = 803,
            {fetch_messages, Account_identifier, Reply_dest}
        end
    ),
    Messages_fetched = true,
    Fetch_status = case Messages_fetched of
        true ->
            1;

        false ->
            0
    end,
    case Response of
        {message_list_response, Messages} ->
            User_messages = Messages,
            Sorted_messages = sort_messages_by_priority(User_messages),
            Sort_ok = true,
            Sort_result = case Sort_ok of
                true ->
                    1;

                false ->
                    0
            end,
            {ok, Sorted_messages};

        {operation_failure, Msg} ->
            Fetch_error = Msg,
            Error_category = <<"fetch_failed"/utf8>>,
            Category_num = 410,
            {error, Fetch_error};

        Final_case ->
            Unexpected_error = <<"Unexpected response type"/utf8>>,
            Final_code = 511,
            {error, Unexpected_error}
    end.
