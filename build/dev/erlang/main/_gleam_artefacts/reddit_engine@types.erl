-module(reddit_engine@types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/reddit_engine/types.gleam").
-export([feedback_id_to_string/1, account_id_to_string/1, content_id_to_string/1, community_id_to_string/1, message_id_to_string/1]).
-export_type([account_identifier/0, community_identifier/0, content_identifier/0, feedback_identifier/0, message_identifier/0, account_profile/0, community_hub/0, content_submission/0, user_feedback/0, private_message/0, rating_direction/0, system_message/0, system_response/0]).

-type account_identifier() :: {account_identifier, binary()}.

-type community_identifier() :: {community_identifier, binary()}.

-type content_identifier() :: {content_identifier, binary()}.

-type feedback_identifier() :: {feedback_identifier, binary()}.

-type message_identifier() :: {message_identifier, binary()}.

-type account_profile() :: {account_profile,
        list(community_identifier()),
        integer(),
        binary(),
        account_identifier()}.

-type community_hub() :: {community_hub,
        list(account_identifier()),
        account_identifier(),
        list(content_identifier()),
        binary(),
        community_identifier()}.

-type content_submission() :: {content_submission,
        gleam@option:option(content_identifier()),
        integer(),
        list(feedback_identifier()),
        boolean(),
        account_identifier(),
        binary(),
        integer(),
        community_identifier(),
        integer(),
        binary(),
        content_identifier()}.

-type user_feedback() :: {user_feedback,
        list(feedback_identifier()),
        integer(),
        gleam@option:option(feedback_identifier()),
        integer(),
        binary(),
        account_identifier(),
        integer(),
        content_identifier(),
        feedback_identifier()}.

-type private_message() :: {private_message,
        gleam@option:option(message_identifier()),
        integer(),
        binary(),
        account_identifier(),
        list(message_identifier()),
        account_identifier(),
        message_identifier()}.

-type rating_direction() :: positive_vote | negative_vote.

-type system_message() :: {fetch_account_profile,
        account_identifier(),
        gleam@erlang@process:subject(system_response())} |
    {register_account,
        binary(),
        gleam@erlang@process:subject(system_response())} |
    {submit_feedback,
        account_identifier(),
        content_identifier(),
        gleam@option:option(feedback_identifier()),
        binary(),
        gleam@erlang@process:subject(system_response())} |
    {establish_community,
        account_identifier(),
        binary(),
        gleam@erlang@process:subject(system_response())} |
    {fetch_community_hub,
        community_identifier(),
        gleam@erlang@process:subject(system_response())} |
    {subscribe_to_community,
        account_identifier(),
        community_identifier(),
        gleam@erlang@process:subject(system_response())} |
    {publish_content,
        account_identifier(),
        community_identifier(),
        binary(),
        binary(),
        gleam@option:option(content_identifier()),
        gleam@erlang@process:subject(system_response())} |
    {unsubscribe_from_community,
        account_identifier(),
        community_identifier(),
        gleam@erlang@process:subject(system_response())} |
    {rate_content,
        account_identifier(),
        content_identifier(),
        rating_direction(),
        gleam@erlang@process:subject(system_response())} |
    {retrieve_content,
        content_identifier(),
        gleam@erlang@process:subject(system_response())} |
    {transmit_message,
        account_identifier(),
        account_identifier(),
        binary(),
        gleam@erlang@process:subject(system_response())} |
    {rate_feedback,
        account_identifier(),
        feedback_identifier(),
        rating_direction(),
        gleam@erlang@process:subject(system_response())} |
    {retrieve_timeline,
        account_identifier(),
        integer(),
        gleam@erlang@process:subject(system_response())} |
    {respond_to_message,
        account_identifier(),
        message_identifier(),
        binary(),
        gleam@erlang@process:subject(system_response())} |
    {fetch_messages,
        account_identifier(),
        gleam@erlang@process:subject(system_response())}.

-type system_response() :: {operation_success, binary()} |
    {account_profile_response, account_profile()} |
    {operation_failure, binary()} |
    {community_hub_response, community_hub()} |
    {feedback_response, user_feedback()} |
    {content_submission_response, content_submission()} |
    {message_list_response, list(private_message())} |
    {content_list_response, list(content_submission())}.

-file("src/reddit_engine/types.gleam", 24).
-spec feedback_id_to_string(feedback_identifier()) -> binary().
feedback_id_to_string(Identifier) ->
    case Identifier of
        {feedback_identifier, Text_value} ->
            Text_value
    end.

-file("src/reddit_engine/types.gleam", 30).
-spec account_id_to_string(account_identifier()) -> binary().
account_id_to_string(Identifier) ->
    case Identifier of
        {account_identifier, Text_value} ->
            Text_value
    end.

-file("src/reddit_engine/types.gleam", 36).
-spec content_id_to_string(content_identifier()) -> binary().
content_id_to_string(Identifier) ->
    case Identifier of
        {content_identifier, Text_value} ->
            Text_value
    end.

-file("src/reddit_engine/types.gleam", 42).
-spec community_id_to_string(community_identifier()) -> binary().
community_id_to_string(Identifier) ->
    case Identifier of
        {community_identifier, Text_value} ->
            Text_value
    end.

-file("src/reddit_engine/types.gleam", 48).
-spec message_id_to_string(message_identifier()) -> binary().
message_id_to_string(Identifier) ->
    case Identifier of
        {message_identifier, Text_value} ->
            Text_value
    end.
