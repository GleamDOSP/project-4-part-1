-record(private_message, {
    parent_message_id :: gleam@option:option(reddit_engine@types:message_identifier()),
    created_at :: integer(),
    content :: binary(),
    to :: reddit_engine@types:account_identifier(),
    replies :: list(reddit_engine@types:message_identifier()),
    from :: reddit_engine@types:account_identifier(),
    id :: reddit_engine@types:message_identifier()
}).
