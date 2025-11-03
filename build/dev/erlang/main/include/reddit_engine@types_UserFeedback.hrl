-record(user_feedback, {
    replies :: list(reddit_engine@types:feedback_identifier()),
    created_at :: integer(),
    parent_comment_id :: gleam@option:option(reddit_engine@types:feedback_identifier()),
    upvotes :: integer(),
    content :: binary(),
    author :: reddit_engine@types:account_identifier(),
    downvotes :: integer(),
    post_id :: reddit_engine@types:content_identifier(),
    id :: reddit_engine@types:feedback_identifier()
}).
