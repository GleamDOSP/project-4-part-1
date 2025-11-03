-record(content_submission, {
    original_post_id :: gleam@option:option(reddit_engine@types:content_identifier()),
    downvotes :: integer(),
    comments :: list(reddit_engine@types:feedback_identifier()),
    is_repost :: boolean(),
    author :: reddit_engine@types:account_identifier(),
    title :: binary(),
    created_at :: integer(),
    subreddit_id :: reddit_engine@types:community_identifier(),
    upvotes :: integer(),
    content :: binary(),
    id :: reddit_engine@types:content_identifier()
}).
