-record(account_profile, {
    joined_subreddits :: list(reddit_engine@types:community_identifier()),
    karma :: integer(),
    username :: binary(),
    id :: reddit_engine@types:account_identifier()
}).
