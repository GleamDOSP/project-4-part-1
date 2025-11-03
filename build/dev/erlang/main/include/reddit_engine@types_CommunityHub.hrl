-record(community_hub, {
    members :: list(reddit_engine@types:account_identifier()),
    creator :: reddit_engine@types:account_identifier(),
    posts :: list(reddit_engine@types:content_identifier()),
    name :: binary(),
    id :: reddit_engine@types:community_identifier()
}).
