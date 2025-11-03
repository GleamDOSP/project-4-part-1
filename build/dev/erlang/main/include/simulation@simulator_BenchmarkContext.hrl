-record(benchmark_context, {
    backend_connection :: simulation@utility:client(),
    registered_accounts :: list(reddit_engine@types:account_identifier()),
    active_communities :: list(reddit_engine@types:community_identifier()),
    published_content :: list(reddit_engine@types:content_identifier()),
    transmission_count :: integer(),
    completed_tasks :: integer(),
    initialization_timestamp :: integer(),
    online_accounts :: list(reddit_engine@types:account_identifier())
}).
