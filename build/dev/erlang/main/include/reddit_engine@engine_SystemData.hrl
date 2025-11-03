-record(system_data, {
    registered_accounts :: gleam@dict:dict(binary(), reddit_engine@types:account_profile()),
    communities :: gleam@dict:dict(binary(), reddit_engine@types:community_hub()),
    submissions :: gleam@dict:dict(binary(), reddit_engine@types:content_submission()),
    feedback_items :: gleam@dict:dict(binary(), reddit_engine@types:user_feedback()),
    private_msgs :: gleam@dict:dict(binary(), reddit_engine@types:private_message()),
    account_lookup :: gleam@dict:dict(binary(), reddit_engine@types:account_identifier()),
    account_counter :: integer(),
    community_counter :: integer(),
    submission_counter :: integer(),
    feedback_counter :: integer(),
    private_msg_counter :: integer()
}).
