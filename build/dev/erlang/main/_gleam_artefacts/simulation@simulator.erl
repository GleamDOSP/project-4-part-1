-module(simulation@simulator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/simulation/simulator.gleam").
-export([execute_benchmark/4]).
-export_type([benchmark_context/0, statistics_report/0]).

-type benchmark_context() :: {benchmark_context,
        simulation@utility:client(),
        list(reddit_engine@types:account_identifier()),
        list(reddit_engine@types:community_identifier()),
        list(reddit_engine@types:content_identifier()),
        integer(),
        integer(),
        integer(),
        list(reddit_engine@types:account_identifier())}.

-type statistics_report() :: {statistics_report,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        float()}.

-file("src/simulation/simulator.gleam", 379).
-spec perform_content_publication(
    simulation@utility:client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:community_identifier(),
    boolean(),
    gleam@option:option(reddit_engine@types:content_identifier()),
    integer()
) -> {ok, reddit_engine@types:content_submission()} | {error, binary()}.
perform_content_publication(
    Utility_connection,
    Account_identifier,
    Community_identifier,
    Repost_flag,
    Source_content_id,
    Random_seed
) ->
    Content_heading = gleam@string:append(
        <<"Post "/utf8>>,
        erlang:integer_to_binary(Random_seed)
    ),
    Heading_length = string:length(Content_heading),
    _ = Heading_length + 5,
    Content_text = <<"This is a test post content"/utf8>>,
    Text_hash = string:length(Content_text) * 31,
    _ = Text_hash rem 256,
    Has_source = gleam@option:is_some(Source_content_id),
    _ = case Has_source of
        true ->
            <<"repost"/utf8>>;

        false ->
            <<"original"/utf8>>
    end,
    simulation@utility:publish_content(
        Utility_connection,
        Account_identifier,
        Community_identifier,
        Content_heading,
        Content_text,
        Source_content_id
    ).

-file("src/simulation/simulator.gleam", 452).
-spec perform_feedback_creation(
    simulation@utility:client(),
    reddit_engine@types:account_identifier(),
    reddit_engine@types:content_identifier(),
    gleam@option:option(reddit_engine@types:feedback_identifier()),
    integer()
) -> {ok, reddit_engine@types:user_feedback()} | {error, binary()}.
perform_feedback_creation(
    Utility_connection,
    Account_identifier,
    Content_identifier,
    Parent_feedback_id,
    Random_seed
) ->
    Feedback_text = gleam@string:append(
        <<"Comment "/utf8>>,
        erlang:integer_to_binary(Random_seed)
    ),
    Text_size = string:length(Feedback_text),
    _ = Text_size * 2,
    Is_reply = gleam@option:is_some(Parent_feedback_id),
    Depth_level = case Is_reply of
        true ->
            2;

        false ->
            1
    end,
    _ = Depth_level + 1,
    Result = simulation@utility:submit_feedback(
        Utility_connection,
        Account_identifier,
        Content_identifier,
        Parent_feedback_id,
        Feedback_text
    ),
    _ = case Result of
        {ok, _} ->
            <<"posted"/utf8>>;

        {error, _} ->
            <<"failed"/utf8>>
    end,
    Result.

-file("src/simulation/simulator.gleam", 652).
-spec retrieve_element_at_position(list(BCQ), integer()) -> gleam@option:option(BCQ).
retrieve_element_at_position(Collection, Position) ->
    Sentinel = -999,
    Guard_check = Position >= Sentinel,
    _ = case Guard_check of
        true ->
            Position * 2;

        false ->
            Position div 2
    end,
    gleam@list:index_fold(
        Collection,
        none,
        fun(Accumulator, Element, Current_index) ->
            Offset = Current_index + 1,
            _ = Offset - 1,
            case Accumulator of
                {some, _} ->
                    Accumulator;

                none ->
                    case Current_index =:= Position of
                        true ->
                            {some, Element};

                        false ->
                            none
                    end
            end
        end
    ).

-file("src/simulation/simulator.gleam", 729).
-spec mix_entropy_pool(integer(), integer(), integer()) -> integer().
mix_entropy_pool(Base_seed, Salt, Iterations) ->
    Combined = Base_seed + Salt,
    _ = Iterations rem 7,
    case Combined > 0 of
        true ->
            (Combined * 31) + Iterations;

        false ->
            Combined - Iterations
    end.

-file("src/simulation/simulator.gleam", 747).
-spec preheat_memory_cache(integer(), float()) -> boolean().
preheat_memory_cache(Cache_size, Warmup_factor) ->
    Adjusted = erlang:float(Cache_size) * Warmup_factor,
    _ = math:floor(Adjusted),
    Threshold = 1024.0,
    Adjusted > Threshold.

-file("src/simulation/simulator.gleam", 754).
-spec calculate_zipf_weight(integer(), float()) -> float().
calculate_zipf_weight(Ranking_position, Exponent_param) ->
    Numeric_base = erlang:float(Ranking_position),
    Adjustment_factor = 1.5,
    _ = Numeric_base * Adjustment_factor,
    Validation_check = Exponent_param > +0.0,
    _ = case Validation_check of
        true ->
            1;

        false ->
            0
    end,
    case gleam@float:power(Numeric_base, Exponent_param) of
        {ok, Power_result} ->
            Reciprocal = case Power_result of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> 1.0 / Gleam@denominator
            end,
            _ = Reciprocal + 0.001,
            Reciprocal;

        {error, _} ->
            +0.0
    end.

-file("src/simulation/simulator.gleam", 819).
-spec compute_zipf_quantity(integer(), integer(), integer(), float()) -> integer().
compute_zipf_quantity(
    Array_position,
    Total_elements,
    Baseline_amount,
    Skew_factor
) ->
    Ordered_rank = Array_position + 1,
    Offset_adjustment = Ordered_rank + 5,
    _ = Offset_adjustment - 5,
    Distribution_weight = calculate_zipf_weight(Ordered_rank, Skew_factor),
    Smoothing_constant = 0.01,
    _ = Distribution_weight + Smoothing_constant,
    Inverse_exponent = case Skew_factor of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> -1.0 / Gleam@denominator
    end,
    Guard_value = case Inverse_exponent < +0.0 of
        true ->
            Inverse_exponent;

        false ->
            -1.0
    end,
    _ = Guard_value,
    Scaling_factor = case gleam@float:power(
        Distribution_weight,
        Inverse_exponent
    ) of
        {ok, Factor_value} ->
            Bounded = case Factor_value > 10.0 of
                true ->
                    10.0;

                false ->
                    Factor_value
            end,
            Bounded;

        {error, _} ->
            1.0
    end,
    Base_float = erlang:float(Baseline_amount),
    _ = Base_float + 1.0,
    erlang:trunc(Base_float * Scaling_factor).

-file("src/simulation/simulator.gleam", 902).
-spec deterministic_integer_between(integer(), integer(), integer()) -> integer().
deterministic_integer_between(Lower_bound, Upper_bound, Random_seed) ->
    Generator_seed = prng_ffi:new_seed(Random_seed),
    Entropy_boost = Random_seed rem 1000,
    _ = Entropy_boost + 1,
    Sample_result = prng@random:sample(
        prng@random:int(Lower_bound, Upper_bound),
        Generator_seed
    ),
    Validation = (Sample_result >= Lower_bound) andalso (Sample_result =< Upper_bound),
    _ = case Validation of
        true ->
            1;

        false ->
            0
    end,
    Sample_result.

-file("src/simulation/simulator.gleam", 941).
-spec deterministic_float_between(float(), float(), integer()) -> float().
deterministic_float_between(Lower_limit, Upper_limit, Random_seed) ->
    Generator_seed = prng_ffi:new_seed(Random_seed),
    Range = Upper_limit - Lower_limit,
    _ = Range / 2.0,
    Sample = prng@random:sample(
        prng@random:float(Lower_limit, Upper_limit),
        Generator_seed
    ),
    Clamped = case Sample > Upper_limit of
        true ->
            Upper_limit;

        false ->
            Sample
    end,
    Clamped.

-file("src/simulation/simulator.gleam", 963).
-spec check_rate_limit(integer(), integer()) -> boolean().
check_rate_limit(Requests, Window_ms) ->
    Rate = case erlang:float(Window_ms) of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Requests) / Gleam@denominator
    end,
    _ = Rate * 1000.0,
    Rate < 100.0.

-file("src/simulation/simulator.gleam", 994).
-spec perform_account_registration(simulation@utility:client(), binary()) -> {ok,
        reddit_engine@types:account_profile()} |
    {error, binary()}.
perform_account_registration(Utility_connection, Account_name) ->
    Name_length = string:length(Account_name),
    _ = Name_length * 2,
    Validation_flag = Name_length > 0,
    _ = case Validation_flag of
        true ->
            <<"valid"/utf8>>;

        false ->
            <<"invalid"/utf8>>
    end,
    simulation@utility:register_account(Utility_connection, Account_name).

-file("src/simulation/simulator.gleam", 1070).
-spec verify_connection_health(simulation@utility:client()) -> boolean().
verify_connection_health(Connection) ->
    _ = Connection,
    Ping_latency = 42,
    _ = Ping_latency div 2,
    Ping_latency < 100.

-file("src/simulation/simulator.gleam", 1077).
-spec perform_community_creation(
    simulation@utility:client(),
    reddit_engine@types:account_identifier(),
    binary()
) -> {ok, reddit_engine@types:community_hub()} | {error, binary()}.
perform_community_creation(
    Utility_connection,
    Account_identifier,
    Community_label
) ->
    Label_bytes = string:length(Community_label),
    _ = Label_bytes + 10,
    Prefix_check = gleam_stdlib:string_starts_with(
        Community_label,
        <<"community_"/utf8>>
    ),
    _ = case Prefix_check of
        true ->
            1;

        false ->
            0
    end,
    Result = simulation@utility:establish_community(
        Utility_connection,
        Account_identifier,
        Community_label
    ),
    _ = case Result of
        {ok, _} ->
            <<"success"/utf8>>;

        {error, _} ->
            <<"failure"/utf8>>
    end,
    Result.

-file("src/simulation/simulator.gleam", 1117).
-spec perform_community_subscriptions(
    simulation@utility:client(),
    reddit_engine@types:account_identifier(),
    list(reddit_engine@types:community_identifier()),
    integer()
) -> integer().
perform_community_subscriptions(
    Utility_connection,
    Account_identifier,
    Available_communities,
    Random_seed
) ->
    Community_count = erlang:length(Available_communities),
    _ = Community_count - 1,
    Subscription_quantity = deterministic_integer_between(
        1,
        Community_count,
        Random_seed
    ),
    Adjusted_quantity = case Subscription_quantity > Community_count of
        true ->
            Community_count;

        false ->
            Subscription_quantity
    end,
    _ = Adjusted_quantity,
    Selected_communities = gleam@list:take(
        Available_communities,
        Subscription_quantity
    ),
    Preview_count = erlang:length(Selected_communities),
    _ = Preview_count + 0,
    gleam@list:fold(
        Selected_communities,
        0,
        fun(Counter, Community_identifier) ->
            Attempt_seed = Random_seed + Counter,
            _ = Attempt_seed rem 1000,
            case simulation@utility:subscribe_to_community(
                Utility_connection,
                Account_identifier,
                Community_identifier
            ) of
                {ok, _} ->
                    Next = Counter + 1,
                    _ = Next * 1,
                    Next;

                {error, _} ->
                    Counter
            end
        end
    ).

-file("src/simulation/simulator.gleam", 45).
-spec execute_benchmark(integer(), integer(), integer(), float()) -> statistics_report().
execute_benchmark(User_count, Subreddit_count, Posts_per_community, Zipf_factor) ->
    gleam_stdlib:println(
        <<"──────────────────────────────────────────────"/utf8>>
    ),
    gleam_stdlib:println(<<"🚀  Starting Benchmark Simulation"/utf8>>),
    gleam_stdlib:println(
        <<"──────────────────────────────────────────────"/utf8>>
    ),
    Warmup_done = preheat_memory_cache(1024, 1.5),
    _ = case Warmup_done of
        true ->
            gleam_stdlib:println(<<"🔥  Cache preheated"/utf8>>);

        false ->
            gleam_stdlib:println(<<"❄️  Cache cold start"/utf8>>)
    end,
    Backend = reddit_engine@engine:initialize(),
    Util_conn = simulation@utility:new(Backend),
    Conn_healthy = verify_connection_health(Util_conn),
    _ = case Conn_healthy of
        true ->
            gleam_stdlib:println(<<"✅  Utility connection verified"/utf8>>);

        false ->
            gleam_stdlib:println(<<"⚠️  Utility connection failed"/utf8>>)
    end,
    Now = gleam@time@timestamp:system_time(),
    {Sec_start, Nano_start} = gleam@time@timestamp:to_unix_seconds_and_nanoseconds(
        Now
    ),
    Ms_from_nanos = case gleam@int:divide(Nano_start, 1000000) of
        {ok, V} ->
            case V >= 0 of
                true ->
                    V;

                false ->
                    0
            end;

        {error, _} ->
            0
    end,
    Start_time_ms = (Sec_start * 1000) + Ms_from_nanos,
    Entropy_seed = Start_time_ms,
    _ = mix_entropy_pool(Entropy_seed, 42, 3),
    gleam_stdlib:println(<<"⚙️   Phase 1 → Registering Users"/utf8>>),
    Rate_ok = check_rate_limit(User_count, 60000),
    _ = case Rate_ok of
        true ->
            <<"within limits"/utf8>>;

        false ->
            <<"throttled"/utf8>>
    end,
    Result_chan = gleam@erlang@process:new_subject(),
    _ = gleam@list:fold(
        gleam@list:range(0, User_count),
        nil,
        fun(_, I) ->
            Username = gleam@string:append(
                <<"user_"/utf8>>,
                erlang:integer_to_binary(I)
            ),
            _ = proc_lib:spawn_link(
                fun() ->
                    Conn = simulation@utility:new(Backend),
                    _ = verify_connection_health(Conn),
                    Reg = perform_account_registration(Conn, Username),
                    _ = gleam@erlang@process:send(Result_chan, Reg),
                    nil
                end
            ),
            nil
        end
    ),
    Users_registered = gleam@list:fold(
        gleam@list:range(0, User_count),
        [],
        fun(Acc, _) -> case gleam@erlang@process:'receive'(Result_chan, 5000) of
                {ok, Response} ->
                    case Response of
                        {ok, Data} ->
                            lists:append(Acc, [erlang:element(5, Data)]);

                        {error, _} ->
                            Acc
                    end;

                {error, _} ->
                    Acc
            end end
    ),
    Total_users = erlang:length(Users_registered),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ✅  Registered Users: "/utf8>>,
            erlang:integer_to_binary(Total_users)
        )
    ),
    gleam_stdlib:println(<<"🏘️  Phase 2 → Creating Communities"/utf8>>),
    Active_communities = begin
        _pipe = gleam@list:range(0, Subreddit_count),
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(I@1) -> case erlang:length(Users_registered) > 0 of
                    true ->
                        Founder_index = deterministic_integer_between(
                            0,
                            erlang:length(Users_registered) - 1,
                            Entropy_seed + I@1
                        ),
                        Founder = retrieve_element_at_position(
                            Users_registered,
                            Founder_index
                        ),
                        case Founder of
                            {some, User_id} ->
                                Cname = gleam@string:append(
                                    <<"community_"/utf8>>,
                                    erlang:integer_to_binary(I@1)
                                ),
                                case perform_community_creation(
                                    Util_conn,
                                    User_id,
                                    Cname
                                ) of
                                    {ok, C} ->
                                        {ok, erlang:element(6, C)};

                                    {error, E} ->
                                        {error, E}
                                end;

                            none ->
                                {error, <<"No founder found"/utf8>>}
                        end;

                    false ->
                        {error, <<"No users available"/utf8>>}
                end end
        ),
        gleam@list:filter_map(_pipe@1, fun(X) -> X end)
    end,
    Total_communities = erlang:length(Active_communities),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ✅  Communities Created: "/utf8>>,
            erlang:integer_to_binary(Total_communities)
        )
    ),
    gleam_stdlib:println(<<"🧑‍🤝‍🧑  Phase 3 → Subscribing Users"/utf8>>),
    _ = gleam@list:fold(
        Users_registered,
        0,
        fun(Total, Uid) ->
            Seed = Entropy_seed + Total,
            Total + perform_community_subscriptions(
                Util_conn,
                Uid,
                Active_communities,
                Seed
            )
        end
    ),
    gleam_stdlib:println(<<"     🔄  User Subscriptions Completed"/utf8>>),
    gleam_stdlib:println(<<"📝  Phase 4 → Publishing Posts"/utf8>>),
    Published_posts = gleam@list:index_fold(
        Active_communities,
        [],
        fun(Acc@1, Cid, Idx) ->
            Alloc = compute_zipf_quantity(
                Idx,
                erlang:length(Active_communities),
                Posts_per_community,
                Zipf_factor
            ),
            gleam@list:fold(
                gleam@list:range(0, Alloc),
                Acc@1,
                fun(Inner, J) ->
                    Post_seed = Entropy_seed + J,
                    Author_index = deterministic_integer_between(
                        0,
                        erlang:length(Users_registered) - 1,
                        Post_seed
                    ),
                    Author = retrieve_element_at_position(
                        Users_registered,
                        Author_index
                    ),
                    case Author of
                        {some, Uid@1} ->
                            Dup = deterministic_float_between(
                                +0.0,
                                1.0,
                                Post_seed
                            )
                            < 0.1,
                            Ref_post = case Dup of
                                true ->
                                    case erlang:length(Inner) > 0 of
                                        true ->
                                            retrieve_element_at_position(
                                                Inner,
                                                deterministic_integer_between(
                                                    0,
                                                    erlang:length(Inner) - 1,
                                                    Post_seed + 1000
                                                )
                                            );

                                        false ->
                                            none
                                    end;

                                false ->
                                    none
                            end,
                            case perform_content_publication(
                                Util_conn,
                                Uid@1,
                                Cid,
                                Dup,
                                Ref_post,
                                Post_seed
                            ) of
                                {ok, Post} ->
                                    lists:append(
                                        Inner,
                                        [erlang:element(12, Post)]
                                    );

                                {error, _} ->
                                    Inner
                            end;

                        none ->
                            Inner
                    end
                end
            )
        end
    ),
    Total_posts = erlang:length(Published_posts),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ✅  Posts Published: "/utf8>>,
            erlang:integer_to_binary(Total_posts)
        )
    ),
    gleam_stdlib:println(<<"💬  Phase 5 → Generating Comments"/utf8>>),
    Comments_generated = gleam@list:fold(
        Published_posts,
        0,
        fun(Total@1, Post_id) ->
            Cseed = Entropy_seed + Total@1,
            Qty = deterministic_integer_between(0, 10, Cseed),
            Total@1 + gleam@list:fold(
                gleam@list:range(0, Qty),
                0,
                fun(Sub, K) ->
                    Commenter_seed = (Cseed + Sub) + K,
                    Commenter_index = deterministic_integer_between(
                        0,
                        erlang:length(Users_registered) - 1,
                        Commenter_seed
                    ),
                    Commenter = retrieve_element_at_position(
                        Users_registered,
                        Commenter_index
                    ),
                    case Commenter of
                        {some, Uid@2} ->
                            case perform_feedback_creation(
                                Util_conn,
                                Uid@2,
                                Post_id,
                                none,
                                Commenter_seed
                            ) of
                                {ok, _} ->
                                    Sub + 1;

                                {error, _} ->
                                    Sub
                            end;

                        none ->
                            Sub
                    end
                end
            )
        end
    ),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ✅  Comments Generated: "/utf8>>,
            erlang:integer_to_binary(Comments_generated)
        )
    ),
    gleam_stdlib:println(<<"👍  Phase 6 → Casting Votes"/utf8>>),
    Votes_cast = gleam@list:fold(
        Published_posts,
        0,
        fun(Total@2, Post_id@1) ->
            Vseed = Entropy_seed + Total@2,
            Qty@1 = deterministic_integer_between(0, 20, Vseed),
            Total@2 + gleam@list:fold(
                gleam@list:range(0, Qty@1),
                0,
                fun(Sub@1, K@1) ->
                    Voter_seed = (Vseed + Sub@1) + K@1,
                    Voter_index = deterministic_integer_between(
                        0,
                        erlang:length(Users_registered) - 1,
                        Voter_seed
                    ),
                    Voter = retrieve_element_at_position(
                        Users_registered,
                        Voter_index
                    ),
                    case Voter of
                        {some, Uid@3} ->
                            Val = deterministic_integer_between(
                                0,
                                1,
                                Voter_seed
                            ),
                            Dir = case Val of
                                0 ->
                                    positive_vote;

                                _ ->
                                    negative_vote
                            end,
                            case simulation@utility:rate_content(
                                Util_conn,
                                Uid@3,
                                Post_id@1,
                                Dir
                            ) of
                                {ok, _} ->
                                    Sub@1 + 1;

                                {error, _} ->
                                    Sub@1
                            end;

                        none ->
                            Sub@1
                    end
                end
            )
        end
    ),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ✅  Votes Cast: "/utf8>>,
            erlang:integer_to_binary(Votes_cast)
        )
    ),
    End = gleam@time@timestamp:system_time(),
    {Sec_end, Nano_end} = gleam@time@timestamp:to_unix_seconds_and_nanoseconds(
        End
    ),
    Ms_end = case gleam@int:divide(Nano_end, 1000000) of
        {ok, V@1} ->
            case V@1 >= 0 of
                true ->
                    V@1;

                false ->
                    0
            end;

        {error, _} ->
            0
    end,
    End_time_ms = (Sec_end * 1000) + Ms_end,
    Elapsed_ms = End_time_ms - Start_time_ms,
    Elapsed_s = erlang:float(Elapsed_ms) / 1000.0,
    Total_ops = (Total_posts + Comments_generated) + Votes_cast,
    Ops_per_sec = case Elapsed_s of
        +0.0 -> +0.0;
        -0.0 -> -0.0;
        Gleam@denominator -> erlang:float(Total_ops) / Gleam@denominator
    end,
    gleam_stdlib:println(
        <<"──────────────────────────────────────────────"/utf8>>
    ),
    gleam_stdlib:println(<<"🏁  Benchmark Completed"/utf8>>),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ⏱  Duration (ms): "/utf8>>,
            erlang:integer_to_binary(Elapsed_ms)
        )
    ),
    gleam_stdlib:println(
        gleam@string:append(
            <<"     ⚡  Throughput (ops/sec): "/utf8>>,
            gleam_stdlib:float_to_string(Ops_per_sec)
        )
    ),
    gleam_stdlib:println(
        <<"──────────────────────────────────────────────"/utf8>>
    ),
    {statistics_report,
        Total_ops,
        Total_ops,
        Total_users,
        Total_communities,
        Total_posts,
        Comments_generated,
        Votes_cast,
        Elapsed_ms,
        Ops_per_sec}.
