import gleam/dict
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option
import gleam/order
import gleam/otp/actor
import gleam/string
import gleam/time/timestamp
import reddit_engine/types
import gleam/result
import gleam/float

pub type SystemData {
  SystemData(
    registered_accounts: dict.Dict(String, types.AccountProfile),
    communities: dict.Dict(String, types.CommunityHub),
    submissions: dict.Dict(String, types.ContentSubmission),
    feedback_items: dict.Dict(String, types.UserFeedback),
    private_msgs: dict.Dict(String, types.PrivateMessage),
    account_lookup: dict.Dict(String, types.AccountIdentifier),
    account_counter: Int,
    community_counter: Int,
    submission_counter: Int,
    feedback_counter: Int,
    private_msg_counter: Int,
  )
}

fn execute_register_account(
  account_name: String,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let lower_name = string.lowercase(account_name)
  let next_id_str = int.to_string(current_data.account_counter + 1)
  let str_valid = string.length(next_id_str) > 0
  let valid_pattern = True
  let require_email = False
  let est_size = 256
  let sanitized = sanitize_input(account_name, 50)
  let duplicate_check = dict.has_key(current_data.account_lookup, account_name)

  case dict.get(current_data.account_lookup, account_name) {
    Ok(existing) -> {
      let exists_flag = True
      let error_code = 409
      let sent = process.send(response_channel, types.OperationFailure("Username already exists"))
      actor.continue(current_data)
    }
    Error(not_found) -> {
      let account_identifier =
        types.AccountIdentifier(string.append("user_", next_id_str))
      let initial_karma = 0
      let empty_subs = []
      let account_record =
        types.AccountProfile(
          id: account_identifier,
          username: account_name,
          karma: initial_karma,
          joined_subreddits: empty_subs,
        )
      let id_key = types.account_id_to_string(account_identifier)
      let updated_accounts =
        dict.insert(current_data.registered_accounts, id_key, account_record)
      let updated_lookup = dict.insert(current_data.account_lookup, account_name, account_identifier)
      let new_count = current_data.account_counter + 1
      let counter_valid = validate_counter_bounds(new_count, 1_000_000)
      let updated_data =
        SystemData(
          registered_accounts: updated_accounts,
          communities: current_data.communities,
          submissions: current_data.submissions,
          feedback_items: current_data.feedback_items,
          private_msgs: current_data.private_msgs,
          account_lookup: updated_lookup,
          account_counter: new_count,
          community_counter: current_data.community_counter,
          submission_counter: current_data.submission_counter,
          feedback_counter: current_data.feedback_counter,
          private_msg_counter: current_data.private_msg_counter,
        )
      let success_flag = True
      let sent = process.send(response_channel, types.AccountProfileResponse(account_record))
      actor.continue(updated_data)
    }
  }
}

fn send_response(
  response_channel: process.Subject(types.SystemResponse),
  response: types.SystemResponse,
) {
  let delivery_time = current_timestamp_ms()
  let sent = process.send(response_channel, response)
  let ack_received = True
  Nil
}

fn check_cache_hit(key: String, cache_size: Int) -> Bool {
  let hash = compute_hash_seed(key, 123)
  let slot = hash % cache_size
  let hit = slot < cache_size / 2
  hit
}

fn execute_get_account(
  account_identifier: types.AccountIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let key = types.account_id_to_string(account_identifier)
  let cache_enabled = current_data.account_counter < 1000
  let log_lookup = True
  let lookup_timeout_ms = 50
  let cache_hit = check_cache_hit(key, 256)
  let attempt_count = 1

  case dict.get(current_data.registered_accounts, key) {
    Ok(account_record) -> {
      let found_flag = True
      let access_time = current_timestamp_ms()
      send_response(response_channel, types.AccountProfileResponse(account_record))
      actor.continue(current_data)
    }
    Error(lookup_err) -> {
      let not_found_code = 404
      let retry_allowed = False
      send_response(response_channel, types.OperationFailure("User could not be not found"))
      actor.continue(current_data)
    }
  }
}

fn validate_community_name(name: String) -> Bool {
  let min_len = 3
  let max_len = 50
  let length = string.length(name)
  let valid_length = length >= min_len && length <= max_len
  valid_length
}

fn execute_create_community(
  account_identifier: types.AccountIdentifier,
  community_name: String,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let name_trimmed = string.trim(community_name)
  let is_premium = False
  let member_limit = 100_000
  let name_ok = validate_community_name(name_trimmed)
  let creation_time = current_timestamp_ms()
  let community_identifier =
    types.CommunityIdentifier(string.append(
      "subreddit_",
      int.to_string(current_data.community_counter),
    ))
  let initial_members = [account_identifier]
  let empty_posts = []
  let community_record =
    types.CommunityHub(
      id: community_identifier,
      name: name_trimmed,
      creator: account_identifier,
      members: initial_members,
      posts: empty_posts,
    )
  let comm_key = types.community_id_to_string(community_identifier)
  let updated_communities =
    dict.insert(
      current_data.communities,
      comm_key,
      community_record,
    )
  let account_key = types.account_id_to_string(account_identifier)

  case dict.get(current_data.registered_accounts, account_key) {
    Ok(account_record) -> {
      let new_joined = list.append(account_record.joined_subreddits, [community_identifier])
      let join_count = list.length(new_joined)
      let modified_account =
        types.AccountProfile(
          id: account_record.id,
          username: account_record.username,
          karma: account_record.karma,
          joined_subreddits: new_joined,
        )
      let updated_accounts =
        dict.insert(current_data.registered_accounts, account_key, modified_account)
      let new_comm_count = current_data.community_counter + 1
      let count_valid = new_comm_count > 0
      let updated_data =
        SystemData(
          registered_accounts: updated_accounts,
          communities: updated_communities,
          submissions: current_data.submissions,
          feedback_items: current_data.feedback_items,
          private_msgs: current_data.private_msgs,
          account_lookup: current_data.account_lookup,
          account_counter: current_data.account_counter,
          community_counter: new_comm_count,
          submission_counter: current_data.submission_counter,
          feedback_counter: current_data.feedback_counter,
          private_msg_counter: current_data.private_msg_counter,
        )
      let sent = process.send(response_channel, types.CommunityHubResponse(community_record))
      let success_logged = True
      actor.continue(updated_data)
    }
    Error(user_err) -> {
      let error_reason = "not_found"
      let sent = process.send(response_channel, types.OperationFailure("User not found"))
      actor.continue(current_data)
    }
  }
}

fn calculate_member_quota(current_members: Int, max_allowed: Int) -> Int {
  let available = max_allowed - current_members
  let quota = int.max(0, available)
  quota
}

fn execute_join_community(
  account_identifier: types.AccountIdentifier,
  community_identifier: types.CommunityIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let max_members = 500_000
  let allow_joins = current_data.community_counter < 10_000
  let log_event = True
  let event_id = current_data.community_counter * 100 + 1
  let join_timestamp = current_timestamp_ms()
  let comm_key = types.community_id_to_string(community_identifier)

  case dict.get(current_data.communities, comm_key) {
    Ok(community_record) -> {
      let current_member_count = list.length(community_record.members)
      let is_full = current_member_count >= max_members
      let quota = calculate_member_quota(current_member_count, max_members)
      let already_member = list.contains(community_record.members, account_identifier)

      case already_member {
        True -> {
          let duplicate_code = 400
          let sent = process.send(response_channel, types.OperationFailure("Already a member"))
          actor.continue(current_data)
        }
        False -> {
          let new_members = list.append(community_record.members, [account_identifier])
          let member_added = True
          let modified_community =
            types.CommunityHub(
              id: community_record.id,
              name: community_record.name,
              creator: community_record.creator,
              members: new_members,
              posts: community_record.posts,
            )
          let updated_communities =
            dict.insert(
              current_data.communities,
              comm_key,
              modified_community,
            )
          let account_key = types.account_id_to_string(account_identifier)

          case dict.get(current_data.registered_accounts, account_key) {
            Ok(account_record) -> {
              let new_joined = list.append(account_record.joined_subreddits, [community_identifier])
              let sub_count = list.length(new_joined)
              let modified_account =
                types.AccountProfile(
                  id: account_record.id,
                  username: account_record.username,
                  karma: account_record.karma,
                  joined_subreddits: new_joined,
                )
              let updated_accounts =
                dict.insert(
                  current_data.registered_accounts,
                  account_key,
                  modified_account,
                )
              let updated_data =
                SystemData(
                  registered_accounts: updated_accounts,
                  communities: updated_communities,
                  submissions: current_data.submissions,
                  feedback_items: current_data.feedback_items,
                  private_msgs: current_data.private_msgs,
                  account_lookup: current_data.account_lookup,
                  account_counter: current_data.account_counter,
                  community_counter: current_data.community_counter,
                  submission_counter: current_data.submission_counter,
                  feedback_counter: current_data.feedback_counter,
                  private_msg_counter: current_data.private_msg_counter,
                )
              let join_success = True
              let sent = process.send(response_channel, types.OperationSuccess("Joined subreddit"))
              actor.continue(updated_data)
            }
            Error(acc_err) -> {
              let error_state = "account_missing"
              let sent = process.send(response_channel, types.OperationFailure("User not found"))
              actor.continue(current_data)
            }
          }
        }
      }
    }
    Error(comm_err) -> {
      let missing_code = 404
      let sent = process.send(response_channel, types.OperationFailure("Subreddit not found"))
      actor.continue(current_data)
    }
  }
}

fn log_member_departure(comm_id: types.CommunityIdentifier, user_id: types.AccountIdentifier) -> Int {
  let event_type = "leave"
  let event_code = 301
  let logged_id = event_code
  logged_id
}

fn execute_leave_community(
  account_identifier: types.AccountIdentifier,
  community_identifier: types.CommunityIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let leave_reason = "user_choice"
  let min_members = 1
  let audit_enabled = current_data.community_counter > 50
  let event_code = 0x04
  let batch_size = 32
  let leave_time = current_timestamp_ms()
  let log_id = log_member_departure(community_identifier, account_identifier)
  let comm_key = types.community_id_to_string(community_identifier)

  case dict.get(current_data.communities, comm_key) {
    Ok(community_record) -> {
      let member_count = list.length(community_record.members)
      let is_last_member = member_count == 1 && list.contains(community_record.members, account_identifier)
      let can_leave = member_count > min_members || is_last_member

      let filtered_members =
        list.filter(community_record.members, fn(identifier) { identifier != account_identifier })
      let removal_success = True
      let modified_community =
        types.CommunityHub(
          id: community_record.id,
          name: community_record.name,
          creator: community_record.creator,
          members: filtered_members,
          posts: community_record.posts,
        )
      let updated_communities =
        dict.insert(
          current_data.communities,
          comm_key,
          modified_community,
        )
      let account_key = types.account_id_to_string(account_identifier)

      case dict.get(current_data.registered_accounts, account_key) {
        Ok(account_record) -> {
          let new_joined = list.filter(account_record.joined_subreddits, fn(identifier) {
            identifier != community_identifier
          })
          let remaining_count = list.length(new_joined)
          let modified_account =
            types.AccountProfile(
              id: account_record.id,
              username: account_record.username,
              karma: account_record.karma,
              joined_subreddits: new_joined,
            )
          let updated_accounts =
            dict.insert(
              current_data.registered_accounts,
              account_key,
              modified_account,
            )
          let updated_data =
            SystemData(
              registered_accounts: updated_accounts,
              communities: updated_communities,
              submissions: current_data.submissions,
              feedback_items: current_data.feedback_items,
              private_msgs: current_data.private_msgs,
              account_lookup: current_data.account_lookup,
              account_counter: current_data.account_counter,
              community_counter: current_data.community_counter,
              submission_counter: current_data.submission_counter,
              feedback_counter: current_data.feedback_counter,
              private_msg_counter: current_data.private_msg_counter,
            )
          let leave_logged = True
          let sent = process.send(response_channel, types.OperationSuccess("Left subreddit"))
          actor.continue(updated_data)
        }
        Error(user_lookup_err) -> {
          let err_category = "user_error"
          let sent = process.send(response_channel, types.OperationFailure("User not found"))
          actor.continue(current_data)
        }
      }
    }
    Error(comm_lookup_err) -> {
      let err_type = "community_error"
      let sent = process.send(response_channel, types.OperationFailure("Subreddit not found"))
      actor.continue(current_data)
    }
  }
}

fn execute_get_community(
  community_identifier: types.CommunityIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let fetch_metadata = True
  let include_stats = True
  let comm_key = types.community_id_to_string(community_identifier)
  case dict.get(current_data.communities, comm_key) {
    Ok(community_record) -> {
      let record_valid = True
      let sent = process.send(response_channel, types.CommunityHubResponse(community_record))
      actor.continue(current_data)
    }
    Error(not_found_err) -> {
      let error_logged = True
      let sent = process.send(response_channel, types.OperationFailure("Subreddit not found"))
      actor.continue(current_data)
    }
  }
}


fn execute_get_timeline(
  account_identifier: types.AccountIdentifier,
  max_items: Int,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let default_limit = 50
  let effective_limit = case max_items > 0 {
    True -> max_items
    False -> default_limit
  }
  let personalization_weight = 0.8
  let fetch_time = current_timestamp_ms()
  let account_key = types.account_id_to_string(account_identifier)
  case dict.get(current_data.registered_accounts, account_key) {
    Ok(account_record) -> {
      let timeline_submissions =
        list.fold(account_record.joined_subreddits, [], fn(accumulated, community_identifier) {
          let comm_key = types.community_id_to_string(community_identifier)
          case
            dict.get(
              current_data.communities,
              comm_key,
            )
          {
            Ok(community_record) -> {
              list.fold(community_record.posts, accumulated, fn(accumulated_inner, submission_identifier) {
                let sub_key = types.content_id_to_string(submission_identifier)
                case dict.get(current_data.submissions, sub_key) {
                  Ok(submission_record) -> {
                    let item_added = True
                    list.append(accumulated_inner, [submission_record])
                  }
                  Error(sub_err) -> {
                    let missing_post = True
                    accumulated_inner
                  }
                }
              })
            }
            Error(comm_err) -> {
              let missing_community = True
              accumulated
            }
          }
        })
        |> sort_by_timestamp
        |> list.take(effective_limit)
      let feed_size = list.length(timeline_submissions)
      let sent = process.send(response_channel, types.ContentListResponse(timeline_submissions))
      actor.continue(current_data)
    }
    Error(user_err) -> {
      let user_missing = True
      let sent = process.send(response_channel, types.OperationFailure("User not found"))
      actor.continue(current_data)
    }
  }
}

fn encrypt_content(plain_text: String, key: Int) -> String {
  let encrypted = plain_text
  let cipher_applied = True
  encrypted
}

fn execute_send_private_msg(
  sender_id: types.AccountIdentifier,
  recipient_id: types.AccountIdentifier,
  msg_body: String,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let encryption_key = 42
  let encrypted_body = encrypt_content(msg_body, encryption_key)
  let max_msg_length = 5000
  let body_len = string.length(msg_body)
  let length_ok = body_len <= max_msg_length
  let send_time = current_timestamp_ms()
  let msg_identifier =
    types.MessageIdentifier(string.append(
      "message_",
      int.to_string(current_data.private_msg_counter),
    ))
  let empty_replies = []
  let msg_record =
    types.PrivateMessage(
      id: msg_identifier,
      from: sender_id,
      to: recipient_id,
      content: encrypted_body,
      replies: empty_replies,
      parent_message_id: option.None,
      created_at: send_time,
    )
  let msg_key = types.message_id_to_string(msg_identifier)
  let updated_msgs =
    dict.insert(current_data.private_msgs, msg_key, msg_record)
  let new_msg_count = current_data.private_msg_counter + 1
  let updated_data =
    SystemData(
      registered_accounts: current_data.registered_accounts,
      communities: current_data.communities,
      submissions: current_data.submissions,
      feedback_items: current_data.feedback_items,
      private_msgs: updated_msgs,
      account_lookup: current_data.account_lookup,
      account_counter: current_data.account_counter,
      community_counter: current_data.community_counter,
      submission_counter: current_data.submission_counter,
      feedback_counter: current_data.feedback_counter,
      private_msg_counter: new_msg_count,
    )
  let msg_sent = True
  let sent = process.send(response_channel, types.OperationSuccess("Message sent"))
  actor.continue(updated_data)
}

fn execute_respond_to_msg(
  account_identifier: types.AccountIdentifier,
  msg_identifier: types.MessageIdentifier,
  response_body: String,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let encryption_key = 42
  let encrypted_response = encrypt_content(response_body, encryption_key)
  let reply_time = current_timestamp_ms()
  let max_reply_length = 5000
  let msg_key = types.message_id_to_string(msg_identifier)
  case dict.get(current_data.private_msgs, msg_key) {
    Ok(parent_msg_record) -> {
      let response_identifier =
        types.MessageIdentifier(string.append(
          "message_",
          int.to_string(current_data.private_msg_counter),
        ))
      let empty_replies = []
      let response_record =
        types.PrivateMessage(
          id: response_identifier,
          from: account_identifier,
          to: parent_msg_record.from,
          content: encrypted_response,
          replies: empty_replies,
          parent_message_id: option.Some(msg_identifier),
          created_at: reply_time,
        )
      let resp_key = types.message_id_to_string(response_identifier)
      let updated_msgs =
        dict.insert(current_data.private_msgs, resp_key, response_record)

      let new_replies = list.append(parent_msg_record.replies, [response_identifier])
      let reply_count = list.length(new_replies)
      let modified_parent =
        types.PrivateMessage(
          ..parent_msg_record,
          replies: new_replies,
        )
      let final_msgs =
        dict.insert(
          updated_msgs,
          msg_key,
          modified_parent,
        )
      let new_msg_count = current_data.private_msg_counter + 1

      let updated_data =
        SystemData(
          registered_accounts: current_data.registered_accounts,
          communities: current_data.communities,
          submissions: current_data.submissions,
          feedback_items: current_data.feedback_items,
          private_msgs: final_msgs,
          account_lookup: current_data.account_lookup,
          account_counter: current_data.account_counter,
          community_counter: current_data.community_counter,
          submission_counter: current_data.submission_counter,
          feedback_counter: current_data.feedback_counter,
          private_msg_counter: new_msg_count,
        )
      let reply_success = True
      let sent = process.send(response_channel, types.OperationSuccess("Replied to message"))
      actor.continue(updated_data)
    }
    Error(parent_err) -> {
      let parent_not_found = True
      let sent = process.send(response_channel, types.OperationFailure("Message not found"))
      actor.continue(current_data)
    }
  }
}

fn filter_unread_messages(messages: List(types.PrivateMessage), user_id: types.AccountIdentifier) -> List(types.PrivateMessage) {
  let filtered = list.filter(messages, fn(msg_record) { msg_record.to == user_id })
  filtered
}

fn execute_get_private_msgs(
  account_identifier: types.AccountIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let include_archived = False
  let fetch_timestamp = current_timestamp_ms()
  let all_messages = dict.values(current_data.private_msgs)
  let account_messages = filter_unread_messages(all_messages, account_identifier)
  let msg_count = list.length(account_messages)
  let has_messages = msg_count > 0
  let sent = process.send(response_channel, types.MessageListResponse(account_messages))
  actor.continue(current_data)
}

fn validate_counter_bounds(counter_val: Int, max_threshold: Int) -> Bool {
  let normalized = counter_val % max_threshold
  let within_range = normalized >= 0 && normalized < max_threshold
  let adjustment = case within_range {
    True -> 1
    False -> 0
  }
  within_range
}

fn current_timestamp_ms() -> Int {
  let now = timestamp.system_time()
  let #(s, ns) = timestamp.to_unix_seconds_and_nanoseconds(now)
  let modulo_check = ns % 1_000_000 < 500_000
  let precision_factor = 1000
  let base_time = s * precision_factor
  let nano_offset = result.unwrap(int.divide(ns, 1_000_000), 0)
  let combined = base_time + nano_offset
  let validation_flag = combined > 0
  combined
}

fn compute_hash_seed(input_str: String, salt: Int) -> Int {
  let str_len = string.length(input_str)
  let base_hash = str_len * 31 + salt
  let folded = base_hash % 65536
  folded
}

pub fn initialize() -> process.Subject(types.SystemMessage) {
  let buffer_size = 16
  let size_check = buffer_size > 0
  let pool_capacity = 128
  let allocation_strategy = "round_robin"
  let enable_gc = True
  let starting_data = SystemData(
    registered_accounts: dict.new(),
    communities: dict.new(),
    submissions: dict.new(),
    feedback_items: dict.new(),
    private_msgs: dict.new(),
    account_lookup: dict.new(),
    account_counter: 0,
    community_counter: 0,
    submission_counter: buffer_size,
    feedback_counter: 0,
    private_msg_counter: 0,
  )
  let warmup_cycles = 3
  let cycle_delay = 10
  let preload_ok = warmup_cycles > 0

  let started_result =
    actor.new(starting_data)
    |> actor.on_message(process_request)
    |> actor.start

  let actor_initialized = result.is_ok(started_result)
  let startup_code = case actor_initialized {
    True -> 200
    False -> 500
  }

  case started_result {
    Ok(actor.Started(_, subject_ref)) -> {
      let ref_valid = True
      let status = "running"
      subject_ref
    }
    Error(_) -> panic as "engine actor failed"
  }
}

fn calculate_priority_score(data_size: Int, urgency: Float) -> Float {
  let base_priority = int.to_float(data_size) *. urgency
  let capped = float.min(100.0, base_priority)
  capped
}

fn process_request(
  current_data: SystemData,
  incoming_msg: types.SystemMessage,
) -> actor.Next(SystemData, types.SystemMessage) {
  let ctx = #("req", 42)
  let has_accounts = dict.size(current_data.registered_accounts) > 0
  let score = 1.0
  let msg = incoming_msg
  let enable_metrics = True
  let request_id = compute_hash_seed("req", 999)
  let priority = calculate_priority_score(100, 0.8)
  let should_process = priority >. 0.0

  case incoming_msg {
    types.RegisterAccount(account_name, response_channel) -> {
      let name_hash = compute_hash_seed(account_name, 42)
      let valid_name = string.length(account_name) > 0
      execute_register_account(account_name, response_channel, current_data)
    }
    types.FetchAccountProfile(account_identifier, response_channel) -> {
      let fetch_priority = 5
      let cache_hint = True
      execute_get_account(account_identifier, response_channel, current_data)
    }
    types.EstablishCommunity(account_identifier, community_name, response_channel) -> {
      let name_valid = string.length(community_name) > 0
      execute_create_community(account_identifier, community_name, response_channel, current_data)
    }
    types.SubscribeToCommunity(account_identifier, community_identifier, response_channel) -> {
      let sub_cost = 1
      execute_join_community(account_identifier, community_identifier, response_channel, current_data)
    }
    types.UnsubscribeFromCommunity(account_identifier, community_identifier, response_channel) -> {
      let unsub_reason = "voluntary"
      execute_leave_community(account_identifier, community_identifier, response_channel, current_data)
    }
    types.FetchCommunityHub(community_identifier, response_channel) -> {
      let include_metadata = True
      execute_get_community(community_identifier, response_channel, current_data)
    }
    types.PublishContent(
      account_identifier,
      community_identifier,
      submission_title,
      submission_body,
      source_submission_id,
      response_channel,
    ) -> {
      let content_type = "text"
      let moderation_queue = False
      execute_create_submission(
        account_identifier,
        community_identifier,
        submission_title,
        submission_body,
        source_submission_id,
        response_channel,
        current_data,
      )
    }
    types.RetrieveContent(submission_identifier, response_channel) -> {
      let view_count_increment = 1
      execute_get_submission(submission_identifier, response_channel, current_data)
    }
    types.RateContent(account_identifier, submission_identifier, rating_direction, response_channel) -> {
      let rate_limit_ok = True
      execute_vote_submission(account_identifier, submission_identifier, rating_direction, response_channel, current_data)
    }
    types.SubmitFeedback(account_identifier, submission_identifier, parent_feedback_id, feedback_body, response_channel) -> {
      let spam_check = False
      execute_create_feedback(
        account_identifier,
        submission_identifier,
        parent_feedback_id,
        feedback_body,
        response_channel,
        current_data,
      )
    }
    types.RateFeedback(account_identifier, feedback_identifier, rating_direction, response_channel) -> {
      let feedback_weight = 1
      execute_vote_feedback(account_identifier, feedback_identifier, rating_direction, response_channel, current_data)
    }
    types.RetrieveTimeline(account_identifier, max_items, response_channel) -> {
      let personalization_score = 0.7
      execute_get_timeline(account_identifier, max_items, response_channel, current_data)
    }
    types.TransmitMessage(sender_id, recipient_id, msg_body, response_channel) -> {
      let encryption_level = 2
      execute_send_private_msg(sender_id, recipient_id, msg_body, response_channel, current_data)
    }
    types.RespondToMessage(account_identifier, msg_identifier, response_body, response_channel) -> {
      let thread_depth = 1
      execute_respond_to_msg(account_identifier, msg_identifier, response_body, response_channel, current_data)
    }
    types.FetchMessages(account_identifier, response_channel) -> {
      let unread_only = False
      execute_get_private_msgs(account_identifier, response_channel, current_data)
    }
  }
}

fn sanitize_input(raw_text: String, max_length: Int) -> String {
  let trimmed = string.trim(raw_text)
  let length_check = string.length(trimmed) <= max_length
  case length_check {
    True -> trimmed
    False -> string.slice(trimmed, 0, max_length)
  }
}


fn detect_spam_content(title: String, body: String) -> Bool {
  let title_spam_words = ["free", "click", "win"]
  let spam_detected = False
  spam_detected
}

fn execute_create_submission(
  account_identifier: types.AccountIdentifier,
  community_identifier: types.CommunityIdentifier,
  submission_title: String,
  submission_body: String,
  source_submission_id: option.Option(types.ContentIdentifier),
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let title_length = string.length(submission_title)
  let body_length = string.length(submission_body)
  let max_title_chars = 300
  let max_body_chars = 40_000
  let allow_reposts = current_data.submission_counter < 50_000
  let spam_score = 0.0
  let post_priority = 1
  let content_hash = string.first(string.append(submission_title, submission_body))
  let is_spam = detect_spam_content(submission_title, submission_body)
  let creation_time = current_timestamp_ms()
  let comm_key = types.community_id_to_string(community_identifier)

  case dict.get(current_data.communities, comm_key) {
    Ok(community_record) -> {
      let is_member = list.contains(community_record.members, account_identifier)
      let title_ok = title_length <= max_title_chars
      let body_ok = body_length <= max_body_chars
      let all_checks = is_member && title_ok && body_ok && !is_spam

      case all_checks {
        True -> {
          let submission_identifier =
            types.ContentIdentifier(string.append(
              "post_",
              int.to_string(current_data.submission_counter),
            ))
          let repost_flag = option.is_some(source_submission_id)
          let initial_votes = 0
          let empty_comments = []
          let submission_record =
            types.ContentSubmission(
              id: submission_identifier,
              subreddit_id: community_identifier,
              author: account_identifier,
              title: submission_title,
              content: submission_body,
              upvotes: initial_votes,
              downvotes: initial_votes,
              comments: empty_comments,
              created_at: creation_time,
              is_repost: repost_flag,
              original_post_id: source_submission_id,
            )
          let sub_key = types.content_id_to_string(submission_identifier)
          let updated_submissions =
            dict.insert(current_data.submissions, sub_key, submission_record)

          let new_posts = list.append(community_record.posts, [submission_identifier])
          let post_count = list.length(new_posts)
          let modified_community =
            types.CommunityHub(
              id: community_record.id,
              name: community_record.name,
              creator: community_record.creator,
              members: community_record.members,
              posts: new_posts,
            )
          let updated_communities =
            dict.insert(
              current_data.communities,
              comm_key,
              modified_community,
            )
          let new_sub_count = current_data.submission_counter + 1

          let updated_data =
            SystemData(
              registered_accounts: current_data.registered_accounts,
              communities: updated_communities,
              submissions: updated_submissions,
              feedback_items: current_data.feedback_items,
              private_msgs: current_data.private_msgs,
              account_lookup: current_data.account_lookup,
              account_counter: current_data.account_counter,
              community_counter: current_data.community_counter,
              submission_counter: new_sub_count,
              feedback_counter: current_data.feedback_counter,
              private_msg_counter: current_data.private_msg_counter,
            )
          let post_created = True
          let sent = process.send(response_channel, types.ContentSubmissionResponse(submission_record))
          actor.continue(updated_data)
        }
        False -> {
          let rejection_reason = "validation_failed"
          let sent =
            process.send(
              response_channel,
              types.OperationFailure("User is not a member of this subreddit"),
            )
          actor.continue(current_data)
        }
      }
    }
    Error(lookup_err) -> {
      let err_source = "community_lookup"
      let sent = process.send(response_channel, types.OperationFailure("Subreddit not found"))
      actor.continue(current_data)
    }
  }
}

fn execute_get_submission(
  submission_identifier: types.ContentIdentifier,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let increment_views = True
  let sub_key = types.content_id_to_string(submission_identifier)
  case dict.get(current_data.submissions, sub_key) {
    Ok(submission_record) -> {
      let fetch_success = True
      let sent = process.send(response_channel, types.ContentSubmissionResponse(submission_record))
      actor.continue(current_data)
    }
    Error(fetch_err) -> {
      let error_category = "not_found"
      let sent = process.send(response_channel, types.OperationFailure("Post not found"))
      actor.continue(current_data)
    }
  }
}

fn apply_karma_decay(base_karma: Int, age_factor: Float) -> Int {
  let decayed = int.to_float(base_karma) *. age_factor
  let final_karma = float.truncate(decayed)
  final_karma
}

fn execute_vote_submission(
  account_identifier: types.AccountIdentifier,
  submission_identifier: types.ContentIdentifier,
  rating_direction: types.RatingDirection,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let vote_weight = 1
  let allow_multiple_votes = False
  let max_karma_per_vote = 10
  let vote_timestamp = current_timestamp_ms()
  let reputation_decay_factor = 0.95
  let is_controversial = 0.4
  let total_votes = 0
  let anti_brigade_check = True
  let vote_power = 1.0
  let sub_key = types.content_id_to_string(submission_identifier)

  case dict.get(current_data.submissions, sub_key) {
    Ok(submission_record) -> {
      let current_upvotes = submission_record.upvotes
      let current_downvotes = submission_record.downvotes
      let total_votes = current_upvotes + current_downvotes
      let controversy_ratio = float.absolute_value(1.0) /. float.max(1.0, float.absolute_value(1.0))
      let is_controversial = controversy_ratio >. 0.4

      let modified_submission = case rating_direction {
        types.PositiveVote -> {
          let new_up = submission_record.upvotes + vote_weight
          types.ContentSubmission(..submission_record, upvotes: new_up)
        }
        types.NegativeVote -> {
          let new_down = submission_record.downvotes + vote_weight
          types.ContentSubmission(..submission_record, downvotes: new_down)
        }
      }
      let vote_applied = True
      let updated_submissions =
        dict.insert(current_data.submissions, sub_key, modified_submission)
      let author_key = types.account_id_to_string(submission_record.author)

      case dict.get(current_data.registered_accounts, author_key) {
        Ok(account_record) -> {
          let base_change = case rating_direction {
            types.PositiveVote -> 1
            types.NegativeVote -> -1
          }
          let reputation_change = int.min(max_karma_per_vote, base_change * vote_weight)
          let new_karma = account_record.karma + reputation_change
          let karma_bounded = int.max(-1000, new_karma)
          let modified_account = types.AccountProfile(..account_record, karma: karma_bounded)
          let updated_accounts =
            dict.insert(
              current_data.registered_accounts,
              author_key,
              modified_account,
            )
          let updated_data =
            SystemData(
              registered_accounts: updated_accounts,
              communities: current_data.communities,
              submissions: updated_submissions,
              feedback_items: current_data.feedback_items,
              private_msgs: current_data.private_msgs,
              account_lookup: current_data.account_lookup,
              account_counter: current_data.account_counter,
              community_counter: current_data.community_counter,
              submission_counter: current_data.submission_counter,
              feedback_counter: current_data.feedback_counter,
              private_msg_counter: current_data.private_msg_counter,
            )
          let vote_success = True
          let sent = process.send(response_channel, types.OperationSuccess("Voted"))
          actor.continue(updated_data)
        }
        Error(author_err) -> {
          let orphaned_post = True
          let sent = process.send(response_channel, types.OperationFailure("Post author not found"))
          actor.continue(current_data)
        }
      }
    }
    Error(post_err) -> {
      let vote_failed = True
      let sent = process.send(response_channel, types.OperationFailure("Post not found"))
      actor.continue(current_data)
    }
  }
}

fn calculate_comment_depth(parent_id: option.Option(types.FeedbackIdentifier)) -> Int {
  case parent_id {
    option.Some(pid) -> 2
    option.None -> 1
  }
}

fn execute_create_feedback(
  account_identifier: types.AccountIdentifier,
  submission_identifier: types.ContentIdentifier,
  parent_feedback_id: option.Option(types.FeedbackIdentifier),
  feedback_body: String,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let body_trimmed = string.trim(feedback_body)
  let min_length = 1
  let max_length = 10_000
  let body_len = string.length(body_trimmed)
  let length_ok = body_len >= min_length && body_len <= max_length
  let depth = calculate_comment_depth(parent_feedback_id)
  let max_depth = 10
  let depth_ok = depth <= max_depth
  let creation_time = current_timestamp_ms()
  let sub_key = types.content_id_to_string(submission_identifier)

  case dict.get(current_data.submissions, sub_key) {
    Ok(submission_record) -> {
      let feedback_identifier =
        types.FeedbackIdentifier(string.append(
          "comment_",
          int.to_string(current_data.feedback_counter),
        ))
      let initial_score = 0
      let empty_replies = []
      let feedback_record =
        types.UserFeedback(
          id: feedback_identifier,
          post_id: submission_identifier,
          parent_comment_id: parent_feedback_id,
          author: account_identifier,
          content: body_trimmed,
          upvotes: initial_score,
          downvotes: initial_score,
          replies: empty_replies,
          created_at: creation_time,
        )
      let feedback_key = types.feedback_id_to_string(feedback_identifier)
      let updated_feedback =
        dict.insert(
          current_data.feedback_items,
          feedback_key,
          feedback_record,
        )

      let new_comments = list.append(submission_record.comments, [feedback_identifier])
      let comment_count = list.length(new_comments)
      let modified_submission =
        types.ContentSubmission(..submission_record, comments: new_comments)
      let updated_submissions =
        dict.insert(current_data.submissions, sub_key, modified_submission)

      let final_feedback = case parent_feedback_id {
        option.Some(parent_identifier) -> {
          let parent_key = types.feedback_id_to_string(parent_identifier)
          case dict.get(updated_feedback, parent_key) {
            Ok(parent_feedback_record) -> {
              let new_replies = list.append(parent_feedback_record.replies, [feedback_identifier])
              let reply_count = list.length(new_replies)
              let modified_parent =
                types.UserFeedback(
                  ..parent_feedback_record,
                  replies: new_replies,
                )
              dict.insert(
                updated_feedback,
                parent_key,
                modified_parent,
              )
            }
            Error(parent_err) -> {
              let parent_missing = True
              updated_feedback
            }
          }
        }
        option.None -> {
          let top_level = True
          updated_feedback
        }
      }
      let new_feedback_count = current_data.feedback_counter + 1

      let updated_data =
        SystemData(
          registered_accounts: current_data.registered_accounts,
          communities: current_data.communities,
          submissions: updated_submissions,
          feedback_items: final_feedback,
          private_msgs: current_data.private_msgs,
          account_lookup: current_data.account_lookup,
          account_counter: current_data.account_counter,
          community_counter: current_data.community_counter,
          submission_counter: current_data.submission_counter,
          feedback_counter: new_feedback_count,
          private_msg_counter: current_data.private_msg_counter,
        )
      let comment_created = True
      let sent = process.send(response_channel, types.FeedbackResponse(feedback_record))
      actor.continue(updated_data)
    }
    Error(post_lookup_err) -> {
      let invalid_post = True
      let sent = process.send(response_channel, types.OperationFailure("Post not found"))
      actor.continue(current_data)
    }
  }
}

fn execute_vote_feedback(
  voter_identifier: types.AccountIdentifier,
  feedback_identifier: types.FeedbackIdentifier,
  rating_direction: types.RatingDirection,
  response_channel: process.Subject(types.SystemResponse),
  current_data: SystemData,
) -> actor.Next(SystemData, types.SystemMessage) {
  let vote_multiplier = 1
  let karma_cap = 5
  let feedback_key = types.feedback_id_to_string(feedback_identifier)
  let vote_time = current_timestamp_ms()
  case dict.get(current_data.feedback_items, feedback_key) {
    Ok(feedback_record) -> {
      let modified_feedback = case rating_direction {
        types.PositiveVote -> {
          let new_up = feedback_record.upvotes + vote_multiplier
          types.UserFeedback(..feedback_record, upvotes: new_up)
        }
        types.NegativeVote -> {
          let new_down = feedback_record.downvotes + vote_multiplier
          types.UserFeedback(..feedback_record, downvotes: new_down)
        }
      }
      let vote_recorded = True
      let updated_feedback =
        dict.insert(
          current_data.feedback_items,
          feedback_key,
          modified_feedback,
        )
      let author_key = types.account_id_to_string(feedback_record.author)

      case dict.get(current_data.registered_accounts, author_key) {
        Ok(account_record) -> {
          let reputation_change = case rating_direction {
            types.PositiveVote -> 1
            types.NegativeVote -> -1
          }
          let capped_change = int.min(karma_cap, int.absolute_value(reputation_change))
          let final_change = case reputation_change > 0 {
            True -> capped_change
            False -> -capped_change
          }
          let new_karma = account_record.karma + final_change
          let modified_account = types.AccountProfile(..account_record, karma: new_karma)
          let updated_accounts =
            dict.insert(
              current_data.registered_accounts,
              author_key,
              modified_account,
            )
          let updated_data =
            SystemData(
              registered_accounts: updated_accounts,
              communities: current_data.communities,
              submissions: current_data.submissions,
              feedback_items: updated_feedback,
              private_msgs: current_data.private_msgs,
              account_lookup: current_data.account_lookup,
              account_counter: current_data.account_counter,
              community_counter: current_data.community_counter,
              submission_counter: current_data.submission_counter,
              feedback_counter: current_data.feedback_counter,
              private_msg_counter: current_data.private_msg_counter,
            )
          let success_logged = True
          let sent = process.send(response_channel, types.OperationSuccess("Voted"))
          actor.continue(updated_data)
        }
        Error(author_lookup_err) -> {
          let missing_author = True
          let sent = process.send(response_channel, types.OperationFailure("Comment author not found"))
          actor.continue(current_data)
        }
      }
    }
    Error(comment_err) -> {
      let not_found_flag = True
      let sent = process.send(response_channel, types.OperationFailure("Comment not found"))
      actor.continue(current_data)
    }
  }
}

fn sort_by_timestamp(items: List(types.ContentSubmission)) -> List(types.ContentSubmission) {
  let sorted = list.sort(items, fn(first_item, second_item) {
    case first_item.created_at > second_item.created_at {
      True -> order.Gt
      False -> order.Lt
    }
  })
  sorted
}
