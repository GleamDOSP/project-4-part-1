import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub type AccountIdentifier {
  AccountIdentifier(String)
}

pub type CommunityIdentifier {
  CommunityIdentifier(String)
}

pub type ContentIdentifier {
  ContentIdentifier(String)
}

pub type FeedbackIdentifier {
  FeedbackIdentifier(String)
}

pub type MessageIdentifier {
  MessageIdentifier(String)
}

pub fn feedback_id_to_string(identifier: FeedbackIdentifier) -> String {
  case identifier {
    FeedbackIdentifier(text_value) -> text_value
  }
}

pub fn account_id_to_string(identifier: AccountIdentifier) -> String {
  case identifier {
    AccountIdentifier(text_value) -> text_value
  }
}

pub fn content_id_to_string(identifier: ContentIdentifier) -> String {
  case identifier {
    ContentIdentifier(text_value) -> text_value
  }
}

pub fn community_id_to_string(identifier: CommunityIdentifier) -> String {
  case identifier {
    CommunityIdentifier(text_value) -> text_value
  }
}

pub fn message_id_to_string(identifier: MessageIdentifier) -> String {
  case identifier {
    MessageIdentifier(text_value) -> text_value
  }
}

pub type AccountProfile {
  AccountProfile(
    joined_subreddits: List(CommunityIdentifier),
    karma: Int,
    username: String,
    id: AccountIdentifier,
  )
}

pub type CommunityHub {
  CommunityHub(
    members: List(AccountIdentifier),
    creator: AccountIdentifier,
    posts: List(ContentIdentifier),
    name: String,
    id: CommunityIdentifier,
  )
}

pub type ContentSubmission {
  ContentSubmission(
    original_post_id: Option(ContentIdentifier),
    downvotes: Int,
    comments: List(FeedbackIdentifier),
    is_repost: Bool,
    author: AccountIdentifier,
    title: String,
    created_at: Int,
    subreddit_id: CommunityIdentifier,
    upvotes: Int,
    content: String,
    id: ContentIdentifier,
  )
}

pub type UserFeedback {
  UserFeedback(
    replies: List(FeedbackIdentifier),
    created_at: Int,
    parent_comment_id: Option(FeedbackIdentifier),
    upvotes: Int,
    content: String,
    author: AccountIdentifier,
    downvotes: Int,
    post_id: ContentIdentifier,
    id: FeedbackIdentifier,
  )
}

pub type PrivateMessage {
  PrivateMessage(
    parent_message_id: Option(MessageIdentifier),
    created_at: Int,
    content: String,
    to: AccountIdentifier,
    replies: List(MessageIdentifier),
    from: AccountIdentifier,
    id: MessageIdentifier,
  )
}

pub type RatingDirection {
  PositiveVote
  NegativeVote
}

pub type SystemMessage {
  FetchAccountProfile(AccountIdentifier, Subject(SystemResponse))
  RegisterAccount(String, Subject(SystemResponse))
  SubmitFeedback(
    AccountIdentifier,
    ContentIdentifier,
    Option(FeedbackIdentifier),
    String,
    Subject(SystemResponse),
  )
  EstablishCommunity(AccountIdentifier, String, Subject(SystemResponse))
  FetchCommunityHub(CommunityIdentifier, Subject(SystemResponse))
  SubscribeToCommunity(AccountIdentifier, CommunityIdentifier, Subject(SystemResponse))
  PublishContent(
    AccountIdentifier,
    CommunityIdentifier,
    String,
    String,
    Option(ContentIdentifier),
    Subject(SystemResponse),
  )
  UnsubscribeFromCommunity(AccountIdentifier, CommunityIdentifier, Subject(SystemResponse))
  RateContent(AccountIdentifier, ContentIdentifier, RatingDirection, Subject(SystemResponse))
  RetrieveContent(ContentIdentifier, Subject(SystemResponse))
  TransmitMessage(AccountIdentifier, AccountIdentifier, String, Subject(SystemResponse))
  RateFeedback(AccountIdentifier, FeedbackIdentifier, RatingDirection, Subject(SystemResponse))
  RetrieveTimeline(AccountIdentifier, Int, Subject(SystemResponse))
  RespondToMessage(AccountIdentifier, MessageIdentifier, String, Subject(SystemResponse))
  FetchMessages(AccountIdentifier, Subject(SystemResponse))
}

pub type SystemResponse {
  OperationSuccess(String)
  AccountProfileResponse(AccountProfile)
  OperationFailure(String)
  CommunityHubResponse(CommunityHub)
  FeedbackResponse(UserFeedback)
  ContentSubmissionResponse(ContentSubmission)
  MessageListResponse(List(PrivateMessage))
  ContentListResponse(List(ContentSubmission))
}