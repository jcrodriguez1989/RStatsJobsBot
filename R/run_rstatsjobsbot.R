#' Start RStatsJobsBot bot server.
#'
#' Starts the server, will be executing until it is aborted Ctrl+c .
#'
#' @param rtweet_app Twitter credentials.
#' @param rtweet_consumer_key Twitter credentials.
#' @param rtweet_consumer_secret Twitter credentials.
#' @param rtweet_access_token Twitter credentials.
#' @param rtweet_access_secret Twitter credentials.
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param max_hashtags Numeric indicating the max number of hashtags that a
#'   tweet can have (removed if has more).
#' @param blocked A character vector with the `screen_name`s of blocked accounts (not to retweet).
#'
#' @importFrom dplyr distinct
#' @importFrom rtweet create_token
#'
#' @export
#'
run_rstatsjobsbot <- function(rtweet_app,
                                rtweet_consumer_key,
                                rtweet_consumer_secret,
                                rtweet_access_token,
                                rtweet_access_secret,
                                user = "RStatsJobsBot",
                                from_time = Sys.time(),
                                max_hashtags = 15,
                                blocked = c()) {
  # Set Twitter credentials.
  create_token(
    app = rtweet_app, consumer_key = rtweet_consumer_key, consumer_secret = rtweet_consumer_secret,
    access_token = rtweet_access_token, access_secret = rtweet_access_secret
  )
  message(paste0("user: ", user))
  # Get posts that should be retweeted.
  rtable_posts <- get_rtable_posts(user, from_time, max_hashtags, blocked)
  message(paste0(Sys.time(), " - ", nrow(rtable_posts), " tweets to rt."))
  id_str <- NULL
  if (nrow(rtable_posts) > 0) {
    # Start retweeting.
    rtable_posts <- distinct(rtable_posts, id_str, .keep_all = TRUE)
    apply(rtable_posts, 1, retweet)
  }
}

#' Search Tweets With User Data.
#' 
#' Search tweets, and return them with users data.
#' 
#' @param q Query to be searched, see `?rtweet::search_tweets`.
#' @param type Which type of search results to return, see `?rtweet::search_tweets`.
#' @param include_rts Logical, indicating whether to include retweets in search results, see
#'   `?rtweet::search_tweets`.
#' 
#' @importFrom dplyr `%>%` bind_cols rename_at
#' @importFrom rtweet search_tweets users_data
#' 
search_tweets_with_user_data <- function(q, type = "recent", include_rts = FALSE) {
  tweets <- try(search_tweets(q, type = type, include_rts = include_rts))
  # If there was an error (internet mostly) return an empty data.frame .
  if (inherits(tweets, "try-error")) {
    return(data.frame(text = character()))
  }
  if (nrow(tweets) == 0) {
    return(tweets)
  }
  tweets_users_data <- users_data(tweets) %>%
    rename_at(
      c("id", "id_str", "created_at", "withheld_in_countries", "withheld_scope", "entities"),
      function(colname) paste0("users_data_", colname)
    )
  bind_cols(tweets, tweets_users_data)
}

#' Get retweetable tweets.
#'
#' Returns a table of tweets to retweet.
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param max_hashtags Numeric indicating the max number of hashtags that a
#'   tweet can have (removed if has more).
#' @param blocked A character vector with the `screen_name`s of blocked accounts (not to retweet).
#'
#' @importFrom dplyr `%>%` anti_join arrange distinct filter
#' @importFrom rtweet get_timeline search_tweets
#' @importFrom stringr str_count
#'
get_rtable_posts <- function(user, from_time, max_hashtags, blocked) {
  # Avoid R CMD check warnings.
  in_reply_to_screen_name <- screen_name <- created_at <- text <- quoted_status_id <- NULL
  # Get tweets with my username.
  mentions <- search_tweets_with_user_data(user)
  # Get tweets containing the keywords.
  kword_tweets <- search_tweets_with_user_data("(rstat OR rstats) AND (hiring)") %>%
    distinct(text, .keep_all = TRUE)
  # If there was an error (internet mostly) return an empty data.frame .
  if (inherits(kword_tweets, "try-error")) {
    kword_tweets <- data.frame()
  }
  # Return both mentions and kword_tweets, but remove already posted tweets.
  rbind(mentions, kword_tweets) %>% 
    # Remove replies to me, or posts written by me.
    filter(!in_reply_to_screen_name %in% user & !screen_name %in% user) %>%
    # Keep newer than from_time.
    filter(created_at > from_time) %>% 
    # Remove blocked accounts.
    filter(!screen_name %in% blocked) %>% 
    # Remove already tweeted by me.
    filter(!id_str %in% get_timeline(user)$quoted_status_id_str) %>% 
    # Remove tweets with multiple hashtags.
    filter(str_count(text, "#") <= max_hashtags)
}

#' Retweets a tweet.
#'
#' For now, just that, retweets a tweet.
#'
#' @param tweet A list with the tweet data.
#'
#' @importFrom rtweet post_tweet
#'
retweet <- function(tweet) {
  new_tweet_msg <- paste0(
    random_job_message(1), " https://twitter.com/staceystats/status/", tweet$id_str
  )
  suppressMessages(try(post_tweet(new_tweet_msg)))
  # This one just retweets:
  # suppressMessages(try(post_tweet(retweet_id = tweet$status_id)))
}

#' Returns random messages.
#'
#' Return a message to add to retweet with comments.
#'
#' @param n A numeric indicating the number of messages to return.
#'
random_job_message <- function(n) {
  sample(c(
    "R job alert!",
    "R jobs jobs jobs!",
    "Get the R job of your dreams!",
    "Do you want an R job?",
    "Woohoo, R job!"
  ), n, replace = TRUE)
}
