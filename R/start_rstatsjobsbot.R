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
#'   
#' @importFrom rtweet create_token
#'
#' @export
#'
start_rstatsjobsbot <- function(
  rtweet_app,
  rtweet_consumer_key, 
  rtweet_consumer_secret,
  rtweet_access_token, 
  rtweet_access_secret,
  user = "RStatsJobsBot",
  from_time = Sys.time(),
  max_hashtags = 15
  ) {
  # Set Twitter credentials.
  create_token(
    app = rtweet_app, consumer_key = rtweet_consumer_key, consumer_secret = rtweet_consumer_secret,
    access_token = rtweet_access_token, access_secret = rtweet_access_secret
  )
  message(paste0("user: ", user))
  # Get posts that should be retweeted.
  rtable_posts <- get_rtable_posts(user, from_time, max_hashtags)
  message(paste0(Sys.time(), " - ", nrow(rtable_posts), " tweets to rt."))
  # Update from_time.
  from_time <- Sys.time()
  if (nrow(rtable_posts) > 0) {
    from_time <- max(rtable_posts$created_at)
    # Start retweeting.
    apply(rtable_posts, 1, retweet)
  }
}

#' Get retweetable tweets.
#'
#' Returns a table of tweets to retweet.
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param max_hashtags Numeric indicating the max number of hashtags that a
#'   tweet can have (removed if has more).
#'
#' @importFrom dplyr `%>%` arrange distinct filter
#' @importFrom rtweet search_tweets
#' @importFrom stringr str_count
#'
get_rtable_posts <- function(user, from_time, max_hashtags) {
  # Avoid R CMD check warnings.
  reply_to_screen_name <- screen_name <- created_at <- is_retweet <- text <-
    NULL
  # Get tweets with my username, and newer than from_time.
  mentions <- try({
    search_tweets(user, type = "recent", include_rts = FALSE) %>%
      # Remove replies to me, or posts written by me.
      filter(!reply_to_screen_name %in% user & !screen_name %in% user) %>%
      filter(created_at > from_time)
  })
  # If there was an error (internet mostly) return an empty data.frame .
  if (inherits(mentions, "try-error")) {
    mentions <- data.frame()
  }
  # Get tweets containing the keywords, and newer than from_time.
  kword_tweets <- try({
    # For some reason it is not getting some tweets when `include_rts = FALSE`.
    search_tweets(
      "(rstat OR rstats) AND (hiring)",
      type = "recent",
      include_rts = FALSE
    ) %>%
      arrange(is_retweet) %>%
      distinct(text, .keep_all = TRUE) %>%
      filter(created_at > from_time)
  })
  # If there was an error (internet mostly) return an empty data.frame .
  if (inherits(kword_tweets, "try-error")) {
    kword_tweets <- data.frame()
  }
  # Return both mentions and kword_tweets.
  rbind(mentions, kword_tweets) %>% 
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
  new_tweet_msg <- paste(random_job_message(1), tweet$status_url)
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
