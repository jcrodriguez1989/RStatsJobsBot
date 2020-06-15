#' Start RStatsJobsBot bot server.
#'
#' Starts the server, will be executing until it is aborted Ctrl+c .
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#' @param sleep_secs Numeric indicating the seconds to sleep between execution.
#' @param rtweet_token_file A character with the file path of an rds file with
#'   the rtweet token to be used. If NULL, it will load the default token.
#'
#' @export
#'
start_rstatsjobsbot <- function(
  user = "RStatsJobsBot",
  from_time = Sys.time(),
  sleep_secs = 15 * 60,
  rtweet_token_file = NULL
  ) {
  # If rtweet token provided:
  if (!is.null(rtweet_token_file)) {
    # then load it by editing the default token in the rtweet env.
    assign(
      "twitter_tokens",
      readRDS(rtweet_token_file),
      envir = rtweet:::.state
    )
  }
  message(paste0("user: ", user, "\nsleep_secs: ", sleep_secs))
  # Forever do:
  while (TRUE) {
    # Get posts that should be retweeted.
    rtable_posts <- get_rtable_posts(user, from_time)
    message(paste0(Sys.time(), " - ", nrow(rtable_posts), " tweets to rt."))
    # Update from_time.
    from_time <- Sys.time()
    if (nrow(rtable_posts) > 0) {
      from_time <- max(rtable_posts$created_at)
      # Start retweeting.
      apply(rtable_posts, 1, retweet)
    }
    # Let RStatsJobsBot sleep a little bit.
    Sys.sleep(sleep_secs)
  }
}

#' Get retweetable tweets.
#'
#' Returns a table of tweets to retweet.
#'
#' @param user A character with the bot username.
#' @param from_time A POSIXct indicating minimum tweets time to attend.
#'
#' @importFrom dplyr `%>%` arrange distinct filter
#' @importFrom rtweet search_tweets
#'
get_rtable_posts <- function(user, from_time) {
  # Avoid R CMD check warnings.
  created_at <- is_retweet <- text <- NULL
  # Get tweets with my username, and newer than from_time.
  mentions <- try({
    search_tweets(user, type = "recent", include_rts = FALSE) %>%
      filter(!reply_to_screen_name %in% user) %>%
      filter(created_at > from_time)
  })
  # If there was an error (internet mostly) return an empty data.frame .
  if (inherits(mentions, "try-error")) {
    mentions <- data.frame()
  }
  # Get tweets containing the keywords, and newer than from_time.
  kword_tweets <- try({
    # For some reason it is not getting some tweets, so allow retweets.
    search_tweets(
      "(rstat OR rstats) AND (hiring)",
      type = "recent",
      include_rts = TRUE
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
  rbind(mentions, kword_tweets)
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
  suppressMessages(try(post_tweet(retweet_id = tweet$status_id)))
}
