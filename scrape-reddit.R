library(RedditExtractoR)


# Collect URLs from provided subreddits and keywords.
collectUrls <- function(subreddits, keywords) {
  urls <- list()
  for (sub in subreddits) {
    for (key in keywords) {
      urls <- c(urls, RedditExtractoR::find_thread_urls(key, su = sub, period = "all"))
    }
  }

  deduplicated <- unique(urls$url)

  return(deduplicated)
}

readComments <- function(urls) {
  comments <- RedditExtractoR::get_thread_content(urls)
  return(comments)
}
