library(dplyr)
library(wordcloud)

source("scrape-reddit.R")
source("mining.R")

# Scrape reddit comments.
subreddits <- c("Providence", "RhodeIsland")
keywords <- c("sustainability")
links <- collectUrls(subreddits, keywords)
comments <- readComments(links)

# Pre-process the comments.
corpus <- createCorpus(comments)
corpus.t <- preprocessCorpus(corpus, ignoreStem = c("providence"))
dtm <- documentTermMatrix(corpus.t)

# Observe frequent terms.
set.seed(1324)

wordcloud(
  names(frequentTerms(dtm, 25)),
  frequentTerms(dtm, 25),
  max.words = 25,
  colors = brewer.pal(8, "Dark2")
)

barplot(
  frequentTerms(dtm, 15),
  xlab = "Words",
  ylab = "Frequency",
  main = "Frequent Terms in Reddit Comments"
)
