library(dplyr)
library(tm)
library(SnowballC)

createCorpus <- function(comments) {
  # Extract the 'text' field from each data frame in the list.
  texts <- comments$comments[[9]]

  # Unlist the texts and create a corpus.
  corpus <- VCorpus(VectorSource(unlist(texts)))

  return(corpus)
}

preprocessCorpus <- function(corpus, ignoreStem = character(0)) {
  stopWords <- stopwords("en")

  corpus <- tm_map(
    corpus,
    content_transformer(function(x) parseComment(x, stopWords = stopWords, ignoreStem = ignoreStem))
  )

  return(corpus)
}

parseComment <- function(comment, stopWords = character(0), ignoreStem = character(0)) {
  comment <- removePunctuation(comment)
  comment <- removeNumbers(comment)
  comment <- tolower(comment)
  comment <- removeWords(comment, stopWords)
  comment <- stripWhitespace(comment)

  # Split the comment into words
  words <- strsplit(comment, "\\s")[[1]]

  # Stem words that are not in ignoreStem
  stemmed_words <- sapply(words, function(word) {
    if (word %in% ignoreStem) {
      return(word)
    } else {
      return(wordStem(word))
    }
  })

  # Combine the words back into a single string
  comment <- paste(stemmed_words, collapse = " ")

  return(comment)
}

documentTermMatrix <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  dtm <- removeSparseTerms(dtm, 0.95)

  return(dtm)
}

frequentTerms <- function(dtm, n = 10) {
  # Calculate the frequency of each term.
  freq <- colSums(as.matrix(dtm))

  # Filter terms that appear at least 10 times.
  freq <- subset(freq, freq >= 10)

  # Sort the terms from most to least frequent.
  freq <- sort(freq, decreasing = TRUE)

  # Retrieve the top n terms.
  if (length(freq) > n) {
    freq <- freq[1:n]
  }

  return(freq)
}
