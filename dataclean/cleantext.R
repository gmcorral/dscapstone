library(stringr)
library(data.table)
library(tm)

check.word <- function(word, dict)
{
    # find in dictionary
    if(nrow(dict[V1 == word]) > 0)
        word
    else
        ""
}

expand.contr <- function(sentence)
{
    sentence <- gsub("'ll|´ll", " will", sentence)
    sentence <- gsub("'d|´d", " would", sentence)
    sentence <- gsub("'m|´m", " am", sentence)
    sentence <- gsub("'ve|´ve", " have", sentence)
    sentence <- gsub("won't|won´t", "will not", sentence)
    sentence <- gsub("n't|n´t", " not", sentence)
    sentence <- gsub("let's|let´s", "let us", sentence)
    sentence <- gsub("'s|´s", " is", sentence)
    sentence <- gsub("'re|´re", " are", sentence)
    sentence <- gsub("gonna", "going to", sentence)
    sentence <- gsub("wanna", "want to", sentence)
    sentence <- gsub("^b | b | b$", " be ", sentence)
    sentence <- gsub("^r | r | r$", " are ", sentence)
    sentence <- gsub("^u | u | u$", " you ", sentence)
    sentence <- gsub("^w | w | w$", " with ", sentence)
    sentence <- gsub("^s | s | s$", " ", sentence)
    sentence
}

clean.sentence <- function(sentence, dict)
{
    # transform to lower case
    sentence <- tolower(sentence)

	# remove URIs
	sentence <- gsub("http://[[:alnum:]]*", " ", sentence)

    # remove hashtags
    sentence <- gsub("#[a-z0-9]+ ", " ", sentence)

    # expand contractions
    sentence <- expand.contr(sentence)

    # remove any other non alphabetic characters
    sentence <- gsub("[^[:alpha:]]", " ", sentence)

    # filter dictionary words
    #sentence <- paste(sapply(unlist(strsplit(sentence, " ")),
    #                         check.word, dict = dict),
    #                  sep = "", collapse = " ")

    # remove multiple spaces
    sentence <- gsub(" ( )+", " ", sentence)

    # trim leading and trailing blanks
    sentence <- str_trim(sentence)

    sentence[!sentence ==""]
}

clean.file <- function(data, dict)
{
	splitted <- strsplit(data[[1]],"[\\.,;:¡!\\[¿\\?\\(\\)«»<>“”\"\\|&/=\\+\\{}[:digit:]+\\-]")
	unlist(sapply(splitted, clean.sentence, dict = dict))
}

# start script
setwd("/Users/gmcorral/Coursera/datascientist/capstone//final/en_US/")

# load english dictionary
#dict <- fread("wordlist_lowercase.txt", header = FALSE)

# clean twitter
file <- "en_US.twitter.txt"
tt <- tempfile()  # or tempfile(tmpdir="/dev/shm")
system(paste0("tr < ", file, " -d '\\000' >", tt))
twitter <- fread(tt, sep="\n", header=FALSE, showProgress = TRUE)
twitter.clean <- clean.file(twitter, dict)
write.table(twitter.clean, file = "en_US.twitter.clean.txt", row.names = FALSE, col.names = FALSE)
rm(twitter)
rm(twitter.clean)

# clean blogs
blogs <- fread("en_US.blogs.txt", sep="\n", header=FALSE, showProgress = TRUE)
blogs.clean <- clean.file(blogs, dict)
write.table(blogs.clean, file = "en_US.blogs.clean.txt", row.names = FALSE, col.names = FALSE)
rm(blogs)
rm(blogs.clean)

# clean news
news <- fread("en_US.news.txt", sep="\n", header=FALSE, showProgress = TRUE)
news.clean <- clean.file(news, dict)
write.table(news.clean, file = "en_US.new



s.clean.txt", row.names = FALSE, col.names = FALSE)
rm(news)
rm(news.clean)

# corpus
doc.vec <- VectorSource(blogs.clean)
doc.corpus <- Corpus(doc.vec)
summary(doc.corpus)
doc.corpus <- tm_map(doc.corpus, tolower)
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
doc.corpus <- tm_map(doc.corpus, stripWhitespace)
TDM <- TermDocumentMatrix(doc.corpus)
TDM
findFreqTerms(TDM, 2000)
findAssocs(TDM, "love", 0.8)
TDM.common = removeSparseTerms(TDM, 0.1)
TDM.dense = melt(TDM.dense, value.name = "count")
