library(stringr)
library(data.table)

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
    sentence <- gsub("n't|n´t", "n not", sentence)
    sentence <- gsub("let's|let´s", "let us", sentence)
    sentence <- gsub("'s|´s", " is", sentence)
    sentence <- gsub("'re|´re", " are", sentence)
    sentence <- gsub("gonna", "going to", sentence)
    sentence <- gsub("wanna", "want to", sentence)
    sentence
}

clean.sentence <- function(sentence)#, dict)
{
    # transform to lower case
    sentence <- tolower(sentence)

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

clean.file <- function(file, dict)
{
	splitted <- strsplit(blogs[[1]],"[\\.,;:¡!\\[¿\\?\\(\\)«»<>“”\"\\|&/=\\+\\{}[:digit:]+\\-]")
	unlist(sapply(splitted, clean.sentence))
}

# start script
setwd("/Users/gmcorral/Coursera/datascientist/capstone//final/en_US/")

# load english dictionary
dict <- fread("wordlist_lowercase.txt", header = FALSE)

# clean twitter
file <- "en_US.twitter.txt"
tt <- tempfile()  # or tempfile(tmpdir="/dev/shm")
system(paste0("tr < ", file, " -d '\\000' >", tt))
twitter <- fread(tt, sep="\n", header=FALSE, showProgress = TRUE)
clean.file(twitter, dict)

# clean blogs
blogs <- fread("en_US.blogs.txt", sep="\n", header=FALSE, showProgress = TRUE)
blogs.clean <- clean.file(blogs, dict)

# clean news
news <- fread("en_US.news.txt", sep="\n", header=FALSE, showProgress = TRUE)
news.clean <- clean.file(news, dict)
