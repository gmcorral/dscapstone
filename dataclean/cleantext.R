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
    sentence <- gsub("n't|n´t", " not", sentence)
    sentence <- gsub("let's|let´s", "let us", sentence)
    sentence <- gsub("'s|´s", " is", sentence)
    sentence <- gsub("'re|´re", " are", sentence)
    sentence <- gsub("gonna", "going to", sentence)
    sentence <- gsub("wanna", "want to", sentence)
    sentence
}

clean.sentence <- function(sentence, dict)
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

    sentence
}

clean.file <- function(table, outputFile, dict)
{
    #conR  <- file(inputFile, open = "r")
    conW  <- file(outputFile, open = "w")

    #maxLines <- 100000
    #count <- 0
    #while ((length(oneLine <- readLines(conR, n = 1, warn = FALSE)) > 0) && (count < maxLines))
    for(i in 1:nrow(twitter))
    {
        oneLine <- as.character(table[i])
        sentences <- sapply(unlist(strsplit(oneLine,
                                    "[\\.;:¡!\\[¿\\?\\(\\)«»<>“”\"\\|&/=\\+\\{}[:digit:]+\\-]")),
                            clean.sentence, dict = dict)
        for(sentence in sentences)
            if(!is.na(sentence) && nchar(sentence) > 0)
                #rbindlist(list(tableClean, as.list(sentence)))
                writeLines(sentence, conW)

        #count <- count + 1
        #print(count)
    }

    rm(sentences)
    rm(sentence)
    rm(oneLine)
    #rm(count)
    #rm(maxLines)

    #close(conR)
    close(conW)
}

# start script
setwd("/Users/gmcorral/Coursera/datascientist/capstone//final/en_US/")

# load english dictionary
dict <- fread("wordlist_lowercase.txt", header = FALSE)

file <- "en_US.twitter.txt"
tt <- tempfile()  # or tempfile(tmpdir="/dev/shm")
system(paste0("tr < ", file, " -d '\\000' >", tt))
twitter <- fread(tt, sep="\n", header=FALSE, showProgress = TRUE)
clean.file(twitter, "en_US.twitter.clean.txt", dict)

blogs <- fread("en_US.blogs.txt", sep="\n", header=FALSE, showProgress = TRUE)
clean.file(blogs, "en_US.blogs.clean.txt", dict)

news <- fread("en_US.news.txt", sep="\n", header=FALSE, showProgress = TRUE)
clean.file(news, "en_US.news.clean.txt", dict)

