library(stringr)
library(data.table)

clean.word <- function(word, dict)
{
    # find in dictionary
    if(nrow(dict[V1 == word]) > 0)
        word
    else
        ""
}

find.words <- function(inputFile, outputFile, dict)
{

    conR  <- file(inputFile, open = "r")
    conW  <- file(outputFile, open = "w")

    while ((length(oneLine <- readLines(conR, n = 1, warn = FALSE)) > 0))
    {
        words <- str_trim(paste(sapply(unlist(strsplit(oneLine, " ")), clean.word, dict = dict),
                       sep = "", collapse = " "))
        if(nchar(words) > 0)
            writeLines(words, conW)
    }
    rm(words)
    rm(oneLine)
    close(conR)
    close(conW)
}

# start script
setwd("/Users/gmcorral/Coursera/datascientist/capstone//final/en_US/")

dict <- fread("wordlist_lowercase.txt", header = FALSE)

find.words("en_US.twitter.clean.txt", "en_US.twitter.correct.txt", dict)
find.words("en_US.blogs.clean.txt", "en_US.blogs.correct.txt", dict)
find.words("en_US.news.clean.txt", "en_US.news.correct.txt", dict)

#rm(dict)
