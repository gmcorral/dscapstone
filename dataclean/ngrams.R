# 1-gram
#all.splitted <- sapply(all, strsplit, split=" ")
#all.1gram <- make.ngrams(unlist(all.splitted))
#all.1gram.table <- table(all.1gram)
#all.1gram.sorted <- sort(all.1gram.table, decreasing = TRUE)

# 2-gram
#all.splitted <- all.splitted[sapply(all.splitted, function(x) length(x) > 1)]
#all.2gram <- unlist(saplly(all.splitted, make.ngrams, 2))
#all.2gram.table <- table(all.2gram)
#all.2gram.sorted <- sort(all.2gram.table, decreasing = TRUE)

# 3-gram
#all.splitted <- all.splitted[sapply(all.splitted, function(x) length(x) > 2)]
#all.3gram <- unlist(saplly(all.splitted, make.ngrams, 3))
#all.3gram.table <- table(all.3gram)
#all.3gram.sorted <- sort(all.3gram.table, decreasing = TRUE)

# 4-gram
#all.splitted <- all.splitted[sapply(all.splitted, function(x) length(x) > 4)]
#all.4gram <- unlist(saplly(all.splitted, make.ngrams, 4))
#all.4gram.table <- table(all.4gram)
#all.4gram.sorted <- sort(all.4gram.table, decreasing = TRUE)

library(data.table)
library(stylo)
library(stringr)

make.ngram <- function (data, n, sorted)
{
	print("Splitting..")
	start <- Sys.time()
	print(start)
	
    #data <- sapply(data, USE.NAMES=FALSE, strsplit, split=" ")
	if(n > 1)
	{
		print("Trimming..")
		print(Sys.time())
		data <- data[sapply(data, function(x) length(x) >= n)]
	}
	
	print("Making n-gram..")
	print(Sys.time())
	
	data.gram <- unlist(sapply(data, make.ngrams, n))
	
	print("Converting to table..")
	print(Sys.time())
	
	data.gram.table <- table(data.gram)
		
	if(sorted)
	{
		print("Sorting..")
		print(Sys.time())
		data.gram.table <- sort(data.gram.table, decreasing = TRUE)
	}
	
	print("Done")
	print(Sys.time())
	print(Sys.time() - start)
	
	data.gram.table
}

# start script
setwd("/Users/gmcorral/Coursera/datascientist/capstone//final/en_US/")

# read clean sources
twitter.clean <- fread("en_US.twitter.clean.txt", sep="\n", header=FALSE, showProgress = TRUE)
blogs.clean <- fread("en_US.blogs.clean.txt", sep="\n", header=FALSE, showProgress = TRUE)
news.clean <- fread("en_US.news.clean.txt", sep="\n", header=FALSE, showProgress = TRUE)

# join files and create sample
all <- unlist(c(twitter.clean, blogs.clean, news.clean), use.names=FALSE)
rm(list=c("blogs.clean", "news.clean", "twitter.clean"))
all.sample <- sample(all, 3243174, replace = FALSE)
rm(all)
all.sample <- sapply(all.sample, USE.NAMES=FALSE, strsplit, split=" ")
save(all.sample, file='sample.RData')
gc()

# backup n-grams
#write.table(all.1gram, file = "en_US.all.1gram.txt", row.names = TRUE, col.names = FALSE)
#write.table(all.2gram, file = "en_US.all.2gram.txt", row.names = TRUE, col.names = FALSE)
#write.table(all.3gram, file = "en_US.all.3gram.txt", row.names = TRUE, col.names = FALSE)
#write.table(all.4gram, file = "en_US.all.4gram.txt", row.names = TRUE, col.names = FALSE)

# create data frames from n-grams
print("Calculating 1-gram..")
all.1gram <- make.ngram(all.sample, 1, TRUE)
rm(all.sample)
gc()

print("Data frame for 1-gram..")
print(Sys.time())
all.1gram <- all.1gram[all.1gram > 100]
all.1gram.df <- data.frame(w1=names(all.1gram), p=all.1gram,
							row.names=seq_along(all.1gram))
save(all.1gram.df, file='1gram.RData')
rm(list=c('all.1gram', 'all.1gram.df'))
gc()
print(Sys.time())


print("Calculating 2-gram..")
load('sample.RData')
all.2gram <- make.ngram(all.sample, 2, TRUE)
rm(all.sample)
gc()

print("Data frame for 2-gram..")
print(Sys.time())
all.2gram <- all.2gram[all.2gram > 2]
gc()
splitted.2gram <- str_split_fixed(names(all.2gram), " ", 2)
print(Sys.time())
all.2gram.df <- data.frame(w1=splitted.2gram[,1], w2=splitted.2gram[,2], p=all.2gram,
							row.names=seq_along(all.2gram))
save(all.2gram.df, file='2gram.RData')
rm(list=c('all.2gram', 'all.2gram.df', 'splitted.2gram'))
gc()
print(Sys.time())


print("Calculating 3-gram..")
load('sample.RData')
all.3gram <- make.ngram(all.sample, 3, TRUE)
rm(all.sample)
gc()
print("Data frame for 3-gram..")
print(Sys.time())
all.3gram <- all.3gram[all.3gram > 2]
gc()
splitted.3gram <- str_split_fixed(names(all.3gram), " ", 3)
print(Sys.time())
all.3gram.df <- data.frame(w1=splitted.3gram[,1], w2=splitted.3gram[,2], w3=splitted.3gram[,3],
							p=all.3gram, row.names=seq_along(all.3gram))
save(all.3gram.df, file='3gram.RData')
rm(list=c('all.3gram', 'all.3gram.df', 'splitted.3gram'))
gc()
print(Sys.time())


print("Calculating 4-gram..")
load('sample.RData')
all.4gram <- make.ngram(all.sample, 4, TRUE)
rm(all.sample)
gc()

print("Data frame for 4-gram..")
print(Sys.time())
all.4gram <- all.4gram[all.4gram > 2]
gc()
splitted.4gram <- str_split_fixed(names(all.4gram), " ", 4)
print(Sys.time())
all.4gram.df <- data.frame(w1=splitted.4gram[,1], w2=splitted.4gram[,2], w3=splitted.4gram[,3],
							w4=splitted.4gram[,4], p=all.4gram, row.names=seq_along(all.4gram))
save(all.4gram.df, file='4gram.RData')
rm(list=c('all.4gram', 'all.4gram.df', 'splitted.4gram'))
gc()
print(Sys.time())

# calculate relative weights
#total.1gram <- sum(all.1gram.df[,2])
#all.1gram.df[,2] <- all.1gram.df[,2]/total.1gram

#calc.2gram.weight <- function (row)
#{
#	as.numeric(row[3]) / all.1gram.df[all.1gram.df$w1 == row[1], 2]
#}
#all.2gram.df$p <- sapply(all.2gram.df, calc.2gram.weight)



# predict
predict.words <- function(phrase, nwords)
{
	if(is.null(phrase) | length(phrase) == 0 | nchar(str_trim(phrase)) == 0)
		as.character(head(all.1gram.df$w1, nwords))

	phrase <- str_trim(gsub("[^[:alpha:]]", " ", phrase))
	words <- unlist(strsplit(phrase, "( )+"))
	size <- length(words)

	if(size >= 3)
		predict.3(words[size-2], words[size-1], words[size], nwords)
	else if(size == 2)
		predict.2(words[1], words[2], nwords)
	else
		predict.1(words[1], nwords)
}

predict.3 <- function(word1, word2, word3, nwords)
{
	candidates <- as.character(all.4gram.df[all.4gram.df$w1 == word1 &
								all.4gram.df$w2 == word2 &
								all.4gram.df$w3 == word3, 4])
	nres <- length(candidates)
	if(nres >= nwords)
		candidates[1:nwords]
	else
		unique(c(candidates, predict.2(word2, word3, nwords - nres)))
}

predict.2 <- function(word1, word2, nwords)
{
	candidates <- as.character(all.3gram.df[all.3gram.df$w1 == word1 &
								all.3gram.df$w2 == word2, 3])
	nres <- length(candidates)
	if(nres >= nwords)
		candidates[1:nwords]
	else
		unique(c(candidates, predict.1(word2, nwords - nres)))
}

predict.1 <- function(word1, nwords)
{
	candidates <- as.character(all.2gram.df[all.2gram.df$w1 == word1, 2])
	nres <- length(candidates)
	if(nres >= nwords)
		candidates[1:nwords]
	else
		unique(c(candidates, as.character(head(all.1gram.df$w1, nwords - nres))))
}
