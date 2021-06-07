#
# BUILD CORPUS SEARCH TABLE
#
# REQUIREMENTS:
# 1. 'EDAPREP' RUNS TO DO PRELIMINARY TEXT PREPARATION (CLEANING, LEMMATIZATION, ETC)
# 2. CHANGE 'pctTxt' VALUE IN CALLS TO 'buildDFM' PROCEDURE
# 3. UPDATE THE FINAL OUTPUT FILE NAME (THIS WILL BE INPUT TO 'BuildLookupTables.R')
#
#

library(textclean)
library(textstem)
library(readtext)
library(stringr)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(lubridate)
library(dplyr)
library(data.table)
library(tidyr)

buildNgramList <- function(txtSource, pctTxt="05"){
        startTime <- Sys.time()
        print(paste(now(), "  1. Reading text source ", txtSource))
        inFile <- paste0(getwd(), "/TextData/rt_", txtSource, "_", pctTxt, "PCT.txt", sep="")
        rt_txt <- readtext(inFile)
        print(paste(now(), "  2. Building Corpus ", txtSource))
        rt_corp <- corpus(rt_txt)
        print(paste(now(), "  3. Creating tokens and nGrams for ", txtSource))
        qt <- tokens(rt_corp)
#        qt <- tokens_select(qt,stopwords("en"), selection='remove')
        qt <- tokens_select(qt,pattern="*", min_nchar=2)
#        qtn <- tokens_ngrams(qt,n=nGramSize, concatenator=" ")

        print(paste(now(), "  4. COMPLETE ", txtSource))
        endTime <- Sys.time()
        print(paste("Processing time = ", endTime - startTime))
        
        return(qt)
}
buildDFM <- function(txtSource, pctTxt="40", nGramSize = 2:4){
        startTime <- Sys.time()
        print(paste(now(), "  1. Reading text source ", txtSource))
        inFile <- paste0(getwd(), "/TextData/rt_", txtSource, "_", pctTxt, "PCT.txt", sep="")
        rt_txt <- readtext(inFile)
        print(paste(now(), "  2. Building Corpus ", txtSource))
        rt_corp <- corpus(rt_txt)
        print(paste(now(), "  3. Creating tokens and nGrams ", nGramSize, " for ", txtSource))
        qt <- tokens(rt_corp)
#        qt <- tokens_select(qt,stopwords("en"), selection='remove')
        qt <- tokens_select(qt,pattern="*", min_nchar=2)
        qtn <- tokens_ngrams(qt,n=nGramSize, concatenator=" ")
        print(paste(now(), "  4. Building DFM ", txtSource))
        dfm <- dfm(qtn)
        dfmList <- dfm_trim(dfm, min_termfreq = 2)
        print(paste(now(), "  5. COMPLETE ", txtSource))
        endTime <- Sys.time()
        print(paste("Processing time = ", endTime - startTime))
        
        return(dfmList)
}

newsNgrams <- buildNgramList("news", pctTxt="40")
blogsNgrams <- buildNgramList("blogs", pctTxt="40")
twitterNgrams <- buildNgramList("twitter", pctTxt="40")

tokenDFM <- rbind(newsNgrams, blogsNgrams, twitterNgrams)
# tokenList <- as.data.frame(tokenList)
outFile <- paste0(getwd(),"/TextData/tokenList_40PCT.csv")
write.csv(tokenList,outFile, row.names=T)

newsDFM    <- buildDFM("news", pctTxt="40")
blogsDFM   <- buildDFM("blogs", pctTxt="40")
twitterDFM <- buildDFM("twitter", pctTxt="40")

dfmc <- rbind(newsDFM, blogsDFM, twitterDFM)
flist <- textstat_frequency(dfmc)
flist <- as.data.frame(flist)

flist <- subset(flist,select=c("feature", "frequency"))
flist$nGramCount <- sapply(str_split(flist$feature," "),length)
markovTable <- flist
markovTable <- markovTable %>% mutate(searchWords = word(feature, 1, -2))  
markovTable <- markovTable %>% mutate(nextWord = word(feature, -1))
outFile <- paste0(getwd(),"/TextData/markovTable_40PCT.csv")
write.csv(markovTable,outFile, row.names=T)


itemsToRemoveFromEnv <- c("blogsDFM", "newsDFM", "twitterDFM", 
                          "qt", "qtn", "rt_corp", "flist","rt_txt", "dfmc")
rm(list=itemsToRemoveFromEnv)


