#
# BUILD THE PROBABILITY LOOKUP TABLES TO BE MARRIED WITH THE MARKOV TABLE
#
# REQUIREMENTS:
# 1. CHANGE 'inFile" NAME TO MATCH OUTPUT FROM 'buildMarkovTable.R' PROCEDURE

library(dplyr)
library(readtext)
library(reader)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)

inFile <- paste0(getwd(),"/TextData/markovTable_40PCT.csv")                  # CHANGE THIS FILE NAME!
rt_txt <- read.csv(inFile)

markovTable <- as.data.frame(rt_txt)

mc           <- aggregate(markovTable$frequency, 
                          by=list(markovTable$searchWords), FUN=sum)         # COUNT OF TIMES "SEARCH WORDS" APPEARS IN markovTable
nw           <- markovTable %>% group_by(nGramCount) %>% count(nextWord)     # COUNT OF TIMES "NEXT WORD" APPEARS IN markovTable
nw$searchKey <- paste0(nw$nGramCount,nw$nextWord)
mcType       <- markovTable %>% count(searchWords)                           # COUNT OF SEARCH WORD "TYPES"
nGramCount   <- markovTable %>% count(nGramCount)

colnames(mc)         <- c("searchWords", "freq")
colnames(nw)         <- c("nGramCount","nextWord", "freq", "searchKey")
colnames(mcType)     <- c("searchWords", "freq")
colnames(nGramCount) <- c("nGram", "freq")

outFile <- paste0(getwd(),"/TextData/mc.csv")
write.csv(mc,outFile, row.names=T)

outFile <- paste0(getwd(),"/TextData/nw.csv")
write.csv(nw,outFile, row.names=T)

outFile <- paste0(getwd(),"/TextData/mcType.csv")
write.csv(mcType,outFile, row.names=T)

outFile <- paste0(getwd(),"/TextData/nGramCount.csv")
write.csv(nGramCount,outFile, row.names=T)



