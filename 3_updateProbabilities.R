#
# UPDATE MARKOV TEXT PREDICTION TABLE WITH WORD PROBABILITIES USING
# MODIFIED INTERPOLATED KNESER-NEY SMOOTHING VALUES (RESULT IS "PKN")
#
# NOTE:  THIS IS A LONG RUNNING PROCEDURE SO YOU WILL RUN THIS MULTIPLE TIMES
# REQUIREMENTS:
#    PROCEDURE '2_buildLookupTables.R' MUST HAVE RUN TO BUILD LOOKUP TABLES
# 1. UPDATE THE 'inFile' NAME AND 'outFile' NAME
# 2. UPDATE THE 'skip' AND 'nrows' IN THE 'read.csv' STATEMENT
#       

library(dplyr)
library(readtext)
library(reader)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)


inFile <- paste0(getwd(),"/TextData/markovTable_40PCT.csv")
outFile <- paste0(getwd(),"/TextData/markovTable_1750001.csv")
headers <- names(read.csv(inFile,nrows=1))
rt_txt <- read.csv(inFile, header=F, col.names=headers, skip=1750001, nrows=250000)

markovTable <- as.data.frame(rt_txt)
#
#  LOAD LOOKUP TABLES
#
        inFile <- paste0(getwd(),"/TextData/mc.csv")
        mc <- as.data.frame(read.csv(inFile))
        inFile <- paste0(getwd(),"/TextData/mcType.csv")
        mcType <- as.data.frame(read.csv(inFile))
        inFile <- paste0(getwd(),"/TextData/nw.csv")
        nw <- as.data.frame(read.csv(inFile))
        inFile <- paste0(getwd(),"/TextData/nGramCount.csv")
        nGramCount <- as.data.frame(read.csv(inFile))
#

nextWordProbability <- function(df, sampleSize=nrow(df)) {
        print(paste0(now()," - Starting maxLikelihood calculation with sample size =  ", sampleSize, " tokens & n-grams"))
        
         dValue <- .75                                                        # STANDARD DISCOUNTING
#        print(paste0(now(), "  COMPLETE-Lookup tables built"))
#
#        set.seed(1234)
        dfTable <- df

        pb <- txtProgressBar(min = 0, max = sampleSize, style = 3)
        for(i in 1: nrow(dfTable)) {
                setTxtProgressBar(pb, i)
 #               cat(paste0("\r","Iteration = ", i))

                dfTable$searchCount[i] <- mc$freq[match(dfTable$searchWords[i],mc$searchWords)]
                cat(paste0("\r","Iteration = ", i, "  searchWord count done     "))
                dfTable$mcTypeCount[i] <- mcType$freq[match(dfTable$searchWords[i], mcType$searchWords)]
                cat(paste0("\r","Iteration = ", i, "  searchWord type count done"))
                dfTable$wordMLE[i]     <- dfTable$frequency[i]/dfTable$searchCount[i]
                lookupKey <- paste0(dfTable$nGramCount[i], dfTable$nextWord[i])
                dfTable$nwCount[i]     <- nw$freq[match(lookupKey, nw$searchKey)]
                cat(paste0("\r","Iteration = ", i, "  nextWord count done       "))
                #
                # KNESER-NEY SMOOTHING FACTORS
                #
                dfTable$firstTerm[i] <- max((dfTable$frequency[i] - dValue)/dfTable$searchCount[i], 0)
                dfTable$lambda[i]    <- (dValue/dfTable$searchCount[i]) * dfTable$mcTypeCount[i]
                nGramc <- nGramCount$freq[match(dfTable$nGramCount[i], nGramCount$nGram)]
                dfTable$pCont[i]     <-  dfTable$nwCount[i] / nGramc
                
                dfTable$PKN[i]       <- dfTable$firstTerm[i] + (dfTable$lambda[i] * dfTable$pCont[i])
                cat(paste0("\r","Iteration = ", i, "  probabilities done        "))
        }
        cat("\n")
        print(paste0(now(), " - Calculation Complete"))
        return(dfTable)
        
}

newTable <- nextWordProbability(markovTable)
head(newTable)

write.csv(newTable,outFile, row.names=T)
