library(dplyr)
library(readtext)
library(reader)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)

inFile <- paste0(getwd(),"/TextData/markovTable_40PCT_FINAL.csv")
rt_txt <- fread(inFile, sep=",", header=T)

markovTable <- as.data.table(rt_txt)

str(markovTable)

## markovTable <- subset(markovTable, frequency != 2)

markovReduced <- subset(markovTable, select = -c(X.1, feature, searchCount, mcTypeCount, firstTerm, lambda, pCont))
str(markovReduced)

outFile <- paste0(getwd(),"/TextData/markovTable_40PCT_REDUCED.csv")
write.csv(markovReduced,outFile, row.names=F)