#
# COMBINE INDIVIDUAL MARKOV TABLES INTO ONE
#
# REQUIREMENTS:
#    PROCEDURE '3_updateProbabilities.R' MUST HAVE RUN TO BUILD LOOKUP TABLES
# 1. FOR EACH FILE OUTPUT FROM '3_updateProbabilities.R'
#    A. CREATE A 'read.csv' AND "tn" STATEMENT
#    B. UPDATE THE FILE NAME IN EACH 'read.csv' STATEMENT
# 2. UPDATE THE 'outFile' STATEMENT WITH THE CORRECT OUTPUT FILE NAME
#       
library(dplyr)
library(readtext)
library(reader)
library(stringr)
library(lubridate)
library(data.table)
library(tidyr)


#  inFile <- paste0(getwd(),"/TextData/markovTable_40PCT.csv")
rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_0000001.csv"))
t1 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_250001.csv"))
t2 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_500001.csv"))
t3 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_750001.csv"))
t4 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_1000001.csv"))
t5 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_1250001.csv"))
t6 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_1500001.csv"))
t7 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_1750001.csv"))
t8 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_2000001.csv"))
t9 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_2250001.csv"))
t10 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_2500001.csv"))
t11 <- as.data.frame(rt_txt)

rt_txt <- read.csv(paste0(getwd(),"/TextData/markovTable_2750001.csv"))
t12 <- as.data.frame(rt_txt)



markovTable <- rbind(t1, t2, t3, t4, t5, t6, t7,
                     t8, t9, t10, t11, t12)
head(markovTable)

outFile <- paste0(getwd(),"/TextData/markovTable_40PCT_FINAL.csv")
write.csv(markovTable,outFile, row.names=T)



