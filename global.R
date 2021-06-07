#
# global.R - the purpose of this R script is to manage reading of data files

library(shiny)
library(shinyWidgets)
library(textclean)
library(textstem)

library(stringr)

library(data.table)
library(formattable)

library(qdap)

inFile <- paste0(getwd(),"/markovTable_40PCT_REDUCED.csv")
rt_txt <- fread(inFile, sep=",", header=T)

markovTable <- as.data.table(rt_txt)
setkey(markovTable,searchWords)

maxNgramCount <- max(markovTable$nGramCount) - 1  ## THIS IS THE MAX SIZE OF SEARCH WORDS

rm(list="rt_txt")