#
# NEXT WORD PREDICTION MODEL
#
# 1. READ IN PRE-PROCESSED CORPUS (WITH N-GRAMS, FREQUENCIES, AND LOOK-UP VALUES)
# 2. EXECUTE PREDICTION MODEL WITH INPUT
#

library(textclean)
library(textstem)
library(readtext)
#library(markovchain)
library(stringr)
#library(quanteda)
#library(quanteda.corpora)
#library(quanteda.textmodels)
#library(quanteda.textplots)
#library(quanteda.textstats)
library(lubridate)
library(dplyr)
library(data.table)
library(tidyr)
library(qdap)
#library(doSNOW)


inFile <- paste0(getwd(),"/TextData/markovTable_40PCT_FINAL.csv")
rt_txt <- fread(inFile, sep=",", header=T)

markovTable <- as.data.table(rt_txt)
setkey(markovTable,searchWords)


#
# length of user text will not exceed maximum ngram length (n)
#       if # user words, greater than ngram length, then use last n words
# 1. check word to see if it's in the model.
#       if yes, then continue
#       if no, accept next input word
# 1. get highest priority words
# 2. get take last token from predicted ngram
        
maxNgramCount <- max(markovTable$nGramCount) - 1 
# maxNgramCount <- 4

nextWordIs <- function(userText) {
        ut <- str_split(userText," ", simplify=T)
        indx <- 1
        if(length(ut) > maxNgramCount) {
                indx <- length(ut) - maxNgramCount + 1
        }
        ut <- ut[indx:length(ut)]
        ut <- paste(ut, collapse=" ")
        
        startWord <- 1
        endWord   <- sapply(strsplit(ut," "), length)
        searchingForMatch <- TRUE
        
        nextWord <- data.frame("word" = character(0), "PKN"= numeric(0))
        row.names(nextWord) <- NULL
        
        while (searchingForMatch) {
 #               srchPattern <- paste0("^",word(ut,startWord, endWord),"$")
                srchPattern <- word(ut,startWord, endWord)
        
         print(paste(".... searching for=", startWord, " ", srchPattern))
        #
        # APPROACH EXTRACTING FROM NORMALIZED DATA TABLE (ZERO VALUES REMOVED)

 #       suggestions <- filter(markovTable, grepl(srchPattern,searchWords))
 #        print(paste(">>>>>>> SUGGESTIONS: ", suggestions$nextWord[1:3]))
         suggestions <- markovTable[.(srchPattern), nomatch=0L]

        if(nrow(suggestions) == 0) {
                startWord <- startWord + 1
        }else {

                suggestions <- subset(suggestions, select=c("nextWord", "PKN"))
                suggestions <- suggestions[order(-suggestions$PKN),]
 #               print(paste("suggestions >>> ", suggestions))
                nextWord    <- na.omit(suggestions[1:3,])    ## GET HIGHEST VALUE
 #               print(paste("nextWord == ", nextWord))
  #              print(paste("class == ", class(nextWord)))
                for (i in 1:nrow(nextWord)) {
                        nextWord[i,1] <- paste(userText, "<strong>", nextWord[i,1],
                                               "</strong>")
                }
                
                rownames(nextWord) <- NULL
                searchingForMatch <- FALSE
                return(nextWord)
        }
        if(startWord > endWord) {
                searchingForMatch <- FALSE
                altWord <- markovTable$nextWord[which.max(markovTable$nwCount)]
                return(altWord)
                
        }
}
}

cleanInput <- function(userText) {
        cleanText <- tolower(userText)
        cleanText <- replace_contraction(cleanText, sent.cap=F)
        cleanText <- lemmatize_strings(cleanText)
#        cleanText <- rm_stopwords(cleanText, strip=T, apostrophe.remove = T, separate=F)
        
#        print(paste("userText=", userText, " is now", cleanText))
        return(cleanText)
}

for (utIndx in 1:10) {

        userText <- testText_2[utIndx,]
        userText <- cleanInput(userText)
        nextWords <- nextWordIs(userText)
        cat(paste("\nSuggested word is ... '", nextWords[1], "'\n", sep=))
        cat(paste("Other possibilities are ...'", nextWords[2], "' & '", nextWords[3], "'\n", sep=""))
        
}

userText <- "the same as"
userText <- cleanInput(userText)

nextWords <- nextWordIs(userText)
print(nextWords)
print(rownames(nextWords))
cat(paste("\nSuggested word is ... '", nextWords, "'\n", sep=))
cat(paste("Other possibilities are ...'", nextWords[2,], "' & '", nextWords[3,], "'\n", sep=""))













#
#  TEST DATA FROM QUIZ 1
#
testText_1 <- data.frame(matrix(NA,nrow=10,ncol=1))

testText_1[1,] <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"                         # BEER
testText_1[2,] <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"                   # WORLD 
testText_1[3,] <- "Hey sunshine, can you follow me and make me the"                                                       # HAPPIEST
testText_1[4,] <- "Very early observations on the Bills game: Offense still struggling but the"                           # DEFENSE
testText_1[5,] <- "Go on a romantic date at the"                                                                          # MOVIES - missed
testText_1[6,] <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"    # WAY
testText_1[7,] <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"                   # TIME
testText_1[8,] <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"      # FINGERS
testText_1[9,] <- "Be grateful for the good times and keep the faith during the"                                          # BAD
testText_1[10,] <- "If this isn't the cutest thing you've ever seen, then you must be"                                    # INSANE

for (i in 1:10) {
        testText_1[i,] <- cleanInput(testText_1[i,])

}

#
#  TEST DATA FROM QUIZ 2
#
testText_2 <- data.frame(matrix(NA,nrow=10,ncol=1))

testText_2[1,] <- "When you breathe, I want to be the air for you. Ill be there for you, I'd live and I'd"                              # DIE 
testText_2[2,] <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"   # MARITAL 
testText_2[3,] <- "I'd give anything to see arctic monkeys this"                                                                        # WEEKEND
testText_2[4,] <- "Talking to your mom has the same effect as a hug and helps reduce your"                                              # STRESS
testText_2[5,] <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"                            # PICTURE 
testText_2[6,] <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"                 # MATTER
testText_2[7,] <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"              # ARM/HAND
testText_2[8,] <- "Every inch of you is perfect from the bottom to the"                                                                 # TOP
testText_2[9,] <- "I’m thankful my childhood was filled with imagination and bruises from playing"                                      # OUTSIDE
testText_2[10,] <- "I like how the same people are in almost all of Adam Sandler's"                                                     # MOVIES

for (i in 1:10) {
        testText_2[i,] <- cleanInput(testText_2[i,])
        
}






#
# APPROACH EXTRACTING FROM MARKOV TRANSITION MATRIX DIRECTLY
#
# xx <- markovchain::markovchainSequence(n=1, markovchain=tm$estimate,t0=ut, include.t0=F)
#suggest <- tm$estimate[userText,] %>% sort(decreasing = T) %>% head(3)
#suggest <- suggest[suggest >0] %>% names() %>% unlist() %>% tail(1)
#suggest <- str_split(suggest, pattern=" ", simplify=T)
#nextWord <- suggest[length(suggest)]
#
#nextWord
