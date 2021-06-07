#
# NEXT WORD IS ... Application Server
#
# This module implements a probabilistic model (using Markov chain concepts) to 
# "suggest" the next word in a phrase/sentence.
#



shinyServer(function(input, output) {

# nextWordIs Function is the workhorse which searches the markovTable to find the
# most likely next word.
# The routine uses a "backoff" routine which starts at the highest n-gram level 
# and reduces if no match is found
#
    nextWordIs <- function(userText) {

      
# 1. FIND THE NUMBER OF WORDS IN THE INPUT STRING TO USE AS THE SEARCH TERMS
#    - START FROM BACK OF THE INPUT STRING (MOST RECENTLY ENTERED)
#
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
        
        nextWord <- data.frame("word" = character(0), "PKN"= numeric(0))   ######

# 2. ITERATE THROUGH THE SEARCH STRING UNTIL EITHER A MATCH IS FOUND OR END OF STRING
        
        while (searchingForMatch) {

             srchPattern <- word(ut,startWord, endWord)
            
            # APPROACH EXTRACTING FROM NORMALIZED DATA TABLE (ZERO VALUES REMOVED)
            

             suggestions <- markovTable[.(srchPattern), nomatch=0L]

            if(nrow(suggestions) == 0) {
                startWord <- startWord + 1
            }else {
                suggestions <- subset(suggestions, select=c("nextWord", "PKN"))
                suggestions <- suggestions[order(-suggestions$PKN),]
 
                nextWord    <- na.omit(suggestions[1:3,])    ## GET HIGHEST VALUE
                
                for (i in 1:nrow(nextWord)) {

                    nextWord[i,1] <- paste("<font color=\"gray98\">", input$userText, "</font>",
                                           "<font color=\"blue\"> <strong>", nextWord[i,1],"</strong></font>")

                }
                rownames(nextWord) <- NULL
                colnames(nextWord) <- c("Suggested next word is ...", "Frequency")
                searchingForMatch <- FALSE

                return(nextWord)
            }
            if(startWord > endWord) {
                searchingForMatch <- FALSE
                nextWord[1,1] <- paste("<font color=\"gray98\">", input$userText, "</font>",
                                       "<font color=\"blue\"> <strong>", 
                                       markovTable$nextWord[which.max(markovTable$nwCount)], 
                                       "</strong></font>")                 
                nextWord[1,2] <- markovTable$PKN[which.max(markovTable$nwCount)]
                rownames(nextWord) <- NULL
                colnames(nextWord) <- c("Suggested next word is ...", "Frequency")
                return(nextWord)
                
            }
        }
    }
#
#  cleanInput function normalizes the user input string to be consistent with 
#  the structure of the markovTable.
#
    cleanInput <- function(userText) {
        cleanText <- tolower(userText)
        cleanText <- replace_contraction(cleanText, sent.cap=F)
        cleanText <- lemmatize_strings(cleanText)

        return(cleanText)
    }
    

   output$nextWordList <- renderFormattable({
                               userText <- cleanInput(input$userText)
                               nextWordList <- as.data.frame(nextWordIs(userText))
                               formattable(nextWordList,
                                           align="l",
                                           list("Frequency"=percent))})
    output$howToUse <- renderImage({
                        imgName <- normalizePath(file.path("./www/howToUse.png"))
                        list(src=imgName, width="100%")
    },
    
    deleteFile=FALSE)
    

    output$aboutNextWord <- renderImage({
      imgName <- normalizePath(file.path("./www/aboutNextWordIs.png"))
      list(src=imgName, width="100%")
    },
    
    deleteFile=FALSE)
                        

})
