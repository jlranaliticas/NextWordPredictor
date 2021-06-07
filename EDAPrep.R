#
library(readtext)
library(reader)
library(tm)
library(plyr)
library(dplyr)

library(MODIS)
library(quanteda)
library(quanteda.corpora)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(grid)
library(gridExtra)
library(e1071)
library(irlba)
library(corpustools)
library(kableExtra)
library(text)
library(textstem)
library(textclean)
library(stringr)
library(lubridate)
library(LaF)

#
#
df <- data.frame(Document=c("News", "Blogs", "Twitter", "TOTAL"),
                 MB=c(196.3, 200.4, 159.4, 556.1),
                 Lines         = c(0,0,0,0),
                 Sample_Pct    = c(0,0,0,0),
                 Sample_Lines  = rep(c(0),4),
                 No_Tokens     = rep(c(0),4),
                 UniqueTokens  = rep(c(0),4),
                 Token_Prep    = rep(c(0),4),
                 Unique_Prep   = rep(c(0),4))

readPreProc <- function(txtSource, useCache=FALSE) {
  
      if(txtSource == "news") {indx <- 1}
        else if(txtSource == "blogs") {indx <-2}
             else if(txtSource == "twitter") {indx <- 3}
                  else {print("Invalid source")
                      return}

      inFile <- paste0(getwd(), "/TextData/en_US.", txtSource,".txt")
      outFile <- paste0(getwd(),"/textData/rt_", txtSource, ".txt")
      df$MB[indx] <- round(fileSize(inFile,units="MB"),1)
      assign("df", df, envir=.GlobalEnv)
      
      if(useCache) {
        print(paste0(now()," ", txtSource,"Using cached file"))
        inFile <- paste0(getwd(), outFile)
        rt_txt <- readtext(inFile)
        return(rt_txt)
      }
      
      print(paste0(now()," ", txtSource,":Reading file"))
      rt_txt <- readtext(inFile)
      print(paste0(now()," ", txtSource,":Begin pre-processing"))
      rt_txt <- str_to_lower(rt_txt)
      print(paste0(now()," ", txtSource,":Begin replace contraction"))
      rt_txt <- replace_contraction(rt_txt, sent.cap=F)
      print(paste0(now()," ", txtSource,":Begin lemmatize strings"))
      rt_txt <- lemmatize_strings(rt_txt)
      print(paste0(now()," ", txtSource,":Lemmatize complete"))
      rt_txt <- str_replace_all(rt_txt,"[^[A-Za-z]]"," ")
      

      print(paste0(now()," ", txtSource,"Writing pre-processing file"))
      write.table(rt_txt,file=outFile,row.names=FALSE)
        
      return(rt_txt)
}

samplePreProc <- function(txtSource, samplePCT=.40, useCache=FALSE) {
  
  set.seed(1234)
  
  if(txtSource == "news") {indx <- 1}
  else if(txtSource == "blogs") {indx <-2}
  else if(txtSource == "twitter") {indx <- 3}
  else {print("Invalid source")
    return}
  
  inFile <- paste0(getwd(), "/TextData/en_US.", txtSource,".txt")
  outFile <- paste0(getwd(),"/textData/rt_", txtSource, ".txt")

  linesInFile <- determine_nlines(inFile)
  sampleSize  <- round(linesInFile * samplePCT,0)
  df$MB[indx] <- round(fileSize(inFile,units="MB"),1)
  df$Lines[indx]        <- linesInFile
  df$Sample_Pct[indx]   <- paste(samplePCT * 100,"%", sep="")
  df$Sample_Lines[indx] <- sampleSize
  assign("df", df, envir=.GlobalEnv)
  
  if(useCache) {
    print(paste0(now()," ", txtSource," : Using cached file"))
    inFile <- outFile
    rt_txt <- readtext(inFile)
  } else {
        print(paste0(now()," ", txtSource,":Sampling file with ", sampleSize, " lines"))
        rt_txt <- sample_lines(inFile, sampleSize)
        print(paste0(now()," ", txtSource,":Begin pre-processing"))
        rt_txt <- str_to_lower(rt_txt)
        print(paste0(now()," ", txtSource,":Begin replace contraction"))
        rt_txt <- replace_contraction(rt_txt, sent.cap=F)
        print(paste0(now()," ", txtSource,":Begin lemmatize strings"))
        rt_txt <- lemmatize_strings(rt_txt)
        print(paste0(now()," ", txtSource,":Lemmatize complete"))
        rt_txt <- str_replace_all(rt_txt,"[^[A-Za-z]]"," ")
        print(paste0(now()," ", txtSource,": Writing pre-processing file"))
        write.table(rt_txt,file=outFile,row.names=FALSE)
    }

  return(rt_txt)
}

buildCorpus <- function(txtSource, txtFile) {
    if(txtSource == "news") {indx <- 1}
        else if(txtSource == "blogs") {indx <-2}
              else if(txtSource == "twitter") {indx <- 3}
                    else if(txtSource == "ALL") {indx <- 4}
                          else {print("Invalid source")
                              return}

    print(paste0(now()," ", txtSource,":Building corpus"))
    source_tc <- create_tcorpus(txtFile)
    print(paste0(now()," ", txtSource,":Corpus Complete.Start Pre-processing"))
    source_tc$preprocess(lowercase=T,
                       remove_punctuation=T,
                       remove_numbers=T,
                       remove_stopwords=T,
                       use_stemming = T)
    
    print(paste0(now()," ", txtSource,": Removing single characters"))
    source_tc$tokens$feature[str_length(source_tc$tokens$feature)==1] <- NA
    
    assign("df", df, envir=.GlobalEnv)
    
    df$No_Tokens[indx]     <- sum(!is.na(source_tc$tokens$token))
    df$UniqueTokens[indx]  <- length(unique(source_tc$tokens$token))
    df$Token_Prep[indx]    <- sum(!is.na(source_tc$tokens$feature))
    df$Unique_Prep[indx]   <- length(unique(source_tc$tokens$feature))
    assign("df", df, envir=.GlobalEnv)
    
#
#    source_tf <- top_features(source_tc,"feature", n=25, return_long=T)
    
    print(paste0(now()," ", txtSource,": Building ngram=2"))
    source_tc$preprocess(column="feature", new_column="feature2", ngrams=2, min_char=2)
    print(paste0(now()," ", txtSource,": Building ngram=3"))
    source_tc$preprocess(column="feature", new_column="feature3", ngrams=3, min_char=2)
    
    print(paste0(now()," ", txtSource,":Corpus Complete"))
    return(source_tc)
    
}

#
# 1. READ AND PRE-PROCESS INDIVIDUAL TEXT FILES
#

print((paste0(now()," Begin NEWS.txt processing")))
news_rt <- samplePreProc(txtSource = "news")
#news_tc <- buildCorpus(txtSource = "news", txtFile=news_rt)
print((paste0(now()," End NEWS.txt processing")))

blogs_rt <- samplePreProc(txtSource = "blogs")
print((paste0(now()," End BLOGS.txt processing")))

print((paste0(now()," Begin TWITTER.txt processing")))
twitter_rt <- samplePreProc(txtSource = "twitter")
#twitter_tc <- buildCorpus(txtSource = "twitter", txtFile=twitter_rt)
print((paste0(now()," End TWITTER.txt processing")))


#
# 2. COMBINE INDIVIDUAL CORPUS INTO MASTER CORPUS
#
makeDF <- function(tsource, fileName) {
  f.df <- as.data.frame(fileName)
#  f.df <- cbind(tsource, f.df)
#  colnames(f.df)[1] <- "source"
#  colnames(f.df)[2] <- "text"
  return(f.df)
}
news_df <- makeDF("news", news_rt)
blogs_df <- makeDF("blogs", blogs_rt)
twitter_df <- makeDF("twitter", twitter_rt)

cc_rt <- rbind(news_df, blogs_df, twitter_df)
      df$Lines[4] <- sum(df$Lines[1:3])
      df$Sample_Lines[4] <- sum(df$Sample_Lines[1:3])
      
cc_tc <- buildCorpus(txtSource="ALL", txtFile=cc_rt)

pctCoverage <- data.frame(pctTok=c(.5, .8, .9, .95),
                          tokCount=rep(c(0),4))
featureList <- top_features(cc_tc,feature="feature",n=df$Token_Prep[4], return_long=T)
featureList <- featureList %>% mutate(pctOfTok = freq/df$Token_Prep[4])
cumulPct <- 0
j <- 1

for (i in 1:nrow(featureList)) {
  cumulPct <- cumulPct + featureList$pctOfTok[i]
  
  if(cumulPct >= pctCoverage$pctTok[j]) {
      pctCoverage$tokCount[j] <- i
      j <- j + 1
  }
  
  if(j > nrow(pctCoverage)) {   ## EXIT IF ALL COVERAGE POINTS COMPLETE
    break()
  }
}

#
# OUTPUT DATA FILES FOR DISPLAY IN R MARKDOWN DOCUMENT
#
colnames(df) <- c("Document", "MB", "Lines","Pct", "No.Lines", 
                  "No.Tokens", "No.Unique", "Tokens", "Unique")

write.csv(df,"xc_summary.csv", row.names=F)             # OUTPUT THE CORPUS SUMMARIES
write.csv(pctCoverage,"TokenCoverage.csv", row.names=F) # OUTPUT KEY COVERAGE


featurePlot <- function(ntf, heading="Top Features") {
                colnames(ntf)[1] <- "feature"
                plottedFeatures <- ntf %>% 
                        ggplot(aes(x=reorder(feature,freq), y=freq)) + 
                        theme(axis.text.x=element_text(angle=90,hjust=1)) +
                        geom_point() +
                        coord_flip() +
                        ggtitle(heading)
        return(plottedFeatures)
}

ntf <- top_features(news_tc,feature="feature",n=20, return_long=T)
news_tokplot <- featurePlot(ntf, heading="News")

ntf <- top_features(blogs_tc,feature="feature",n=20, return_long=T)
blog_tokplot <- featurePlot(ntf,"Blogs")

ntf <- top_features(twitter_tc,feature="feature",n=20, return_long=T)
twitter_tokplot <- featurePlot(ntf,heading="Twitter")

ntf <- top_features(cc_tc,feature="feature", n=20, return_long=T)
cc_plot <- featurePlot(ntf,"ALL Sources")

topFeaturePlot <- grid.arrange(news_tokplot, blog_tokplot, twitter_tokplot, cc_plot, ncol=4, 
             top=textGrob("Most Frequent Words by Source", 
                          gp=gpar(fontsize=16)))

ggplot2::ggsave("images/sourceTopFeaturePlot.png", topFeaturePlot, width=7, height=9)


# ggplot2::ggsave("images/allTopFeaturePlot.png", cc_plot, width=6, height=8)

#
# BUILD LIST OF N-GRAMS
#
# Bi(2) - Grams
      ntf <- top_features(news_tc,feature="feature2",n=20, return_long=T)
      news_tokplot <- featurePlot(ntf,"News")
      ntf <- top_features(blogs_tc,feature="feature2",n=20, return_long=T)
      blog_tokplot <- featurePlot(ntf,"Blogs")
      ntf <- top_features(twitter_tc,feature="feature2",n=20, return_long=T)
      twitter_tokplot <- featurePlot(ntf,heading="Twitter")

      topnGramPlot <- grid.arrange(news_tokplot, blog_tokplot, twitter_tokplot, ncol=3, 
                               top=textGrob("Most Frequent 2-Grams by Text Source", 
                                            gp=gpar(fontsize=16)))
      ggplot2::ggsave("images/2gramPlot.png", topnGramPlot)

# Tri(3) - Grams
      ntf <- top_features(news_tc,feature="feature3",n=20, return_long=T)
      news_tokplot <- featurePlot(ntf,"News")
      ntf <- top_features(blogs_tc,feature="feature3",n=20, return_long=T)
      blog_tokplot <- featurePlot(ntf,"Blogs")
      ntf <- top_features(twitter_tc,feature="feature3",n=20, return_long=T)
      twitter_tokplot <- featurePlot(ntf,heading="Twitter")
      

      topnGramPlot <- grid.arrange(news_tokplot, blog_tokplot, twitter_tokplot, ncol=3, 
                             top=textGrob("Most Frequent 3-Grams by Text Source", 
                                          gp=gpar(fontsize=16)))
      ggplot2::ggsave("images/3gramPlot.png", topnGramPlot)
      
# Combined Corpus N-Gram Plots
      
      ntf <- top_features(cc_tc,feature="feature2",n=20, return_long=T)
      xc2_tokplot <- featurePlot(ntf,"2-Grams")
      ggplot2::ggsave("images/2gramPlot.png", xc2_tokplot)
      ntf <- top_features(cc_tc,feature="feature3",n=20, return_long=T)
      xc3_tokplot <- featurePlot(ntf,"3-Grams")
      ggplot2::ggsave("images/3gramPlot.png", topnGramPlot)
      topnGramPlot <- grid.arrange(xc2_tokplot, xc3_tokplot, ncol=2, 
                                   top=textGrob("Most Frequent 2 and 3-Grams", 
                                                gp=gpar(fontsize=16)))
      ggplot2::ggsave("images/topnGramPlot.png", topnGramPlot)
      