#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Capstone Project for Data Science
# Wanda Ken 07/07/2021

library(shiny)
library(lubridate)

library(ggplot2)

library(tokenizers)
library(tm)
library(DT)
library(qdap)
library(pryr)

load("quadgram.RData");
load("trigram.RData");
load("bigram.RData");
load("unigram.RData");


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

output$distPlot <- renderPlot({

  txt <- input$inputText

  txt <- removeNumbers(txt)      
  txt <- replace_contraction(txt)
  txt <- removePunctuation(txt)
  txt <- tolower(txt)
  txt <- stripWhitespace(txt)
  
  cnt <- count_words(txt)
  d <- as.data.frame(strsplit(txt, " "))
  colnames(d) <- c("word")
  n <- NROW(d)
  
  exclWordExist <- function(l,u) {
    if (NROW(u) > 0 && NROW(l) > 0){
      ## exclude next words from lower level gram that are already in upper level gram
      l <- l[!(l$nextwd %in% as.vector(u[,4])),]
    }
    return(l)
  }

  nGram <- function(opt,n,s,d,d3,d2,lamda) {
    
    ## string starting with ^ and ending with " ", Expected : "are you "
    x <- paste0("^",s," ")
    
    ## match last 2 words
    t <- d3[grep(x,d3[,1]),]
    
    if (NROW(t) > 0) {
      
      colnames(t) <- c("ngram","frequency")
      t$cat <- c(opt)
      t$nextwd  <- gsub(x,"",t$ngram)
      
      ## exact string "are you", otherwise "are you" get "are your" or "share your" 
      x <- paste0("^",s,"$")
      
      ## find bigram frequency d2 column 2, of "are you"
      tot <- d2[grep(x,d2[,1]),2]
      
      if (length(tot) > 0) {
        ## calculate score based on bigram frequency
        t$score <- round(lamda*t$frequency/tot,6)
        t$tot <- tot
        rm(tot)
      } 
      else { 
        t$tot <- 0 
        t$score <- 0
      }
      
      ## exclude words in the input text
      t <- t[!(t$nextwd %in% as.vector(d[,1])),]
    }
    
    rm(x)
    
    return(t)
  }
  
  uniGram <- function(d1) {
    
    u <- d1[1:10,]
    colnames(u) <- c("ngram","frequency")
    u$cat <- "Unigram"
    u$nextwd  <- u$ngram 
    u$score <- round(u$frequency/sum(d1[,2]),6)
    u$tot <- sum(d1[,2])
    
    return(u) 
  }
  
  ## main 
  nextword <- function(n,d,d1,d2,d3,d4){
    
    if (n>0) {
      if (n > 2) {
        ## get last 3 words - "how are you"
        s <- paste(d[n-2,1],d[n-1,1],d[n,1])
        t4 <- nGram("Quadgram",n,s,d,d4,d3,1)
        s <- paste(d[n-1,1],d[n,1])
        t3 <- nGram("Trigram",n,s,d,d3,d2,0.4)
        s <- d[n,1]
        t2 <- nGram("Bigram",n,s,d,d2,d1,0.4*0.4)
        t3 <- exclWordExist(t3,t4)
        t2 <- exclWordExist(t2,t4)
        t2 <- exclWordExist(t2,t3)
        t <- rbind(t2,t3,t4)
        if (NROW(t) < 10) {
          t1 <- uniGram(d1)
          t1 <- exclWordExist(t1,t2)
          t1 <- exclWordExist(t1,t3)
          t1 <- exclWordExist(t1,t4)
          t <- rbind(t,t1)
          rm(t1)
        }
        rm(s,t4,t3,t2)
      }  
      else if (n == 2) 
      {  
        ## get last 2 words - "are you"
        s <- paste(d[n-1,1],d[n,1])
        t3 <- nGram("Trigram",n,s,d,d3,d2,1)
        s <- d[n,1]
        t2 <- nGram("Bigram",n,s,d,d2,d1,0.4)
        t2 <- exclWordExist(t2,t3)
        t <- rbind(t2,t3)
        if (NROW(t) < 10) {
          t1 <- uniGram(d1)
          t1 <- exclWordExist(t1,t2)
          t1 <- exclWordExist(t1,t3)
          t <- rbind(t,t1)
          rm(t1)
        }
        rm(s,t2,t3)        
      }
      else if ( n == 1)
      {
        s <- d[n,1]
        t2 <- nGram("BiGram",n,s,d,d2,d1,1)
        if (NROW(t2) < 10) {
          t1 <- uniGram(d1)
          t1 <- exclWordExist(t1,t2)
          t <- rbind(t2,t1)
          rm(t1)
        }
        else { t <- t2 }
        rm(s,t2)
      }
      
      t <- t[order(t[,"score"], decreasing=TRUE),]
      t <- t[1:10,]
       
      
      p<-ggplot(data=t, aes(y=score, x=reorder(nextwd,-score), fill=cat)) +
        geom_bar(stat="identity", width = 0.8, position=position_dodge(width = 0.9) ) +
        geom_text(aes(label=round(score,4)), vjust=0, color="black", size=3.2) +
        labs(title="Top 10 next word prediction", x="Next Word ", y = "Word Score", fill="") +
        theme(text = element_text(size=16), axis.text.x=element_text(angle=45,hjust=1),
              plot.title = element_text(hjust = 0.5, size=30))  
      print(p)
  
      rm(p)
      
      return(t)  
    }
      
  }

  nw <- nextword(n,d,d1,d2,d3,d4)

  gc()

  output$tabText <- renderText ({ 
    out <-  ifelse(n==0," ","Tabular Representation of Top 10 next word scores") 
    
  })  
  
  output$mytable = renderDataTable({
    if (n>0) {
      colnames(nw) <- c("NGram Retrieved","Frequency","Category","Next Word","Score")
      nw[,1:5]}
    else {
      nw <- c()}
  })
  
   output$dispText <- renderText ({ 
      out <-  ifelse(n==0," ",paste(" The next predicted word is : ",nw[1,4])) 
  })  


  output$dispAbout <- renderUI ({
      out0 <- "Capstone Project For the Data Science Specialization."
      out1 <- "This Application was built in R under RStudio and powered by Shinyapps.io."
      out2 <- "The dataset is a sample taken from the SwiftKey Corpus."
      out3 <- "The predictive model is based on the n-gram model using the stupid backoff algorithm."
      out4 <- "V1.00 - July 2021 - Wanda Ken"
      HTML(paste(out1, out2, out3, out4, sep = '<br/><br/>'))
      
  })
  
  output$render <- renderText({
    out <-  ifelse(n==0," ",paste("Last rendered on ",as.character.Date(Sys.time(),"%d-%m-%Y %T"))) 
  })

})

})
