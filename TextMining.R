install.packages("tm")
install.packages("plyr")
install.packages("class")

library(tm)
library(plyr)
library(class)
libs=c("tm","plyr","class")
lapply(libs,require,character.only = T)  

options(stringsAsFactors= F)
cat = c("obama","romney")
pathname = "C:\\Users\\Ogr-1\\Desktop\\speeches\\"
cleanCorpus <-function(corpus) {
       corpus.tmp = tm_map(corpus,removePunctuation)
       corpus.tmp = tm_map(corpus.tmp,removePunctuation)
       corpus.tmp = tm_map(corpus.tmp,stripWhitespace)
       corpus.tmp = tm_map(corpus.tmp,content_transformer(tolower))
       corpus.tmp = tm_map(corpus.tmp,removeWords,stopwords("english"))
      return(corpus.tmp)
}

generateTDM <- function(cat,pathname) {
      s.dir <- sprintf("%s%s", pathname, cat)
      s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF-8"))
      s.cor.cl <- cleanCorpus(s.cor)
      s.tdm <- TermDocumentMatrix(s.cor.cl)
      s.tdm <- removeSparseTerms(s.tdm,0.7)
      result <- list(name=cat, tdm=s.tdm)
   }

tdm = lapply(cat,generateTDM,path = pathname)
str(tdm)

bindCandidatetoTDM <- function(tdm){ 
  s.mat <- t(data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
  } 

candTDM <- lapply(tdm, bindCandidatetoTDM)
str(candTDM)

tdm.stack <- do.call(rbind.fill , candTDM)
tdm.stack[is.na(tdm.stack)] <- 0  
head(tdm.stack)

train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack))[-train.idx]

tdm.cand <- tdm.stack[, "targetcandidate"]
tdm.stack.nl <- tdm.stack[, !colnames(tdm.stack) %in% "targetcandidate"]
knn.pred <- knn(tdm.stack.nl[train.idx, ], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])

conf.mat <- table("Predictions" = knn.pred , Actual = tdm.cand[test.idx]) 
accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100 
accuracy