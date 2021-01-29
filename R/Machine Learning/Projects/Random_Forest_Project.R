# Author: Carlos Heredia Pimienta
# Date: 20/01/20

########################################################
#                WORKSPACE  PREPARATION                #
########################################################

#Clean workspace
rm(list=ls())

########################################################
#                        LIBRARY                       #
########################################################

library(dplyr)
library(corrgram)
library(ggpubr)
library(randomForest)
library(rfPermute)
library(class)
library(ggplot2)
library(psych)
library(caret)
library(PCAmixdata)
library(skimr)
library(pls)

########################################################
#                      FUNCTIONS                       #
########################################################

read <- function(){
  csv <- read.csv(-- Hidden --,sep=";")
  return(csv)
}

del.field <- function(df,field){
  del.df <- (df %>% select(-field))
  return(del.df)
}


to.numeric <- function(df){
  df.factor <- names(df %>% select_if(is.factor))
  df[,df.factor] <- sapply(df[,df.factor],as.numeric)
  return(df)
}

rf.stat <- function(df.trainset){
  rf <-rfPermute(variable~., data = df.trainset, ntree = 200, 
                 na.action = na.omit, nrep = 50, num.cores = 2)
  return(rf)
}

df.results <- function(ml.model, ml.predict, ml.training){
  predict <- mean(ml.predict == ml.training[,1])
  print(paste('La precisi?n es:', predict))
  print('La tabla de confusi?n es:')
  print(table(ml.predict, ml.training[,1]))
  print('Informaci?n de cada variable:')
  print(importance(ml.model))
  print(varImpPlot(ml.model))
}

ml.knn.str <- function(df){
  variable <- df[,ncol(df)]
  df.standarized <- scale(df[,-ncol(df)])
  list.result <- list(variable, df.standarized)
  return(list.result)
}

ml.knn <- function(train.data, test.data, train.status, test.status, nk){
  predicted.status <- NULL
  error.rate <- NULL
  for(i in 1:nk){
    set.seed(101)
    predicted.status <- knn(train.data,test.data,train.status,k=i)
    error.rate[i] <- mean(test.status != predicted.status)
  }
  return(error.rate)
} 

graph.knn <- function(error.rate,nk){
  ks <- 1:nk
  error.df <- data.frame(error.rate,ks)
  ggplot(error.df,aes(x=ks,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
}

corr.str <- function(df){
  dmy <- dummyVars(" ~ .", data = df)
  df.trsf <- data.frame(predict(dmy, newdata = df))
  return(df.trsf)
}

cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

plot.corr <- function(df.real,df.transf){
  bestSub <-  sapply(strsplit(as.character(df.transf$j),'[.]'), "[", 1)
  pairs.panels(df.real[c(bestSub,-- Hidden --)])
}

PCA.MIX <- function(df, n = 3 ){
  split <- splitmix(df)
  X1 <- split$X.quanti
  X2 <- split$X.quali
  res.pcamix <- PCAmix(X.quanti = X1,X.quali = X2,
                       rename.level = TRUE, graph =FALSE)
  print(res.pcamix$eig)
  if(n == 1) {
    plot(res.pcamix, choice = 'ind', coloring.ind = X2, label = FALSE,
         posleg = "bottomright", main = "(a) Observation")
  } else if (n == 2) {
    plot(res.pcamix, choice = 'cor', main = '(c) Numerical variables')
  } else if (n == 3){
    plot(res.pcamix, choice = "sqload", coloring.var = T, leg = TRUE,
         posleg = "topright", main = '(d) All variables')
  }  
}

#######################################################
#                       BODY                          #
#######################################################


df <- read()
df.set1 <- df %>% filter(variable == -- Hidden --) 
df.set2 <- df %>% filter(variable == -- Hidden --)
df.set2 <- sample_n(df.set2, nrow(df.set1))
df <- rbind(df.set1,df.set2)

df$variable <- as.factor(df$variable)
df$variable <- as.factor(df$variable)


#######################################################
#                 BODY - ANALYSI                      #
#######################################################


skim(df)


#######################################################
#               BODY - PCA ANALYSIS                   #
#######################################################

PCA.MIX(df)

#######################################################
#          BODY - CORRELATION ANALYSIS                #
#######################################################


df.num <- corr.str(df)
corMasterList <- flattenSquareMatrix(cor.prob(df.num))
corList <- corMasterList[order(corMasterList$cor),]
selectedsub1 <- subset(corList, (abs(cor) > 0.2 & i == -- Hidden -- & j != -- Hidden --))
selectedsub2 <- subset(corList, (abs(cor) > 0.2 & i == -- Hidden -- & j != -- Hidden --))
#plot.corr(df,selectedSub.con)


#######################################################
#                 BODY - PLR ANALYSIS                 #
#######################################################

df.plr <- corr.str(df[,-1])
df.plr$variable <- df[,1]

# Compile cross-validation settings
set.seed(100)
myfolds <- createMultiFolds(df.plr$variable, k = 5, times = 10)
control <- trainControl("repeatedcv", index = myfolds, selectionFunction = "oneSE")
# Train PLS model
mod1 <- train(variable ~ ., data = df.plr,
              method = "pls",
              metric = "Accuracy",
              tuneLength = 20,
              trControl = control,
              preProc = c("zv","center","scale"))

# Check CV profile
plot(mod1)
plot(varImp(mod1), 10, main = "PLS-DA")


#######################################################
#             BODY - SPLIT DATASETS                   #
#######################################################

# Para el Random Forest
set.seed(100)
df.train <- sample(nrow(df), 0.7*nrow(df), replace = FALSE)
df.trainset <- df[df.train,]
df.validset <- df[-df.train,]

# Para el KNN:
df.num.knn <- corr.str(df[,-1])
df.num.knn$variable <- df[,1]
df.trainset.num <- df.num.knn[!is.na(df.train),]
df.validset.num <- df.num.knn[!is.na(-df.train),]
df.train.knn <- ml.knn.str(df.trainset.num)
df.validset.knn <- ml.knn.str(df.validset.num)


#######################################################
#                BODY - RANDOM FOREST                 #
#######################################################


df.model <- randomForest(variable~., data = df.trainset, importance = TRUE)
df.predict <- predict(df.model, df.trainset, type ="class")
df.validate <- predict(df.model,df.validset, type = "class")

df.results(df.model, df.predict, df.trainset)


#######################################################
#                    BODY - KNN                       #
#######################################################


df.knn <- ml.knn(df.train.knn[[2]], df.validset.knn[[2]], df.train.knn[[1]], df.validset.knn[[1]],20)

graph.knn(df.knn,20)
