# Author: Carlos Heredia Pimienta
# Date: 23/03/2020

########################################################
#                WORKSPACE  PREPARATION                #
########################################################

#Clean workspace
rm(list=ls())

########################################################
#                        LIBRARY                       #
########################################################

library(dplyr)
library(skimr)
library(PCAmixdata)
library(caret)

########################################################
#                     FUNCTIONS                        #
########################################################

# Lectura del csv.
df.project <- function(){
  csv <- read.csv("---- hiden ----",stringsAsFactors = TRUE, sep=",")
  return(csv)
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

# Transformation categorical variables -> Numerical variables.
corr.str <- function(df){
  dmy <- dummyVars(" ~ .", data = df)
  df.trsf <- data.frame(predict(dmy, newdata = df))
  return(df.trsf)
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

cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

#######################################################
#                       BODY                          #
#######################################################

df <- df.project()

df <- df %>% filter ( -- Hiden --) %>% 
             filter ( -- Hiden -- )

df <- df %>% select(-c(-- Hiden --))

df <- na.omit(df)

#######################################################
#                 BODY - ANALYSI                      #
#######################################################

skim(df)


#######################################################
#          BODY - CORRELATION ANALYSIS                #
#######################################################


df.corr <- select(df,-'-- Hiden --')
df.num <- corr.str(df.corr)

## Delete variables sd -> 0
df.num <- df.num[,-nearZeroVar(df.num)]
df.num <- as.matrix(scale(df.num))
corMasterList <- flattenSquareMatrix(cor.prob(df.num))
corList <- corMasterList[order(corMasterList$cor),]
selectedSub.con <- subset(corList, (abs(cor) > 0.2 ))

df <- select(df, -c(-- Hiden --))


#######################################################
#               BODY - PCA ANALYSIS                   #
#######################################################


df.pca <- select(df,-'-- Hiden --')
PCA.MIX(df.pca)

df <- select(df, c(-- Hiden --))


#######################################################
#                    BODY - KMEANS                    #
#######################################################

set.seed(123)

# Compute and plot wss for k = 2 to k = 15.
k.max <- 15

df.kmean <- corr.str(df[,-1])
df.kmean <- df.kmean[,-nearZeroVar(df.kmean)]
df.kmean <- as.matrix(scale(df.kmean))


#######################################################
#                  BODY - KMEANS ELBOW                #
#######################################################


wss <- sapply(1:k.max,function(k){kmeans(df.kmean, k, nstart=50,iter.max = 15)$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#######################################################
#               BODY - KMEANS HIERARCHY               #
#######################################################


clusters <- hclust(dist(df.kmean))
plot(clusters)


#######################################################
#                    BODY - KMEANS H                  #
#######################################################

set.seed(20)
clusters <- kmeans(df.kmean, 4)
df$cluster <- as.factor(clusters$cluster)
