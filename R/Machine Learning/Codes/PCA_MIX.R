# Author: Carlos Heredia Pimienta
# Date: 27/03/20
# Explanation: Implementation of Principal Components Analysis (PCA) to a data set with categorical and ordinal variables.
# Documentation: https://arxiv.org/pdf/1411.4911.pdf

library(PCAmixdata)

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

data("gironde")
