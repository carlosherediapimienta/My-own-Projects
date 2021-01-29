# Author: Carlos Heredia Pimienta
# Date: 30/04/20
# Explanation: How to implement Shannon Entropy in R

library(entropy)
library(dplyr)

########################################################
#                  ENTROPY-SHANNON                     #
########################################################


#compute Shannon entropy
entropy <- function(target) {
  freq <- table(target)/length(target)
  # vectorize
  vec <- as.data.frame(freq)[,2]
  #drop 0 to avoid NaN resulting from log2
  vec<-vec[vec>0]
  #compute entropy
  -sum(vec * log2(vec))
}

#returns IG for numerical variables.
IG_numeric<-function(data, feature, target, bins=4) {
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),]
  #compute entropy for the parent
  e0<-entropy(data[,target])
  
  data$cat<-cut(data[,feature], breaks=bins, labels=c(1:bins))
  
  #use dplyr to compute e and p for each value of the feature
  dd_data <- data %>% group_by(cat) %>% summarise(e=entropy(get(target)), 
                                                  n=length(get(target)),
                                                  min=min(get(feature)),
                                                  max=max(get(feature))
  )
  
  #calculate p for each value of feature
  dd_data$p<-dd_data$n/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}

#returns IG for categorical variables.
IG_cat<-function(data,feature,target){
  #Strip out rows where feature is NA
  data<-data[!is.na(data[,feature]),] 
  #use dplyr to compute e and p for each value of the feature
  dd_data <- data %>% group_by_at(feature) %>% summarise(e=entropy(get(target)), 
                                                         n=length(get(target))
  )
  
  #compute entropy for the parent
  e0<-entropy(data[,target])
  #calculate p for each value of feature
  dd_data$p<-dd_data$n/nrow(data)
  #compute IG
  IG<-e0-sum(dd_data$p*dd_data$e)
  
  return(IG)
}


# Entropy == 0
setosa_subset <- iris[iris$Species=="setosa",]
entropy(setosa_subset$Species)

# Entropy != 0 
entropy(iris$Species)


# Entropy for each variable
col_name<-character()
ig<-numeric()
features<-names(iris)
for (i in 1:4){
  col_name[i]<-names(iris)[i]
  ig[i]<-IG_numeric(iris, names(iris)[i], "Species", bins=5)
}
ig_df<-cbind(col_name, round(ig,2))

# Que variable aporta m?s informaci?n?
data<-iris
data$cat<-cut(data[,"Petal.Width"], breaks=5, labels=c(1:5))
dd_data <- data %>% group_by(cat) %>% summarise(e=entropy(Petal.Width), 
                                                n=length(Petal.Width), 
                                                min=min(Petal.Width), 
                                                max=max(Petal.Width),
                                                Setosa=length(Species[Species=="setosa"])/50,
                                                Versicolor=length(Species[Species=="versicolor"])/50,
                                                Virginica=length(Species[Species=="virginica"])/50)
dd_data<- data.frame(dd_data)
dd_data


# Entropy for each variable
col_name<-character()
ig<-numeric()
features<-names(airquality)
for (i in 1:6){
  col_name[i]<-names(airquality)[i]
  ig[i]<-IG_cat(airquality, names(airquality)[i], "Temp")
}
ig_df<-cbind(col_name, round(ig,2))
