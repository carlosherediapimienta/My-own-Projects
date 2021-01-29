# Read files

df.dispon <- read.csv("-- Hiden --",sep=";")
df.nrsrvs <- read.csv("-- Hiden --",sep=";")
df.festivos <- read.csv("-- Hiden --",sep=";")

library(dplyr)
library(lubridate)
library(ggplot2)
library(caret)
library(corrgram)
library(doParallel)

# Functions
LastDayInMonth <- function(dt)
{
  dt <- (as.character(dt))
  dt <- as.character(as.Date(dt) %m+% variable(1))
  dt <- as.Date(ISOdate(as.numeric(substr(dt, 1, 4)),
                        as.numeric(substr(dt, 6, 7)),
                        1) - days(1))
  return(dt)
}

variable <- function(m){
  if(nchar(m) == 1){
    m <- paste('0',m, sep = '')
  }
  return(m)
}

multiplot <- function(p1,p2,p3,p4, plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(p1,p2,p3,p4), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot_t6 <- function(p1,p2,p3,p4,p5,p6, plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(p1,p2,p3,p4,p5,p6), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Rename
df.set1 <- df.set1 %>% rename(v1 = --Hiden--)
df.set2 <- df.set2 %>% rename(v1 = -- Hiden--)
df.set3 <- df.set3 %>% rename(v2 = -- Hiden --)

# Dates
df.set1$variable <- as.factor(df.set1$variable)
df.set1$variable <- df.set1$variable %>% as.character(df.set1$variable) %>% as.Date(df.set1$variable, format = "%Y%m%d" )
df.set1$variable <- df.set1$variable %>% as.character(df.set1$variable) %>% as.Date(df.set1$variable, format = "%Y%m%d")

df.set2$variable <- as.factor(df.set2$variable)
df.set2$variable <- df.set2$variable %>% as.character() %>%
  sapply(function(x) paste(x, '01', sep=''))
df.set2$variable <- as.Date(df.set2$variable, format = "%Y%m%d")

df.set2$variable <- LastDayInMonth(df.set2$variable)
df.set2 <- df.set2[,-2]

df.set3$variable <- df.set3$variable %>% as.character() %>%
  sapply(function(x) paste(x, '01', sep=''))
df.set3$variable <- as.Date(df.set3$variable, format = "%Y%m%d")
df.set3$variable <- LastDayInMonth(df.set3$variable)
df.set3 <- df.set3[,-1]

# Tables
df.set4 <- merge (df.set2, df.set3, by = c(-- Hiden --))
df <- merge(df.set4, df.set1, by =  -- Hiden --)
df <- df %>% filter(variable <= variable & variable  <= variable)

# Data Frame

df <- df %>% select(variable, variable, variable, variable, variable, variable, variable, variable)
df$variable <- df$variable*df$variable           
df$variable <- df$variable*df$variable

df$variable <- as.character(month(as.POSIXlt(df$variable, format="%y/%m/%d")))
df$variable <- sapply(df$variable, variable)
df$variable <- as.character(year(as.POSIXlt(df$variable, format="%y/%m/%d")))
df$variable <- paste(df$variable, df$variable, sep = '')
df <- df %>% select(variable, variable, variable, variable, variable, variable, variable, variable)

df.group <- df %>% group_by(variable, variable) %>%
  summarise(variable = n_distinct(variable), variable = round(sum(variable)/n_distinct(variable),0),
            variable = round(sum(variable)/n_distinct(variable),0), variable = sum(variable),
            variable = sum(variable), variable = sum(variable))

# Plots

df.graph <- df.group
df.graph$variable <- as.integer(rownames(df.group))

plot1 <- ggplot(df.graph, aes(variable, variable)) +
  geom_line()+
  ggtitle("-- Hiden --")

plot2 <- ggplot(df.graph, aes(variable, variable)) +
  geom_line()+
  ggtitle("-- Hiden --")

plot3 <- ggplot(df.graph, aes(variable, variable)) +
  geom_line()+
  ggtitle("-- Hiden --")

plot4 <- ggplot(df.graph, aes(variable, variable)) +
  geom_line()+
  ggtitle("-- Hiden --")

multiplot(plot1, plot2, plot3, plot4, cols=2)  


# Data Mining
df.ml <- df.graph %>% filter(variable > 10 & variable < 43 )
df.ml.ts <- df.ml
df.ml.ts$variable <- df.ml.ts$variable %>% as.character() %>%
  sapply(function(x) paste(x, '01', sep=''))
df.ml.ts$variable <- as.Date(df.ml.ts$variable, format = "%Y%m%d")


df.ml.train.ts <- df.ml.ts %>% select(variable, variable, variable, variable)  %>% filter(variable < "2018-12-01" & variable > "2017-03-01")
df.ml.train.ts <-  df.ml.train.ts[,-1]

df.ml.predict.ts <- df.ml.ts %>% select(variable, variable, variable )  %>% filter(variable > "2018-11-01")
df.ml.predict.ts <- df.ml.predict.ts[,-1]

real <- df.ml.ts %>% select(variable, variable, variable)  %>% filter(variable > "2018-11-01")
real <- real[,-- Hiden --]

registerDoParallel(cores=2)


########################################################
#                         SVM                          #
########################################################

set.seed(280)


fitControl.svm <- trainControl(method = "timeslice",
                               initialWindow = 4,
                               horizon = 1,
                               search = "random", 
                               fixedWindow = TRUE,
                               allowParallel = TRUE)

model.cv.svm<- train(variable ~., 
                     data = df.ml.train.ts,
                     method = 'svmRadial',
                     tuneLength = 5, 
                     preProcess = c("center", "scale"),
                     trControl = fitControl.svm)

# Predicting 
variable <-predict(model.cv.svm, df.ml.predict.ts)

df.ml.pred.svm <- cbind(df.ml.predict.ts, variable)
Error.svm <- data.frame(variable, real$variable)
Error.svm$Error <- (abs(real$variable - variable)/real$variable)*100 
RMSE(variable, real$variable)
MAE(variable, real$variable)

pd <- position_dodge(0.3)
ggplot(df.ml.ts, aes(variable,variable)) + geom_line() + geom_point(data = df.ml.pred.svm) +
  geom_errorbar(data = df.ml.pred.svm, aes(ymin= variable - 4048, ymax = variable + 4048), width=1, size=0.5, position = pd) +
  labs(title="-- Hiden --",  x ="-- Hiden --", y = "-- Hiden --") +
  theme_minimal()
