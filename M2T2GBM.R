#=========================Set Working Directory=======================================
setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M2T2")
#=================== Loading libraries====================================
#install.packages("doMC",dependencies = TRUE)# using extra cores
library(readxl)
library(caret)  
library(corrplot)
library(arules)
library(dplyr)
#require(doMC)
#====================Data import==========================================
mydata <- read.csv("existingproductattributes2017.2.csv")
names(mydata) <- c("Type","Number","price","Five","Four","Three","Two","One","Psr",
                   "Nsr","Rp","Bsr","Weight","Depth","Width","Height",
                   "Profit","Vol")
#===================Preprocessing==========================================
summary(mydata)
head(mydata)

# dummify the data

newDF <- dummyVars(" ~ .", data = mydata)
readyData <- data.frame(predict(newDF, newdata = mydata))

#========================================function for Inv Rank===========================
IRating <-function(Bsr){
  inv <- c()
  na_index <- which(is.na(Bsr))
  no_na_index <- which(!is.na(Bsr))
  
  inv[na_index] <- 0
  inv[no_na_index] <- 1/Bsr[no_na_index]
  
  return(inv*1000)
}
readyData$Bsr<- IRating(readyData$Bsr)
head(readyData)
#========================================function for star Rank===========================

weightrat <-function(a4,a3,a2,a1){
  avg= a4*4+a3*3+a2*2+a1*1
  return(avg) 
}

readyData$Five <- weightrat(readyData$Four,
          readyData$Three,readyData$Two,
          readyData$One )
head(readyData)
names(readyData)[names(readyData) == 'Five'] <- 'RAT'
head(readyData)
#=========================== dropcol==========================
readyData = filter(readyData, Type.PC == 1 | Type.Netbook == 1 |Type.Laptop == 1 |Type.Smartphone== 1)
readyData$Number<- NULL
#readyData$Five<- NULL
readyData$Four<- NULL
readyData$Three<- NULL
readyData$Two<- NULL
readyData$One<- NULL
#readyData$Nsr<- NULL
#readyData$Bsr<- NULL
summary(readyData)
#readyData <- readyData[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,22,21)]
head(readyData)
readyData <- readyData[ -c(1:12,16) ]

#=========================== Corelations==========================

corrData <- cor(readyData) 
corrplot(corrData, method = "circle")
#==========================REPLACE OUTLIERS BY MEDIAN ===============
outlierKD <- function(dt, var) {
  #define variables
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  m1 <- median(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- median(outlier)
  
  #create 2x2 canvas
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  
  # If value is an outlier introduce median
  # If not, do nothing
  var_name <- ifelse(var_name %in% outlier, m1, var_name)
  m2 <- median(var_name, na.rm = T)
  na <- length(outlier)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check for var", outer=TRUE)
  
  #print messages
  message("Outliers identified: ", na, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", na / tot*100)
  message("Median of the outliers: ", mo)
  message("Median without removing outliers: ", m1)
  message("Median if we remove outliers: ", m2)
  
  dt[as.character(substitute(var))] <- invisible(var_name)
  assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
  message("Outliers successfully removed", "\n")
  par(mfrow= c(1,1),oma=c(0,0,0,0))
  return(invisible(dt))
}

if(outlier_detect){
  setwd("C:\\Users\\ROS\\Documents\\R Tutorial Data\\R Tutorial Data Sets\\ROSDA\\M2T2")
  pdf("outlierspsr.pdf")
  outlierKD(dt,var) # insert data frame and outlier search variable here
  dev.off()
}
outlierKD(readyData,price)
outlierKD(readyData,Psr)
outlierKD(readyData,Weight)
outlierKD(readyData,Depth)
outlierKD(readyData,Height)
outlierKD(readyData,Vol)
outlierKD(readyData,wrat)
outlierKD(readyData,Bsr)
outlierKD(readyData,RAT)
summary(readyData)
head(readyData)

#===================#checks for correlations over 0.85 - adjustable at the end=======
corr_check <- function(yourdataset, threshold){
  cor_matrix <- cor(yourdataset)
  cor_matrix
  
  for (i in 1:nrow(cor_matrix)){
    correlations <-  which((abs(cor_matrix[i,i:ncol(cor_matrix)]) > threshold) & (cor_matrix[i,i:ncol(cor_matrix)] != 1))
    
    if(length(correlations)> 0){
      lapply(correlations,FUN =  function(x) (cat(paste(colnames(yourdataset)[i], "with",colnames(yourdataset)[x]), "\n")))
    }
  }
}

print(corr_check(readyData, 0.85))

#=======================data slicing method 2============================
#====================slicing using Caret keeping the proportions============
set.seed(345)
indexes <- createDataPartition(y=readyData$Vol, times=1,p=0.7,list=FALSE)
trainSet<- readyData[indexes,]
testSet <- readyData[-indexes,]
str(trainSet)

#===========================model RF======================================== 
set.seed(2334)
#ctrl <- trainControl(method="repeatedcv",number=1,repeats = 1) 
#RF <- randomForest(brand~ ., data=trainSet, ntree=50,
#                   importance=TRUE, proximity=TRUE)
svm_tune <- train(Vol~., data = trainSet, method= "gbm", metric="RMSE")
svm_tune
#svm_tune <- train(x=trainSet, y= trainSet$Vol,
#                  method = "svmRadial",   # Radial kernel
#                  tuneLength = 9,					# 9 values of the cost function
#                  preProc = c("center","scale"),  # Center and scale data
#                  metric="pROC",
#                  trControl=ctrl)
# estimate variable importance
importance <- varImp(svm_tune, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

my_control <- rfeControl(functions = lmFuncs, method = "repeatedcv", 
                         repeats = 3, verbose = FALSE)
results <- rfe(readyData[,1:11], readyData$Vol, sizes=8
               , rfeControl=my_control)
# summarize the results #8,5 good res
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
#===========================test============================
svmpred = predict(svm_tune, newdata=testSet)
head(svmpred)
