

#setwd("C:/Users/DarkcodedSoul/Desktop/SVM assignment")

library(dplyr)
library(caret)
library(kernlab)
library(gridExtra)


# Loading the test and train data

library(readr)
mnist_train <- read_csv("mnist_train.csv",col_names = FALSE)
mnist_test <- read_csv("mnist_test.csv",col_names = FALSE)

#checking dimension
dim(mnist_train)

#checking few entries entries
head(mnist_train)
tail(mnist_train)
nrow(mnist_train)
#First column is the tag for number 0-9 which are handwritten digits



#Reducing the training size i.e. subsample the training data
# we took close to 8.5% of the total training data 
train1 <- 5000
trainx <- sample(nrow(mnist_train), train1, replace=FALSE)
test1 <- train1*0.3
testx <- trainx[1:test1]
trainsample <- trainx[(test1+1):train1]

mnistTrain <- mnist_train[trainsample,]
mnisttestx <- mnist_train[testx,]
mnistTest <- mnist_test 

View(mnistTrain)
View(mnistTest)
View(mnisttestx)

#Data understanding
#after an overlook at data , we can conclude that first column is the tag of digit(0-9)
#rest of the columns are the pixels forming the digit mentioned in 1st column


#Renaming the firt and target column to Tag
colnames(mnistTrain)[colnames(mnistTrain)=="X1"] <- "Tag"
colnames(mnistTest)[colnames(mnistTest)=="X1"] <- "Tag"
colnames(mnisttestx)[colnames(mnisttestx)=="X1"] <-"Tag"


#converting it into a factor variable
mnistTrain$Tag <- as.factor(mnistTrain$Tag)

mnistTest$Tag <- as.factor(mnistTest$Tag)

mnisttestx$Tag <- as.factor(mnisttestx$Tag)


#Checking for missing values
#no missing values
sapply(mnistTrain, function(x) sum(is.na(x)))
sapply(mnistTest, function(x) sum(is.na(x)))
sum(is.na(mnistTrain))
sum(is.na(mnistTest))
sum(is.na(mnisttestx))

#checking names and summary
names(mnistTrain)
names(mnistTest)

#after checking out summary we can be sure that there is no negative values in the dataset.
summary(mnistTrain)
summary(mnistTest)
summary(mnisttestx)


#Exploratory data analysis


# a lot of values nearby 600 are off the hook
correlationmatrix <-- cov(mnistTrain[ ,2:785])
highlycorrelated <- findCorrelation(correlationmatrix , cutoff = 0.5)  
print(highlycorrelated)
plot(highlycorrelated)



library(ggplot2)

#checking first 10 rows , and comparing the actual digit in the column vs what image shows us
#and checking them how the pixels are showing the digit

#1st row the digit is 4 but cant trace it from image output as it is in reverse form
digit1 <- matrix(as.numeric(mnistTrain[1,-1]), nrow=28)
image(digit1, col = grey.colors(255))

#2nd row the digit is 2 but cant trace it from image output as it is in reverse form
digit2 <- matrix(as.numeric(mnistTrain[2,-1]), nrow=28)
image(digit2, col = grey.colors(255))

#3rd row the digit is 9 but cant trace it from image output as it is in reverse form
digit3 <- matrix(as.numeric(mnistTrain[3,-1]), nrow=28)
image(digit3, col = grey.colors(255))

#4th row the digit is 8 but can trace it from image output as reverse of 8 is also 8
digit4 <- matrix(as.numeric(mnistTrain[4,-1]), nrow=28)
image(digit4, col = grey.colors(255))

#5th row the digit is 1 but can trace it from image output as the reverse of 1 is also a 1
digit5 <- matrix(as.numeric(mnistTrain[5,-1]), nrow=28)
image(digit5, col = grey.colors(255))

#6th row the digit is 2 but cant trace it from image output as it is in reverse form
digit6 <- matrix(as.numeric(mnistTrain[6,-1]), nrow=28)
image(digit6, col = grey.colors(255))

#7st row the digit is 7 but cant trace it from image output as it is in reverse form 
digit7 <- matrix(as.numeric(mnistTrain[7,-1]), nrow=28)
image(digit7, col = grey.colors(255))

#8th row the digit is 9 but cant trace it from image output as it is in reverse form
digit8 <- matrix(as.numeric(mnistTrain[8,-1]), nrow=28)
image(digit8, col = grey.colors(255))

#9th row the digit is 5 but cant trace it from image output as it is in reverse form
digit9 <- matrix(as.numeric(mnistTrain[9,-1]), nrow=28)
image(digit9, col = grey.colors(255))

#10th row the digit is 4 but cant trace it from image output as it is in reverse form
digit10 <- matrix(as.numeric(mnistTrain[10,-1]), nrow=28)
image(digit10, col = grey.colors(255))

#so after checking first 10 rows , we came to conclusion that image is recording the correct but it is recorded
# upside down reverse order.

#Lets check the intensity of a digit throughout
# we can see that intensity of digit"1" is lowest as per plot
# intensity of digit"0" is highest

mnistTrain$intensity <- apply(mnistTrain[ , -1],1,mean) #takes the mean of each row
intbyTag <- aggregate (mnistTrain$intensity, by = list(mnistTrain$Tag), FUN = mean)

plot <- ggplot(data=intbyTag , aes(x=Group.1, y=x)) + geom_bar(stat="identity")
plot+ scale_x_discrete(limits=0:9) + xlab("Digit Tag") + ylab("average intensity")


head(intbyTag)
tail(intbyTag)


#Group.1        x
#1       0 43.50845
#2       1 19.34823
#3       2 38.10447
#4       3 36.13285
#5       4 31.17861
#6       5 33.95531
#7        6 35.21008
#8        7 29.74158
#9        8 38.18870
#10       9 31.68415



pixel0 <- qplot(subset(mnistTrain, Tag ==0)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 0")

pixel1 <- qplot(subset(mnistTrain, Tag ==1)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 1")

pixel2 <- qplot(subset(mnistTrain, Tag ==2)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 2")

pixel3 <- qplot(subset(mnistTrain, Tag ==3)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 3")

pixel4 <- qplot(subset(mnistTrain, Tag ==4)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 4")

pixel5 <- qplot(subset(mnistTrain, Tag ==5)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 5")

pixel6 <- qplot(subset(mnistTrain, Tag ==6)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 6")

pixel7 <- qplot(subset(mnistTrain, Tag ==7)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 7")

pixel8 <- qplot(subset(mnistTrain, Tag ==8)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 8")

pixel9 <- qplot(subset(mnistTrain, Tag ==9)$intensity, binwidth = .75, 
                xlab = "Intensity Histogram for 9")

pixel0
pixel1
pixel2
pixel3
pixel4
pixel5
pixel6
pixel7
pixel8
pixel9

grid.arrange(pixel0,pixel1,pixel2,pixel3,pixel4,pixel5,pixel6,pixel7,pixel8,pixel9)

#as per histogram plotted
# Pixel0 is normally distributed
# Pixel1 is normally distributed
# Pixel2 is normally distributed
# Pixel3 have 2 modes and is a bi-modal distributed
# Pixel4 have almost bi-modal distribution
# Pixel5 have almost bi-modal distribution
# Pixel6 is normally distributed
# Pixel7 have almost bi-modal distribution
# Pixel8 is normally distributed
# Pixel9 is perfectly normally distributed

#Conclusion - People tend to draw digit3 , digit4 , digit5 , digit6 , digit7 , digit 8 very differently
# as compared to other digits.

#it also makes sense because generally
# digit 5 and digit 8 is sometimes get confusing
# digit 4 is made in 2 styles (1 normal and 1digit 4 with a hole in it)
# there can be a confusion in running digit of 3 and 7

checkfor0 <- mnistTrain[mnistTrain$Tag == 0, ]
checkfor1 <- mnistTrain[mnistTrain$Tag == 1, ]
checkfor2 <- mnistTrain[mnistTrain$Tag == 2, ]
checkfor3 <- mnistTrain[mnistTrain$Tag == 3, ]
checkfor4 <- mnistTrain[mnistTrain$Tag == 4, ]
checkfor5 <- mnistTrain[mnistTrain$Tag == 5, ]
checkfor6 <- mnistTrain[mnistTrain$Tag == 6, ]
checkfor7 <- mnistTrain[mnistTrain$Tag == 7, ]
checkfor8 <- mnistTrain[mnistTrain$Tag == 8, ]
checkfor9 <- mnistTrain[mnistTrain$Tag == 9, ]

flip <- function(matrix){
  apply(matrix, 2, rev)
}


#different types of 0 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 0
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor0[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


#different types of 1 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 1
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor1[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


#different types of 2 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 2
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor2[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


#different types of 3 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 3
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor3[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}



#different types of 4 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 4 all 9 images are quite different 
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor4[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


#different types of 5 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 5 all 9 images are quite different 
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor5[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}

#different types of 6 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 6 
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor6[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}

#different types of 7 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 7
# i wonder why i didnt see the seven with a cut mark in middle of it , i draw like that
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor7[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}

#different types of 8 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 8
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor8[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}


#different types of 9 (total of 9 plots i.e. 3:3 3 rows 3 columns)
#a variety of hand written 9
par(mfrow=c(3,3))
for (i in 2:28){
  digit <- flip(matrix(rev(as.numeric(checkfor9[i, -c(1, 786)])), nrow = 28)) #look at one digit
  image(digit, col = grey.colors(255))
}

#conclusion1 - average intensity can have some logical explanation for prediction
#conclusion2 - There is alot of variablity in the way people write digits.



#Constructing Model

mnistTrain$intensity <- NULL

#Using Linear Kernel
Model1_linear <- ksvm(Tag~ ., data = mnistTrain, kernel = "vanilladot")
Evaluate_model1_linear<- predict(Model1_linear, mnisttestx)

table(Evaluate_model1_linear,mnisttestx$Tag)
agreement <-Evaluate_model1_linear == mnisttestx$Tag
prop.table(table(agreement))

#agreement
#FALSE       TRUE 
#0.09733333 0.90266667


#confusion matrix - Linear Kernel
confusionMatrix(Evaluate_model1_linear,mnisttestx$Tag)
#accuracy .9027

#lets improve accuracy by changing vanilladot kernel
#improving model performance/accuracy by rbfdot kernel

#Using RBF Kernel
Model1_RBF <- ksvm(Tag~ ., data = mnistTrain, scale = FALSE, kernel = "rbfdot")
Evaluate_Model1RBF_RBF<- predict(Model1_RBF, mnisttestx)


table(Evaluate_Model1RBF_RBF,mnisttestx$Tag)
agreement2 <-Evaluate_Model1RBF_RBF == mnisttestx$Tag
prop.table(table(agreement2))

#agreement2
#FALSE       TRUE 
#0.05333333 0.94666667 

#confusion matrix - RBF Kernel
confusionMatrix(Evaluate_Model1RBF_RBF,mnisttestx$Tag)
#accuracy 0.9467

#RBF kernel performs better than the vanilladot/linear 
#RBF kernel accuracy = 0.9467
#Vanilladot accuracy = 0.9027


#hyperparameter tuning and cross validation using rbfdot kernel and tune.svm function
#Cost and Gamma hyperparameters tuning them over a range
# Gamma range 0.01 to 10
# cost range 0.01 to 10
# checking the best parameters


library(e1071)


# CV = Cross validation
# number= number of times fold
trainControl <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters
#that we give input to our model

set.seed(10)
grid <- expand.grid(.sigma=c(0.01,0.015,0.025,0.05,0.1), .C=c(0.1,0.5,1,1.5,2) )


#Method = Cross validation Algorithm
#Metric = Accuracy as defined above
#tuneGrid = the inpur hypoerparameters we gave
#trcontrol = Our traincontrol method.

fit.svm1 <- train(Tag~., data=mnistTrain, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm1)

#Resampling: Cross-Validated (10 fold) 
#Summary of sample sizes: 3151, 3147, 3152, 3150, 3150, 3149, ... 
#Resampling results across tuning parameters:
  
#  sigma  C    Accuracy   Kappa
#0.010  0.1  0.1191415  0    
#0.010  0.5  0.1191415  0    
#0.010  1.0  0.1191415  0    
#0.010  1.5  0.1191415  0    
#0.010  2.0  0.1191415  0    
#0.015  0.1  0.1191415  0    
#0.015  0.5  0.1191415  0    
#0.015  1.0  0.1191415  0    
#0.015  1.5  0.1191415  0    
#0.015  2.0  0.1191415  0    
#0.025  0.1  0.1191415  0    
#0.025  0.5  0.1191415  0    
#0.025  1.0  0.1191415  0    
#0.025  1.5  0.1191415  0    
#0.025  2.0  0.1191415  0    
#0.050  0.1  0.1191415  0    
#0.050  0.5  0.1191415  0    
#0.050  1.0  0.1191415  0    
#0.050  1.5  0.1191415  0    
#0.050  2.0  0.1191415  0    
#0.100  0.1  0.1191415  0    
#0.100  0.5  0.1191415  0    
#0.100  1.0  0.1191415  0    
#0.100  1.5  0.1191415  0    
#0.100  2.0  0.1191415  0    

#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.1 and C = 0.1. 



fit.svm1 <- train(Tag~., data=mnistTrain, method="svmpoly", metric=metric, 
                  tuneGrid=grid, trControl=trainControl)

print(fit.svm1)


fit.svm <- tune.svm(Tag~.,data=mnistTrain , gamma= c(0.01,0.025,0.05) , cost=c(0.1,0.5,1,2))

print(fit.svm)

#Parameter tuning of 'svm':
#  
#  - sampling method: 10-fold cross validation 
#
#- best parameters:
#  gamma cost
#0.01  0.1
#
#- best performance: 0.8808571 




# Constructing final model with tuned parameters
# gamma = 0.1 and Cost(c) =0.1

# so we see that accuracy is very poor i.e. 0.11 for svmradial
# but the hyperparameters are tuned in tune.svm
# gamma = 0.01 and C=0.1
# taking the parameters of  tune.svm over svmradial because the accuracy was higher for tune.svm


# Constructing final model with tuned parameters on rbfdot kernel
Finalmodel <- ksvm(Tag~ ., data = mnistTrain, scale = FALSE, gamma=0.01,cost=0.1 ,kernel = "rbfdot")
EvaluateFinalmodel<- predict(Finalmodel, mnistTest)

confusionMatrix(EvaluateFinalmodel,mnistTest$Tag)
#Accuracy with rbdfot kernel = 0.9482 (matching with our train data set of accuracy = 0.9467)


#checking the final model accuracy with a polydot kernel

Finalmodel1 <- ksvm(Tag~ ., data = mnistTrain, scale = FALSE, gamma=0.01,cost=0.1 ,kernel = "polydot")
EvaluateFinalmodel1<- predict(Finalmodel1, mnistTest)

confusionMatrix(EvaluateFinalmodel1,mnistTest$Tag)

#Accuracy for polydot kernel is 0.9072



#Summary
#accuracy of train data with vanilladot kernel = 90.27%
#accuracy of train data with rbfdot kernel = 94.67%
#svm.fit data with svmradial leads us to a poor accuracy of 11%
#svm.fit using the function tune.svm leads us to the best hyperparameters gamma=0.01 cost=0.1


# Cross validating
# After hyperparameters are tuned
# Gamma= 0.01 and Cost = 0.1
# Accuracy of 94.82% using rbfdot kernel
# Accuracy of 90.72% using polydot kernel
# so we conclude that rbfdot kernel leads us to the best accuracy of 94.82% matching with our train data of accuracy=94.67%
