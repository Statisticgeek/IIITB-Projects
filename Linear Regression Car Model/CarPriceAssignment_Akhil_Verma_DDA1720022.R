library(MASS)
library(car)
library(stringr)
library(tidyr)

carprice <-read.csv("Carprice_Assignment.csv" , stringsAsFactors = F)



str(carprice)
#DataPrepration for further analysis
#Data cleaning 
#weeding out missing values
#Certain Spell mistakes (fixing that) etc etc


#converting into factor to gather further insights

carprice$symboling <- as.factor(carprice$symboling)
carprice$enginelocation <- as.factor(carprice$enginelocation)
carprice$cylindernumber <- as.factor(carprice$cylindernumber)
carprice$fuelsystem <- as.factor(carprice$fuelsystem)
carprice$aspiration <- as.factor(carprice$aspiration)
carprice$doornumber <- as.factor(carprice$doornumber)
carprice$drivewheel <- as.factor(carprice$drivewheel)
carprice$carbody <- as.factor(carprice$carbody)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$fueltype <- as.factor(carprice$fueltype)


#checking factor for carname
summary(as.factor(carprice$CarName))


#extracting company name from car name

carprice$carCompany <-gsub("\\ .*", "", carprice$CarName)
str(carprice$carCompany)

#converting carname into factor
carprice$carCompany <- as.factor(carprice$carCompany)
summary(carprice$carCompany)
levels(carprice$carCompany)

#after much scrutiny Seems there are a lot of spell mistakes in the dataset
#using STR replace all to overcome this issue
#replacing alfa-romero with alfa romeo
#replacing maxda with mazda
#replacing Nissan with nissan
#replacing porsche with porsche
#replacing vokswagen with volkswagen
#replacing vw with volkswagen
#replacing toyouta with toyota

levels(carprice$carCompany)[1] <- "alfa romeo"
levels(carprice$carCompany)

levels(carprice$carCompany)[10] <- "mazda"
levels(carprice$carCompany)

levels(carprice$carCompany)[14] <- "nissan"
levels(carprice$carCompany)

levels(carprice$carCompany)[16] <- "porsche"
levels(carprice$carCompany)

levels(carprice$carCompany)[21] <- "toyota"
levels(carprice$carCompany)

levels(carprice$carCompany)[21] <- "volkswagen"
levels(carprice$carCompany)

levels(carprice$carCompany)[23] <- "volkswagen"
levels(carprice$carCompany)


#missing value and duplicate value check

sum(is.na(carprice))
#0 missing values

which(duplicated(carprice))
#0 duplicate values

#Checking outliers & cap them

quantile(carprice$wheelbase,seq(0,1,0.01))
boxplot(carprice$wheelbase)
carprice$wheelbase[which(carprice$wheelbase>115.544)]<-115.544

quantile(carprice$carlength,seq(0,1,0.01))
boxplot(carprice$carlength)
carprice$carlength[which(carprice$carlength<150.00)] <- 150.00

quantile(carprice$carwidth,seq(0,1,0.01))
boxplot(carprice$carwidth)
carprice$carwidth[which(carprice$carwidth>70.852)] <- 70.852

quantile(carprice$carheight,seq(0,1,0.01))
boxplot(carprice$carheight)
#no outliers in this variable

quantile(carprice$curbweight,seq(0,1,0.01))
boxplot(carprice$curbweight)
#no outliers in this variable

quantile(carprice$enginesize,seq(0,1,0.01))
boxplot(carprice$enginesize)
carprice$enginesize[which(carprice$enginesize>201.20)] <- 201.20

quantile(carprice$boreratio,seq(0,1,0.01))
boxplot(carprice$boreratio)
#no outliers in this variable

quantile(carprice$stroke,seq(0,1,0.01))
boxplot(carprice$stroke)
carprice$stroke[which(carprice$stroke>3.8600)] <- 3.8600
carprice$stroke[which(carprice$stroke<2.6400)] <- 2.6400

quantile(carprice$compressionratio,seq(0,1,0.01))
boxplot(carprice$compressionratio)
carprice$compressionratio[which(carprice$compressionratio>10.9400)] <- 10.4900

#Creating dummy variables

# For carCompany
dummy_1 <- data.frame(model.matrix( ~carCompany, data = carprice))

# For carbody
dummy_2 <- data.frame(model.matrix( ~carbody, data = carprice))


# Drivewheel 
dummy_3 <- data.frame(model.matrix( ~drivewheel, data = carprice))


#Engine type
dummy_4 <- data.frame(model.matrix( ~enginetype, data = carprice))


#cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = carprice))


# Fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = carprice))

# Symboling
dummy_7 <- data.frame(model.matrix( ~symboling, data = carprice))

#removing first column as it is returning "1" value which is not useful.

dummy_1<-dummy_1[,-1]
dummy_2<-dummy_2[,-1]
dummy_3<-dummy_3[,-1]
dummy_4<-dummy_4[,-1]
dummy_5<-dummy_5[,-1]
dummy_6<-dummy_6[,-1]
dummy_7<-dummy_7[,-1]



#for caategorical variables having 2 levels
#converting them to numeric

levels(carprice$fueltype)<-c(1,0)
#assigning 1 to diesel and 0 to gas
carprice$fueltype<- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

# for aspiration
levels(carprice$aspiration)<-c(1,0)
# Assigning 1 to "std" and 0 to "turbo"
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

# For doornumber
levels(carprice$doornumber)<-c(1,0)
# Assigning 1 if the number of doors is 4, and 0 if the number of doors is 2.
carprice$doornumber<- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

# Enginelocation
levels(carprice$enginelocation)<-c(1,0)
# Assigning 1 if the engine is front and 0 if in rear
carprice$enginelocation<- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]



# Combining the dummy variables and the numeric columns (categorical variables with 2 levels)
carprice1 <- cbind(carprice[ , c(1,4:6,9:14,17,19:26)], dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6, dummy_7)
View(carprice1)

#splitting the data into train and test data
# Test data= 30% of the indices(Carprice data)
#Train data=70% of the indices
set.seed(100)
indices= sample(1:nrow(carprice1), 0.7*nrow(carprice1))

train=carprice1[indices,]
test = carprice1[-indices,]



# Regression first model

model_1 <-lm(price~.,data=train[,-1])
# Train[ , -1] is because we dont need the ID of the data
summary(model_1)

# using stepAIC
# Stepwise approach
step <- stepAIC(model_1, direction="both")

step

#making Model2 after running step variable
#according to R statistically these are the variables which are significant
# Also R have removed insignificant variables as per stepAIC.


model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                symboling3, data = train[, -1])
summary(model_2)
vif(model_2)

#sorting VIF values so that it gets easier to deduce which variables to remove

sort(vif(model_2))

#after looking at VIF and significance of variables
#removing carlength variable as it has high VIF and 0 stars in terms of significance
#Removing 1 variable at a time and check all VIF/P-value and Adjusted R-Squared values
#To notice if there is a drastic change after removing a variable

model_3 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + citympg + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                symboling3, data = train[, -1])
summary(model_3)
vif(model_3)

sort(vif(model_3))

#repeating all steps
#removing citympg

model_4 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                fuelsystem2bbl + fuelsystemmpfi + symboling.1 + symboling0 + 
                symboling3, data = train[, -1])
summary(model_4)
vif(model_4)
sort(vif(model_4))


#repeating all steps
#removing fuelsystemempfi
model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyporsche + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                fuelsystem2bbl + symboling.1 + symboling0 + 
                symboling3, data = train[, -1])
summary(model_5)
vif(model_5)
sort(vif(model_5))

#repeating all steps
#removing carcompanyporsche

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                fuelsystem2bbl + symboling.1 + symboling0 + 
                symboling3, data = train[, -1])

summary(model_6)
vif(model_6)
sort(vif(model_6))

#Repeating all steps
#Removing fuelsystem2bbl
model_7 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                symboling.1 + symboling0 + 
                symboling3, data = train[, -1])
summary(model_7)
vif(model_7)
sort(vif(model_7))


#Repeating all steps
#Removing symboling0

model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                symboling.1 + symboling3, data = train[, -1])
summary(model_8)
vif(model_8)
sort(vif(model_8))

#Repeating all steps
#Removing symboling.1
model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                carwidth + curbweight + enginesize + stroke + peakrpm + 
                carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                carCompanyjaguar + carCompanymazda + carCompanymercury + 
                carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                carCompanyplymouth + carCompanyrenault + 
                carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                symboling3, data = train[, -1])
summary(model_9)
vif(model_9)
sort(vif(model_9))

#Repeating all steps
#Removing carcompanymercury

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive + 
                 symboling3, data = train[, -1])
summary(model_10)
vif(model_10)
sort(vif(model_10))

#Repeating all steps
#Removing symboling3

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_11)
vif(model_11)
sort(vif(model_11))

#Repeating all steps
#Removing Carbodyhardtop

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_12)
vif(model_12)
sort(vif(model_12))



#repeating all steps
#removing carbodyhatchback

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_13)
vif(model_13)
sort(vif(model_13))

#repeating all steps
#removing carbodysedan

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen + 
                 carbodywagon + 
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_14)
vif(model_14)
sort(vif(model_14))

#repeating all steps
#removing carbodywagon


model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_15)
vif(model_15)
sort(vif(model_15))

#All the variables are significant now
#choosing variable with higher VIF
#Among enginesize and curbweight , curbweight is less significant i.e. 1 star as compared to enginesize which is 3star.
#Removing curbweight

model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor + cylindernumberfive, data = train[, -1])
summary(model_16)
vif(model_16)
sort(vif(model_16))

#removing cylindernumberfive because of 0 stars (not significant as per model_16)

model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke + peakrpm + 
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_17)
sort(vif(model_17))


#comparing less significant variables among carcompanysaab and peakrpm
#removing peakrms because of higher VIF , higher P value.

model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysaab + carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_18)
sort(vif(model_18))


#removing least significant variable after model_18 carCompanysaab
model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + carCompanyhonda + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_19)
sort(vif(model_19))

#removing least significant variable after model_19 carcompanyhonda

model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + carCompanyrenault + 
                 carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_20)
sort(vif(model_20))


#removing least significant variable after model_20 carCompanyrenault

model_21 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + 
                 carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 drivewheelrwd + enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_21)
sort(vif(model_21))


#comparing least significant variables carcompanyvolkswagen and drivewheelrwd
#removing drivewheelrwd because of higher VIF value

model_22 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + 
                 carCompanysubaru + carCompanytoyota + carCompanyvolkswagen +
                 enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_22)
sort(vif(model_22))

#after model_22 evaluation , removing carCompanyvolkswagen as this variable have 0 significance after model_22

model_23 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + carCompanydodge + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_23)
sort(vif(model_23))


#removing least significant variable after model_23 carCompanydodge

model_24 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanyplymouth + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_24)
sort(vif(model_24))



#removing least significant carCompanyplymouth after model_24

model_25 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanynissan + carCompanypeugeot + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_25)
sort(vif(model_25))

#removing least significant carCompanynissan after model_25

model_26 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanypeugeot + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetypeohc + enginetyperotor, data = train[, -1])
summary(model_26)
sort(vif(model_26))

#among lower significant variables enginetypeohc have larger VIF value
#removing enginetypeohc

model_27 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanymazda +
                 carCompanymitsubishi + carCompanypeugeot + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetyperotor, data = train[, -1])
summary(model_27)
sort(vif(model_27))

#among lower significant variables
#removing carCompanymazda
#because of higher VIF value and higher Pvalue as compared to carcompanymitsubishi

model_28 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanymitsubishi + carCompanypeugeot + 
                 carCompanysubaru + carCompanytoyota + 
                 enginetyperotor, data = train[, -1])
summary(model_28)
sort(vif(model_28))

#removing carCompanytoyota among lower significant variables
#higher P value as compared to carcompanypeugeot
model_29 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + carCompanypeugeot + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_29)
sort(vif(model_29))

#removing least significant variable carcompanypeugeot

model_30 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_30)
sort(vif(model_30))

# Test the model on test dataset
Predict_1 <- predict(model_30,test[,-c(1,20)])

# Adding new column "test_price" into the test dataset

test$test_price <- Predict_1

# calculating the test R2 

cor(test$price,test$test_price)
cor(test$price,test$test_price)^2

#R^2 of test data is 0.8528.
#adjusted R^2 of model_30 is 0.9247
#More variables can be removed



#now all the variables are highly significant
#adjusted Rsquared value = 0.9247

#hit and trial by removing and checking all variables if the removal of variable is making any drastic change
#in the adjusted Rsquared value.


#removing carcompanybmw

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))
#Adjusted R squared value dropped from 0.9247 to 0.9066 , 0.018 drop.


#Removing enginetyperotor

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru, data = train[, -1])
summary(model_31)
sort(vif(model_31))


#Adjusted R squared value dropped from 0.9247 to 0.9177 , 0.007 drop.


#removing aspiration

model_31 <- lm(formula = price ~  enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru + enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))

#Adjusted R squared value dropped from 0.9247 to 0.9219 , 0.002 drop.


#Removing carcompanysubaru

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))

#Adjusted R squared value dropped from 0.9247 to 0.9149 , 0.009 drop.



#Removing enginelocation

model_31 <- lm(formula = price ~ aspiration +
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))

#rsquared dropped .9247 to 0.8841 , 0.0406.

#Removing carcompanyjaguar

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))

#rsquared dropped .9247 to 0.891, 0.033.


#Removing stroke

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))

#rsquared dropped .9247 to 0.9032, 0.0215


#Removing carcompanybuick

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))
#rsquared dropped .9247 to 0.8768, 0.0479.


#Removing carwidth

model_31 <- lm(formula = price ~ aspiration + enginelocation + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))
#rsquared dropped .9247 to 0.8983, 0.0264


#Removing enginesize

model_31 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru +  
                 enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))
#rsquared dropped .9247 to 0.8943, 0.0304



#After checking all the difference in Rsquared values
#Selecting Model_31 with removing aspiration (because of lowest drop)


model_31 <- lm(formula = price ~  enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru + enginetyperotor, data = train[, -1])
summary(model_31)
sort(vif(model_31))



#removing enginetyperotor (2nd lowest difference) 

model_32 <- lm(formula = price ~  enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar + 
                 carCompanysubaru, data = train[, -1])
summary(model_32)
sort(vif(model_32))

#Removing carcompanysubaru (3rd lowest diff)


model_33 <- lm(formula = price ~  enginelocation + 
                 carwidth + enginesize + stroke +  
                 carCompanybmw + carCompanybuick + 
                 carCompanyjaguar, data = train[, -1])
summary(model_33)
sort(vif(model_33))


#Checking Rsquared value for test data
#hit and trial if its coming same
#then we can finalize model_33

# Test the model on test dataset

Predict_1 <- predict(model_33,test[,-c(1,20)])
test$test_price <- Predict_1
cor(test$price,test$test_price)
cor(test$price,test$test_price)^2

#rsquared for test data is 0.8366
#rsquared for train data is 0.9075



#removing stroke

model_34 <- lm(formula = price ~ enginelocation + 
                 carwidth + enginesize +  
                 carCompanybmw + carCompanybuick +
                 carCompanyjaguar, data = train[, -1])
summary(model_34)
sort(vif(model_34))

# Test the model on test dataset

Predict_1 <- predict(model_34,test[,-c(1,20)])
test$test_price <- Predict_1
cor(test$price,test$test_price)
cor(test$price,test$test_price)^2

#rsquared for test data is 0.8567
#rsquared for train data is 0.8989
#.0422 difference (Rsquared values of test and train data)


#removing carcompanyjaguar

model_35 <- lm(formula = price ~ enginelocation + 
                 carwidth + enginesize +  
                 carCompanybmw + carCompanybuick, data = train[, -1])
summary(model_35)
sort(vif(model_35))


#Rsquared value dropped drastically from 0.8989 (model_34) to 0.8742(model_35)
#remaining variables are the final variables needed for price prediction
#testing model35 on test data.
Predict_1 <- predict(model_35,test[,-c(1,20)])
test$test_price <- Predict_1
cor(test$price,test$test_price)
cor(test$price,test$test_price)^2

#adjusted Rsquaredvalue for model35 = 0.8742
#Rsquared value of this model on test data = 0.8270
#0.0472 difference (Rsquared values of test and train data)


#Difference between Rsqaured values is more in model_35 as compared to model_34
#therefore *model_34* is our final model for price prediction

#factors that impact the pricing are
#1-enginelocation
#2-carwidth
#3-enginesize
#4-carcompanybmw
#5-carCompanybuick



