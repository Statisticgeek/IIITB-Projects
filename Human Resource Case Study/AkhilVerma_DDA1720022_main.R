#################################################################
### LOADING THE REQUIRED LIBRARIES
#################################################################


library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(lubridate)
library(scales)
library(gridExtra)
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)




#################################################################
### DATA SOURCING
#################################################################

# Loading csv files in R environment
employee_survey_data <- read.csv(file="employee_survey_data.csv",stringsAsFactors = FALSE)
general_data <- read.csv(file="general_data.csv",stringsAsFactors = FALSE)
manager_survey_data <- read.csv(file="manager_survey_data.csv",stringsAsFactors = FALSE)

# Checking if the number of Employee ID's are equal in files before merging.
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(employee_survey_data$EmployeeID,manager_survey_data$EmployeeID)

# As the differences are 0 hence merging the files into a single data set based upon EmployeeID column
employee_data <- merge(employee_survey_data,general_data, by="EmployeeID", all = F)
employee_data <- merge(employee_data,manager_survey_data, by="EmployeeID", all = F)

# Loading in_time & out_time files in R environment.
intime <- read.csv(file="in_time.csv",stringsAsFactors = FALSE)
outtime <- read.csv(file="out_time.csv",stringsAsFactors = FALSE)




#################################################################
### DATA CLEANING & PREPARATION
#################################################################

# Changing the names of the first column in the intime and outtime datasets to "EmployeeID"
colnames(intime)[1] <- c("EmployeeID")
colnames(outtime)[1] <- c("EmployeeID")

# Changing the Wide format to Long format for intime and outtime datasets
intime <- gather(intime,date,timein,-EmployeeID)
outtime <- gather(outtime,date,timeout,-EmployeeID)

# Merging the intime and outtime datasets as timefile
timefile <- merge(intime,outtime,by=c("EmployeeID","date") , all=F)

# Checking if there are cases in which employee forgot/missed to punch in or out
sum(is.na(timefile$timein) & !is.na(timefile$timeout))
sum(!is.na(timefile$timein) & is.na(timefile$timeout))

# Converting the timein & timeout columns to Date format
timefile$timein <- as.POSIXlt(timefile$timein, format = "%Y-%m-%d %H:%M:%S")
timefile$timeout <- as.POSIXlt(timefile$timeout, format = "%Y-%m-%d %H:%M:%S")


# Derived metric #1
###################

# Calculating difference between intime and outtime in a derived column 'timeDifference'
timefile$timeDifference <- difftime(timefile$timeout,timefile$timein)

# Converting timeDifference colunm to numeric
timefile$timeDifference <- as.numeric(timefile$timeDifference)
summary(timefile$timeDifference)

# Considering only the timeDifference and eliminating the intime/outtime columns
timefile <- timefile[,-c(3,4)]

# Converting the timefile from long format to wide format
timefile<-spread(timefile,date,timeDifference)
timefile[,2:262]<-sapply(timefile[,2:262],function(x) as.integer(as.character(x)))

length(timefile) # the attendance details are provided for 261 days (excluding the 1st coulmn which is EmplyeeID).


# Removal of NA values 1
########################

# Removing columns with all values as NA, presuming those days would be Holidays.
length(colnames(timefile)[colSums(is.na(timefile)) == nrow(timefile)])
timefile<-timefile[, colSums(is.na(timefile)) != nrow(timefile)]

length(timefile) # There are 12 holidays in the data provided.


# Merging the employee_data & timefile as employeefinal, after checking for the differences in EmployeeID column.
setdiff(employee_data$EmployeeID,timefile$EmployeeID)
employeefinal <- merge(employee_data,timefile, by="EmployeeID", all = F)


# Removal of NA values 2
########################

# TotalWorkingYears have 9 NA values (which are not even 1% of the total values (4410)
sum(is.na(employeefinal$TotalWorkingYears))

# Removing rows with NA values from TotalWorkingYears
empTotWorkYrRemove <- c(which(is.na(employeefinal$TotalWorkingYears)))
employeefinal <- employeefinal[-empTotWorkYrRemove,]


# Removal of NA values 3
########################

# NumCompaniesworked have 19 NA values (which are not even 1% of the total values (4410)
sum(is.na(employeefinal$NumCompaniesWorked))

# Removing rows with NA values from NumCompaniesWorked
empNumCompaniesWorkedRemove<-c(which(is.na(employeefinal$NumCompaniesWorked)))
employeefinal<-employeefinal[-empNumCompaniesWorkedRemove,]


# Removal of variables with same values throughout
##################################################

# Removing EmployeeCount column
unique(employeefinal$EmployeeCount)
employeefinal$EmployeeCount<- NULL

# Removing Over18 column
unique(employeefinal$Over18)
employeefinal$Over18<- NULL

# Removing StandardHours column
unique(employeefinal$StandardHours)
employeefinal$StandardHours<- NULL


# Imputation of missing values with mode
#########################################

# Below is the function to calculate mode of a column
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Imputation of values which are still NA's, Imputing them with the mode of that particular column 
employeefinal[which(is.na(employeefinal$EnvironmentSatisfaction)),]$EnvironmentSatisfaction<-getmode(employeefinal$EnvironmentSatisfaction)
employeefinal[which(is.na(employeefinal$JobSatisfaction)),]$JobSatisfaction<-getmode(employeefinal$JobSatisfaction)
employeefinal[which(is.na(employeefinal$WorkLifeBalance)),]$WorkLifeBalance<-getmode(employeefinal$WorkLifeBalance)


str(employeefinal)

# Exporting the employeefinal dataset as csv
write.csv(employeefinal , "employeefinal.csv")




# Outlier Treatment in the continuos data
#########################################


# Defining the theme for histogram & boxplots
box_theme <- theme(axis.line=element_blank(), axis.title=element_blank(), 
                   axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y <- theme(axis.line.y=element_blank(), axis.title.y=element_blank(), 
                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                     legend.position="none")



# Outlier check in Age: No outliers
quantile(employeefinal$Age,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(employeefinal, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)



# Outlier check in DistanceFromHome: No outliers
quantile(employeefinal$DistanceFromHome,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(DistanceFromHome))+ geom_histogram(binwidth = 10),
          ggplot(employeefinal, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

summary(employeefinal$DistanceFromHome)



# Outlier check in MonthlyIncome: Outliers present
quantile(employeefinal$MonthlyIncome,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(MonthlyIncome))+ geom_histogram(),
          ggplot(employeefinal, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Treatment of Outliers in MonthlyIncome column
x<-employeefinal$MonthlyIncome
qnt <- quantile(x, probs = c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value <- qnt[2]+H
employeefinal$MonthlyIncome[which(employeefinal$MonthlyIncome>outlier_value)] <- outlier_value



# Outlier check in PercentSalaryHike: No outliers
quantile(employeefinal$PercentSalaryHike,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(PercentSalaryHike))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 



# outlier check in TotalWorkingYears: Outliers present
quantile(employeefinal$TotalWorkingYears,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(TotalWorkingYears))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Treatment of Outliers in TotalWorkingYears column
x<-employeefinal$TotalWorkingYears
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employeefinal$TotalWorkingYears[which(employeefinal$TotalWorkingYears>outlier_value)]<-outlier_value



# outlier check in YearsAtCompany: Outliers present
quantile(employeefinal$YearsAtCompany,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(YearsAtCompany))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Treatment of Outliers in YearsAtCompany column
x<-employeefinal$YearsAtCompany
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employeefinal$YearsAtCompany[which(employeefinal$YearsAtCompany>outlier_value)]<-outlier_value



# Outlier check in NumCompaniesWorked: No outliers
quantile(employeefinal$NumCompaniesWorked,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(NumCompaniesWorked))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

str(employeefinal)



# outlier check in YearsSinceLastPromotion: Outliers present
quantile(employeefinal$YearsSinceLastPromotion,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 

# Treatment of Outliers in YearsSinceLastPromotion column
v<-employeefinal$YearsSinceLastPromotion
qnt <- quantile(v, probs=c(.25, .75))
H <- 1.5 * IQR(v, na.rm = TRUE)
outlier_value<-qnt[2]+H
employeefinal$YearsSinceLastPromotion[which(employeefinal$YearsSinceLastPromotion>outlier_value)]<-outlier_value



# outlier check in YearsWithCurrManager: Outliers present
quantile(employeefinal$YearsWithCurrManager,seq(0,1,0.01))

plot_grid(ggplot(employeefinal, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Treatment of Outliers in YearsWithCurrManager column
x<-employeefinal$YearsWithCurrManager
qnt <- quantile(x, probs=c(.25, .75))
H <- 1.5 * IQR(x, na.rm = TRUE)
outlier_value<-qnt[2]+H
employeefinal$YearsWithCurrManager[which(employeefinal$YearsWithCurrManager>outlier_value)]<-outlier_value



# Assumptions
##############

# If Current Company is not counted in the NumCompaniesWorked
# So if the Total Working years are equal to Years at company replacing it with 0

length(which(employeefinal$TotalWorkingYears<employeefinal$YearsAtCompany))
diff_years<-employeefinal[which(employeefinal$TotalWorkingYears-employeefinal$YearsAtCompany==1),]$NumCompaniesWorked
unique(diff_years)

# If the difference is 1 replacing it with 1 such that there are only 1 and 0 values for Num companies worked if the difference is 1

employeefinal[which(employeefinal$TotalWorkingYears-employeefinal$YearsAtCompany==1),]$NumCompaniesWorked<-1
employeefinal[which(employeefinal$TotalWorkingYears==employeefinal$YearsAtCompany),]$NumCompaniesWorked<-0



# Converting the categorical variables as Factor using as.factor command.
employeefinal$EnvironmentSatisfaction <- as.factor(employeefinal$EnvironmentSatisfaction)
employeefinal$Attrition <- as.factor(employeefinal$Attrition)
employeefinal$JobSatisfaction <- as.factor(employeefinal$JobSatisfaction)
employeefinal$WorkLifeBalance <- as.factor(employeefinal$WorkLifeBalance)
employeefinal$BusinessTravel <- as.factor(employeefinal$BusinessTravel)
employeefinal$Department <- as.factor(employeefinal$Department)
employeefinal$Education <- as.factor(employeefinal$Education)
employeefinal$EducationField <- as.factor(employeefinal$EducationField)
employeefinal$Gender <- as.factor(employeefinal$Gender)
employeefinal$JobLevel <- as.factor(employeefinal$JobLevel)
employeefinal$JobRole <- as.factor(employeefinal$JobRole)
employeefinal$MaritalStatus <- as.factor(employeefinal$MaritalStatus)
employeefinal$JobInvolvement <- as.factor(employeefinal$JobInvolvement)
employeefinal$PerformanceRating <- as.factor(employeefinal$PerformanceRating)
employeefinal$TrainingTimesLastYear <- as.factor(employeefinal$TrainingTimesLastYear)
employeefinal$StockOptionLevel <- as.factor(employeefinal$StockOptionLevel)


# Derived metric #2
###################
# As each row contains NA's only in the date columns the number of leaves is the count of NA's in a row.
# Deriving new column leaves from the date columns
employeefinal$leaves <- apply(employeefinal, 1, function(x) sum(is.na(x)))

# Derived metric #3
###################
# Deriving new column averageTimeSpent from the date columns
employeefinal$averageTimeSpent<-rowMeans(employeefinal[,27:275], na.rm = TRUE, dims = 1)
employeefinal$averageTimeSpent<-as.integer(employeefinal$averageTimeSpent)
typeof(employeefinal$averageTimeSpent)

# Excluding the date columns
employeefinal[,27:275]<-NULL
str(employeefinal)



# Outlier validation in the derived columns -
#############################################

# Outlier check in leaves columns: No outliers
quantile(employeefinal$leaves,seq(0,1,0.01))
plot_grid(ggplot(employeefinal, aes(leaves))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=leaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

# Outlier check in averageTimeSpent columns: No outliers
quantile(employeefinal$averageTimeSpent,seq(0,1,0.01))
plot_grid(ggplot(employeefinal, aes(averageTimeSpent))+ geom_histogram(binwidth = 5),
          ggplot(employeefinal, aes(x="",y=averageTimeSpent))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)




#################################################################
### EXPLORATORY DATA ANALYSIS
#################################################################

# EDA for continuous variables:
###############################

str(employeefinal)

# Boxplots of Age & DistanceFromHome plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=Age, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeefinal, aes(x=Attrition,y=DistanceFromHome, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Conclusion1--the Age of the people who resgins from a company goes in the lower Age bracket
# COnclusion2--The 50-75 percentile of the person who resigns have to travel more distance from home


# Boxplots of MonthlyIncome & NumCompaniesWorked plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeefinal, aes(x=Attrition,y=NumCompaniesWorked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Conclusion 1-- the monthly income for the persons who resgins are in less bracket than who stays with the company
# Conclusion 2-- the num of company bracket is more for the person who resigned


# Boxplot of PercentSalaryHike plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=PercentSalaryHike, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"))
# Conclusion-- Percent salary hike doesn't matter much for attrition


# Boxplots of TotalWorkingYears & YearsAtCompany plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=TotalWorkingYears, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeefinal, aes(x=Attrition,y=YearsAtCompany, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Conclusion-- the less experinced people resign more 
# i.e the person who have spent less years with the company resigned more


# Boxplots of YearsSinceLastPromotion & YearsWithCurrManager plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=YearsSinceLastPromotion, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeefinal, aes(x=Attrition,y=YearsWithCurrManager, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Conclusion 1 -- the promotion bracket for the person who leaves the company
# is less than the person who stays
# conclusion 2-- the years with current manager is also less
# for the person who leaves the company (Self explanatory)


# Boxplots of leaves & averageTimeSpent plotted against attrition status
plot_grid(ggplot(employeefinal, aes(x=Attrition,y=leaves, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(employeefinal, aes(x=Attrition,y=averageTimeSpent, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
# Conclusion 1 -- the no. of leaves taken don't have much significance on Attrition
# Conclusion 2 -- the average time spent by the persons who leave the company is more than the person who stays



# Correlation between categorical variables

library(GGally)
ggpairs(employeefinal[, c("Age","YearsWithCurrManager","averageTimeSpent","leaves", "DistanceFromHome", 
                          "MonthlyIncome", "NumCompaniesWorked", "PercentSalaryHike", "TotalWorkingYears", 
                          "YearsAtCompany", "YearsSinceLastPromotion")])


# EDA for categorical variables:
###############################

# charts for categorical features with stacked attrition information:

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")


# Chart for EnvironmentSatisfaction, JobSatisfaction, WorkLifeBalance,
# Department, BusinessTravel, Education with stacked attrition information.
plot_grid(ggplot(employeefinal, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employeefinal, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=Education,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Chart for EducationField, Gender, JobLevel, JobRole, MaritalStatus,
# StockOptionLevel with stacked attrition information.
plot_grid(ggplot(employeefinal, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employeefinal, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=JobLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(employeefinal, aes(x=StockOptionLevel,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   


# Chart for JobInvolvement & PerformanceRating with attrition information.
plot_grid(ggplot(employeefinal, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1, 
          ggplot(employeefinal, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")

## CONCLUSION:
# The graphs reveal moderate significance for atrrition wrt EnvironmentSatisfaction, BusinessTravel and Education
# Strong significance with JobRole, JobLevel, EducationField, JobInvolvement and PerformanceRating.


# Scaling continous variables for modelling 

employeefinal$averageTimeSpent <- scale(employeefinal$averageTimeSpent) 
employeefinal$leaves <- scale(employeefinal$leaves) 
employeefinal$YearsWithCurrManager <- scale(employeefinal$YearsWithCurrManager)
employeefinal$YearsSinceLastPromotion <- scale(employeefinal$YearsSinceLastPromotion) 
employeefinal$YearsAtCompany <- scale(employeefinal$YearsAtCompany) 
employeefinal$TotalWorkingYears <- scale(employeefinal$TotalWorkingYears) 
employeefinal$PercentSalaryHike <- scale(employeefinal$PercentSalaryHike) 
employeefinal$NumCompaniesWorked <- scale(employeefinal$NumCompaniesWorked) 
employeefinal$MonthlyIncome <- scale(employeefinal$MonthlyIncome) 
employeefinal$DistanceFromHome <- scale(employeefinal$DistanceFromHome) 
employeefinal$Age <- scale(employeefinal$Age) 


# Converting Attrition into factors of 0 and 1.
employeefinal$Attrition <- ifelse(employeefinal$Attrition=="Yes",1,0)
str(employeefinal)


# Checking attrition rate of prospect employees: 16.08% churn rate (round off)
Attrition <- sum(employeefinal$Attrition)/nrow(employeefinal)
Attrition 

# creating a dataframe of categorical features
employeefinalchr<- employeefinal[,-c(1,5,6,9,16,17,18,20,22,23,24,27,28)]

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employeefinalchr, 
                            function(x) data.frame(model.matrix(~x-1,data =employeefinalchr))[,-1]))

# Final dataset
employeemodel<- cbind(employeefinal[,c(5,6,9,16,17,18,20,22,23,24,27,28)],dummies) 
View(employeemodel)
str(employeemodel)



##################################################################################
# MODEL BUILDING
##################################################################################


set.seed(100)
trainindices= sample(1:nrow(employeemodel), 0.7*nrow(employeemodel))
train = employeemodel[trainindices,]
test = employeemodel[-trainindices,]

model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")
summary(model_1) 

model_2<- stepAIC(model_1, direction="both")

summary(model_2)
sort(vif(model_2),decreasing = TRUE)
# AIC = 2102.9

# removing maritalstatusmarried as it las low significance and a higher VIF 

model_3 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMedical + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + TrainingTimesLastYear.x5 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_3)
sort(vif(model_3),decreasing = TRUE)
# AIC = 2103.6

# removing trainingtimeslastyearX5 as it has low significance and higher vif
model_4 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMedical + 
                 EducationField.xOther + EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_4)
sort(vif(model_4),decreasing = TRUE)
# AIC = 2104.3

# removing Educationfieldother as it has low significance and higher vif
model_5 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMedical + 
                 EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle + StockOptionLevel.x1 + 
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_5)
sort(vif(model_5),decreasing = TRUE)
# AIC = 2104.7

# removing stockoptionlevelX1 as it has low significance and higher vif
model_6 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMedical + 
                 EducationField.xTechnical.Degree + 
                 Gender + JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_6)
sort(vif(model_6),decreasing = TRUE)
# AIC = 2105.2

# removing as it has low significance and higher vif
model_7 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 + EducationField.xMedical + 
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_7)
sort(vif(model_7),decreasing = TRUE)
# AIC = 2105.4

# removing educationfieldmedical as it has low significance and higher vif
model_8 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 +
                 EducationField.xTechnical.Degree + 
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_8)
sort(vif(model_8),decreasing = TRUE)
# AIC = 2107

# removing educationfieldTechnical degree as it has low significance and higher vif
model_9 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                 EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                 JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                 WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                 Department.xSales + Education.x2 +
                 JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                 MaritalStatus.xSingle +
                 TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                 TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
               data = train[, -1])
summary(model_9)
sort(vif(model_9),decreasing = TRUE)
# AIC = 2108.5

# removing educationX2 degree as it has low significance and higher vif
model_10 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +
                  JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +
                  TrainingTimesLastYear.x1 + TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_10)
sort(vif(model_10),decreasing = TRUE)
# AIC = 2110

# removing TrainingTimesLastYearX1 as it has low significance and higher vif
model_11 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently + 
                  BusinessTravel.xTravel_Rarely + Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_11)
sort(vif(model_11),decreasing = TRUE)
# AIC = 2112.2

# removing BusinessTravelXrarely as it has low significance and higher vif
model_12 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_12)
sort(vif(model_12),decreasing = TRUE)
# AIC = 2116.6

# removing JobroleXlaboratoryTechnician as it has low significance and higher vif
model_13 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 + JobRole.xResearch.Director + 
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                  MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_13)
sort(vif(model_13),decreasing = TRUE)
# AIC = 2119.8

# removing JobroleXresearch scientist as it has low significance and higher vif
model_14 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 + JobRole.xResearch.Director + 
                  JobRole.xSales.Executive + MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_14)
sort(vif(model_14),decreasing = TRUE)
# AIC = 2123.5

# removing JobroleXsalesExecutive as it has low significance and higher vif
model_15 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 + JobRole.xResearch.Director + 
                  MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_15)
sort(vif(model_15),decreasing = TRUE)
# AIC = 2127.1

# removing JobroleXResearchDirector as it has low significance and higher vif
model_16 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 +MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 + JobInvolvement.x3, family = "binomial", 
                data = train[, -1])
summary(model_16)
sort(vif(model_16),decreasing = TRUE)
# AIC = 2128.6

# removing JobInvolvementX3 as it has low significance and higher vif
model_17 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales +JobLevel.x5 +MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train[, -1])
summary(model_17)
sort(vif(model_17),decreasing = TRUE)
# AIC = 2130.6

# removing JobLevelX5 as it has low significance and higher vif
model_18 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle +TrainingTimesLastYear.x4 + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train[, -1])
summary(model_18)
sort(vif(model_18),decreasing = TRUE)
# AIC = 2133.3

# removing TrainingTimesLastYearX4 as it has low significance and higher vif
model_19 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train[, -1])
summary(model_19)
sort(vif(model_19),decreasing = TRUE)
# AIC = 2138.1

# removing JobSatisfactionX2 as it has low significance and higher vif
model_20 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 +JobSatisfaction.x3 + 
                  JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train[, -1])
summary(model_20)
sort(vif(model_20),decreasing = TRUE)
# AIC = 2146.4

# removing JobSatisfactionX3 as it has low significance and higher vif
model_21 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle + 
                  TrainingTimesLastYear.x6 , family = "binomial", 
                data = train[, -1])
summary(model_21)
sort(vif(model_21),decreasing = TRUE)
# AIC = 2153.7

# removing trainingtimeslastyearX6 as it has low significance and higher vif
model_22 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  WorkLifeBalance.x4 + BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_22)
sort(vif(model_22),decreasing = TRUE)
# AIC = 2157.9

# removing worklifebalanceX4 as it has low significance and higher vif
model_23 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_23)
sort(vif(model_23),decreasing = TRUE)
# AIC = 2162.3

# removing worklifebalanceX2 as it has low significance and higher vif
model_24 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 + WorkLifeBalance.x3 + 
                  BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_24)
sort(vif(model_24),decreasing = TRUE)
# AIC = 2168.4

# removing worklifebalanceX3 as it has low significance and higher vif
model_25 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 +
                  BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  Department.xSales + MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_25)
sort(vif(model_25),decreasing = TRUE)
# AIC = 2177.9

# removing DepartmentXSales as it has low significance and higher vif
model_26 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 +
                  BusinessTravel.xTravel_Frequently +Department.xResearch...Development + 
                  MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_26)
sort(vif(model_26),decreasing = TRUE)
# AIC = 2189.5

# removing DepartmentXResearchDevelopement as it has low significance and higher vif
model_27 <- glm(formula = Attrition ~ NumCompaniesWorked + TotalWorkingYears + 
                  YearsSinceLastPromotion + YearsWithCurrManager + averageTimeSpent + 
                  EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                  EnvironmentSatisfaction.x4 + JobSatisfaction.x4 +
                  BusinessTravel.xTravel_Frequently + 
                  MaritalStatus.xSingle , family = "binomial", 
                data = train[, -1])
summary(model_27)
sort(vif(model_27),decreasing = TRUE)
# AIC = 2203.1


# as all the variables are significant, we can stop the modelling here.

final_model<- model_27

testpred = predict(final_model, type = "response", 
                   newdata = test[,-2])


test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)# probabilities range from 13% to 99%
test$prob<- test_pred

# Let's Choose the cutoff value. 

# Let's find out the optimal probalility cutoff 


test_actual_Attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

perform_fn <- function(cutoff) 
{
  predicted_Attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_Attrition, test_actual_Attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



# Summary of test probability

summary(test_pred)

s = seq(.01,.95,length=1000)

OUT = matrix(0,1000,3)


for(i in 1:1000)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)|
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff


# Let's choose a cutoff value of 0.183 for final model

test_cutoff_Attrition <- factor(ifelse(test_pred >=0.1831331, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_Attrition, test_actual_Attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc 
#0.7087452

sens 
#0.7027027

spec 
#0.7099726

test$predicted_Attrition<- ifelse(test$prob >=0.1831331, 1,0)

View(test)

#Since accuracy, sensitivity and specificity are almost same, so this a pretty good model.

### KS STATISTIC OF TEST DATA

test_cutoff_Attrition <- ifelse(test_cutoff_Attrition=="Yes",1,0)
test_actual_Attrition <- ifelse(test_actual_Attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_Attrition, test_actual_Attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
#0.4126753

# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_Attrition, test_pred, groups = 10)
Attrition_decile

#bucket total totalresp Cumresp      Gain  Cumlift
#<int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
 # 1      1   132        82      82  36.93694 3.693694
#2      2   131        30     112  50.45045 2.522523
#3      3   132        31     143  64.41441 2.147147
#4      4   131        23     166  74.77477 1.869369
#5      5   132        14     180  81.08108 1.621622
#6      6   131        14     194  87.38739 1.456456
#7      7   132         7     201  90.54054 1.293436
#8      8   131         2     203  91.44144 1.143018
#9      9   132        11     214  96.39640 1.071071
#10     10   131         8     222 100.00000 1.000000
> 





