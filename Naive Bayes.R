empdata<-read.csv("Employee_data.csv")
colnames(empdata)
Sal<-ifelse(empdata$Emp_Sal==">50K","High","Low")
empdata<-data.frame(empdata,Sal)
View(empdata)
library(caret)
require(ggplot2)
require(lattice)
empdata<-empdata[,-15]

set.seed(2)
id<-sample(2,nrow(empdata),prob = c(0.7,0.3),replace = T)
emptrain<-empdata[id==1,]
View(emptrain)

emptest<-empdata[id==2,]
colnames(empdata)
View(emptest)


library(e1071)
emp_nb<-naiveBayes(Sal~ Age_Of_emp + Edu_of_Emp + capital_gain,data = emptrain)
emp_nb<-naiveBayes(Sal~ Age_Of_emp + Emp_Stat_type + Occ_Of_Emp + Edu_of_Emp + Edu_Cat + Work_hour_in_week + country_of_res,data = emptrain)
emp_nb<-naiveBayes(Sal~.,data= emptrain)
emp_nb

preee3<-predict(emp_nb,emptest)
confusionMatrix(table(preee3,emptest$Sal))
preee3

