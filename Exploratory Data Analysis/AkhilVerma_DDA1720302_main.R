library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(scales)
library(Hmisc)
library(gridExtra)

#### Obective - Using EDA understand and report the variables responsible for loan default.

# -----A person who is eligible for a loan and is not disbursed loan = credit loss
# -----A person who shoudnt get loan but got loan disbursed(irrespective of full/partial)= Credit risk

#### We will hence load the given loan data and indetify variables by EDA tools.
#### So we will focus on variables during loan application, to assist the company in deciding DISBURSAL or REJECTION of loan.


# loading data from provided data set
loan<- read.csv("loan.csv" , stringsAsFactors = F)

# overview of data
View(loan)
head(loan)

#structure of data
str(loan)


##----- Understanding the data -----##
# From the data set we identified below varibales to be considered for our analysis.
# What we can overview in this dataset
# 1. Variables related to demography
# 2. Variables related to the loan
# 3. Variables related to the behaviour of loantakers.


## Demographic variables: 

# emp_title 
# emp_length
# home_ownership
# annual_inc
# verification_status
# addr_state
# zip_code
# title
# purpose
# desc
# url

## Loan Characteristics Information

# loan amount
# funded amount
# funded amount invested
# interest rate
# loan status
# loan grade
# loan sub-grade
# dti
# loan issue date
# loan term
# installment

## Information after the loan is disbursed

#  delinq_2yrs
#  earliest_cr_line
#  inq_last_6mths
#  open_acc
#  pub_rec
#  revol_bal
#  revol_util 
#  total_acc
#  out_prncp 
#  out_prncp_inv
# total_pymnt"             
# total_pymnt_inv
# total_rec_prncp
# total_rec_int 
# total_rec_late_fee 
# recoveries             
# collection_recovery_fee
# last_pymnt_d
# last_pymnt_amnt
# next_pymnt_d
# last_credit_pull_d
# application_type       


# ----A person who is eligible for a loan and is not disbursed loan = credit loss
# ----A person who shoudn't get loan but got loan disbursed(irrespective of full/partial)= Credit risk

#### So we will focus on variables during loan application, to assist the company in deciding DISBURSAL or REJECTION of loan.

# Hence, variables related to behaviour of borrowers, should be excluded from our analysis, because that is the analysis
# of post loan disbursal and not at the time of loan application.


#### removing variables related to behaviour of borrowers, since they are analysed post loan disbursal and not at time of application.


removebehaviourvariables<- c( 
  "delinq_2yrs",
  "earliest_cr_line",
  "inq_last_6mths",
  "open_acc",
  "pub_rec",
  "revol_bal",
  "revol_util",
  "total_acc",
  "out_prncp",
  "out_prncp_inv",
  "total_pymnt",
  "total_pymnt_inv",
  "total_rec_prncp",
  "total_rec_int",
  "total_rec_late_fee",
  "recoveries",
  "collection_recovery_fee",
  "last_pymnt_d",
  "last_pymnt_amnt",
  "next_pymnt_d",
  "last_credit_pull_d",
  "application_type")


loan <- loan[,!(colnames(loan) %in% removebehaviourvariables)]

View(loan)


#------- Dropping variables with more than 12.5% missing values-------#

missing_values <- loan %>%
  summarise_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values,key='feature',value = 'missing_percentage')

missing_values %>%
  ggplot(aes(x=reorder(feature,-missing_percentage),y=missing_percentage)) +
  geom_bar(stat = 'identity',fill='green') +
  coord_flip()

# There are some features with redundant values, these need to be removed
# finding out relevant/good features where the missing percentage < 12.5%

x <- filter(missing_values,missing_percentage<0.125)

x <- (x$feature) 

# we should not include missing values with more than 12.5% obs
# ----------Removing all the variables which are not making any impact on the loan process. 

loan <- loan[,(colnames(loan) %in% x)]
summary(loan)


removevariables2 <- c("member_id","id","acc_now_delinq","chargeoff_within_12_mths","pymnt_plan","initial_list_status","delinq_amnt","pub_rec_bankruptcies", "tax_liens","collections_12_mths_ex_med","policy_code",
                   "url","desc","emp_title","zip_code","addr_state","title")
loan <- loan[,!(colnames(loan) %in% removevariables2)]


#count of NA values by column
loan %>%
  summarise_all(funs(sum(is.na(.))))

sum(is.na(loan))

# Interest rate is not a numeric variable , converting it to numeric.

str(loan$int_rate)

# removing "%" from variable

loan$int_rate <- str_replace_all(loan$int_rate, "%", "")

#converting it into numeric
loan$int_rate <- as.numeric(loan$int_rate)
summary(loan$int_rate)
View(loan)

# Converting issue date to default date format 

loan$issue_d <- paste("01-",loan$issue_d,sep="")
loan$issue_d <- as.Date(loan$issue_d,"%d-%B-%y")
View(loan)

# extracting numeric values from employment length

loan$emp_length <- parse_number(loan$emp_length)

# converting character variables to Factor.
loan[sapply(loan, is.character)] <- lapply(loan[sapply(loan, is.character)], 
                                           as.factor)

# we dont need "Current" Loan status 
# as we are not sure if in near future they will be a defaulter or not
# or they will miss any payment/EMI.


current_loan <- filter(loan,loan_status %in% c("Current"))
current_loan$loan_status <- factor(current_loan$loan_status)

# Let's consider Fully Paid & Charged Off levels in the loan variable
loan <- filter(loan,loan_status %in% c("Fully Paid","Charged Off"))
loan$loan_status <- factor(loan$loan_status)
summary(loan$loan_status)
# approx. 14.6% default rate 
table(loan$loan_status)[1]/nrow(loan)

# Let's change "Charged Off" level to "1" and "Fully Paid" to "0"
loan$loan_status <- ifelse(loan$loan_status=="Charged Off",1,0)
loan$loan_status

loan$year <- as.factor(format(loan$issue_d,"%Y"))


# Checking what all type of loans have most/least impact on Lending company

loan %>% group_by(purpose) %>% summarise(n())
ggplot(loan, aes(x=purpose)) + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Turns out Debt consolidation and Credit card have highest contribution


principal_by_purpose <- loan %>% group_by(purpose) %>% 
  summarise(num=n(), total_principal = sum(funded_amnt, na.rm=T)) %>% 
  arrange(desc(total_principal))

principal_by_purpose

ggplot(principal_by_purpose, aes(x=purpose, y=total_principal))+
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

principal_by_purpose$percentage <- 
  round(principal_by_purpose$total_principal / sum(principal_by_purpose$total_principal), 2)
principal_by_purpose


# -----Actual share of specific loan in revenue generation of lending company
#------
#---
#Debt consolidation (53%)
#credit cards (14%)
#home improvement (8%)
#small business (5%)
#Also these 4 loans covers 80% of total revenue generation.]

###----- so it makes sense if we cover these loans in our further analysis




# Default rate across products (purpose)

# segmented univariate analysis
default_rates = loan %>% group_by(loan$purpose) %>% 
  summarise(avg_default = round(mean(loan_status), 2)) %>%
  arrange(desc(avg_default))
default_rates

#ANALYSIS------------------------
# small business has 27% default rate
# debt-consolidation 15% default rate
# home improvement 12% default rate
# credit cards 11% default rate

# Debt consolidation contibutes the highest credit loss
# Credit cards are the second highest, home improvement the third highest

# Moving forward, these few product types should be the focus of analysis
# Because reducing the debt-consolidatiion default rate
# will lead to a larger reduction in credit loss than other categories of loan purpose

loan$purpose <- as.character(loan$purpose)
loan <- filter(loan, 
               purpose == "debt_consolidation" | 
                 purpose == "credit_card" | 
                 purpose == "home_improvement")

loan$purpose <- factor(loan$purpose)
summary(loan$purpose)



###-------univariate analysis--------###
## making a function to generalise the univariate analysis based on different variables

univariate_categorical <- function(dataset,var,var_name){
  
  dataset %>% ggplot(aes(x = as.factor(var))) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + 
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = var_name, y = "Percent", x = var_name)+theme(
      axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank()
    ) 
}


###---- Demographic variables for analysis 

# employment length
univariate_categorical(loan,loan$emp_length,"Emp Length")


#ANALYSIS------------------------
# approx. 40% applicants have either 1 year work experience or about 10 years work experience 

ggplot(loan, aes(x=emp_length))+geom_bar()+facet_wrap(~purpose)

#ANALYSIS------------------------
# Debt consolidation and Credit Cards have a significant number of freshers (< 1 year exp) applying, who are expectedly 
# less in home improvement
# also it makes sense that 1 year work experence wont be having their own home hence less in number#home improvement

# facet for categorical vars
categorical_bivariate <- function(dataset, var, var_name){
  plot_bi = ggplot(dataset, aes(x=var))+geom_bar()+facet_wrap(~loan$purpose)
  return(plot_bi)
}

# facet for continuous vars
continuous_bivariate <- function(dataset, var, var_name){
  plot_cont = ggplot(dataset, aes(x=var))+geom_histogram()+facet_wrap(~loan$purpose)
  return(plot_cont)
}

categorical_bivariate(loan, loan$emp_length, "emp_length")

# Home ownership
categorical_bivariate(loan, loan$home_ownership, "home_ownership")
# most are mortgage and rent, roughly similar across the three categories

# annual income
continuous_bivariate(loan, loan$annual_inc, "annual_income")
#contains outliers
summary(loan$annual_inc)


loan %>% group_by(purpose) %>% summarise(mean(annual_inc))
# CC and debt consolidation have low applicants than home improvement
# CC - 70329.5 Annual income average
# Debt consolidation - 67078.5 annual income average
# home improvement - 89497.2 annual income average

# debt to income ratio
continuous_bivariate(loan, loan$dti, "DTI")
# we can compare the average DTI 
loan %>% group_by(purpose) %>% summarise(mean(dti))
# CC and debt consolidation have same average 


#----------------Loan characteristics

#Purpose of loan 
univariate_categorical(loan,loan$purpose,"Purpose Distribution")
# we have already seen this

# ---------------Term Distribution
univariate_categorical(loan,loan$term,"Term Distribution")
# 36 months is the more common type
categorical_bivariate(loan, loan$term, "term")
# 60 months is not approved either by lending company or people are not interested in taking 60months tenure for Home_improvement


#Grade Distribution
univariate_categorical(loan,loan$grade,"Grade Distribution")
# more than 50% of people have Grade A or Grade B , which is a good sign.
# Maximum people are having Grade B i.e 30%

categorical_bivariate(loan, loan$grade, "grade")

# pattern is same in all three loan types i.e. maximum loan takers are either Grade A or Grade B.
# Home_improvement Grade C loan takers are higher in number

#Sub-Grade Distribution
univariate_categorical(loan,loan$sub_grade,"Sub-Grade Distribution")
# Highest is Grade B3.

# Loan amount
continuous_bivariate(loan, loan$loan_amnt, "amnt")
loan %>% group_by(purpose) %>% summarise(median(loan_amnt))
# There are outliers in all 3 of loan
# However average of Credit card = 10k
# average of debt consolidation = 11k
# average of Home Improvement = 9.6K


# Interest rate
continuous_bivariate(loan, loan$int_rate, "Interest")
loan %>% group_by(purpose) %>% summarise(mean(int_rate))
# debt consolidation has slightly higher interest rate as compared to other two.
# 12.4% approx (debt consolidation)
# 11.6% approx for Credit card
# 11.3% approx for Home improvement

# Installment
continuous_bivariate(loan, loan$installment, "Instalment")
loan %>% group_by(purpose) %>% summarise(mean(installment))
# slightly lower monthly instalments in home_improvement as compared to other two.


# Verification status distribution
univariate_categorical(loan,loan$verification_status,"verification_status Distribution")
# 41% not verified loan takers and are highest among verification category

categorical_bivariate(loan, loan$verification_status, "verfication")
loan %>% group_by(purpose) %>% summarise(n())

# verified percentages
# credit cards 
tapply(loan$verification_status, loan$purpose, summary)[[1]]/tapply(loan$verification_status, loan$purpose, length)[1]
# 44.6% for Not verified
# 23.5% for source verified
# 31.8% for verified source

# debt
tapply(loan$verification_status, loan$purpose, summary)[[2]]/tapply(loan$verification_status, loan$purpose, length)[2]
# 40.0% for Not verified
# 24.6% for source verified
# 35.3% for verified source

# home improvement
tapply(loan$verification_status, loan$purpose, summary)[[3]]/tapply(loan$verification_status, loan$purpose, length)[3]


#ANALYSIS------------------------
# Credit card loans have highest fraction of not verfied, followed by home ownership and debt
# Loan characteristic variables summary:
# Home improvement loans are relatively smaller loan amounts, lower installments and 
# lower interest rates
# Also, home improvement applicants have higher incomes and lower DTIs
# compared to CCs and debt 


# Let's see the correlation values of the numerical variables 
continous_var <- names(loan)[sapply(loan, class) != "factor"]

continous_data <- loan[,(colnames(loan) %in% continous_var)]

# Also, removing loan_status variable. it is a discrete variable
continous_data$loan_status <- NULL

# Also, removing date variable
continous_data$issue_d <- NULL

corr <- cor(continous_data)

# Plot the correlation matrix
corrplot(corr, method="number")


#ANALYSIS------------------------
# Loan amount, funded amount and funded amount invested are hightly correlated. 
# Which is expected, because funded amount can be less than or equal to the loan amount applied by the applicant. 
# And most of the time, full amount has been granted by the company, Thus these variables are highly correlated. 
# One good insight is that the loan amount and annual income are having 26% positive correlation which implies the 
# loan amount requested by applicant has been decided on the basis of his/herannual income


continuous_dist <- function(dataset,con_var,var_name){
  
  con_summary <- tapply(con_var, loan$purpose, summary) 
  
  P1 <- dataset %>% ggplot(aes(x=con_var)) + geom_line(stat = 'density',color='red')+facet_wrap(~purpose)+ggtitle(var_name)+xlab(var_name)
  
  return(list(con_summary,P1))
}

#########################################################################################

#1. loan_amnt # ignoring the rest amount as all the other variables are correlated to each.
continuous_dist(loan,loan$loan_amnt,"loan Distribution")

#2. int_rate
continuous_dist(loan,loan$int_rate,"Interest Rate Distribution")


#########################################################################################

#5. Annual income Distribution

continuous_dist(loan,loan$annual_inc,"Annual Income Distribution")


#################################################################################################

#5. DTI 

continuous_dist(loan,loan$dti,"DTI Distribution")

## On what basis are the loans approved or rejected? 
# And how are interest rates, installment amount etc. decided?



########  Loan characteristics
#####
#0. Purpose of loan 
univariate_categorical(loan,loan$purpose,"Purpose Distribution")
# we have already seen this

#1. Term Distribution
univariate_categorical(loan,loan$term,"Term Distribution")
# 36 months is the more common type
categorical_bivariate(loan, loan$term, "term")
# no 60 months in home improvement


#2. Grade Distribution
univariate_categorical(loan,loan$grade,"Grade Distribution")
categorical_bivariate(loan, loan$grade, "grade")

# #3. Sub-Grade Distribution
# univariate_categorical(loan,loan$sub_grade,"Sub-Grade Distribution")

# 4. Loan amount
continuous_bivariate(loan, loan$loan_amnt, "amnt")
loan %>% group_by(purpose) %>% summarise(median(loan_amnt))
# few loans  in home improvement exceed 20k dollars 

# Interest rate
continuous_bivariate(loan, loan$int_rate, "Interest")
loan %>% group_by(purpose) %>% summarise(mean(int_rate))
# debt consolidation has slightly higher interest rates

# Installment
continuous_bivariate(loan, loan$installment, "Instalment")
loan %>% group_by(purpose) %>% summarise(mean(installment))
# slightly lower monthly instalments in home_improvement


# Verification status distribution
univariate_categorical(loan,loan$verification_status,"verification_status Distribution")
prop.table(table(loan$verification_status))
# total 41% not verified

categorical_bivariate(loan, loan$verification_status, "verfication")
loan %>% group_by(purpose) %>% summarise(n())

# verified percentages
# credit cards 
tapply(loan$verification_status, loan$purpose, summary)[[1]]/tapply(loan$verification_status, loan$purpose, length)[1]

# debt
tapply(loan$verification_status, loan$purpose, summary)[[2]]/tapply(loan$verification_status, loan$purpose, length)[2]

# home improvement
tapply(loan$verification_status, loan$purpose, summary)[[3]]/tapply(loan$verification_status, loan$purpose, length)[3]

# Credit card loans have highest fraction of not verfied, followed by home ownership and debt

# Loan characteristic variables summary:
# Home improvement loans are relatively smaller loan amounts, lower installments and 
# lower interest rates
# Also, home improvement applicants have higher incomes and lower DTIs (lesser financial distress)
# compared to CCs and debt 



######## Segmented Univariate Analysis - Comparing default rates across variables########
# Analysing default rates separately for the three products
loan %>% group_by(purpose) %>% summarise(mean(loan_status))
# debt 15%, home 12%, CC 10%


segmented_defaults <- function(dataset,cat_var, var_name){
  a <- aggregate(loan_status~cat_var, dataset, mean)
  b <- data.frame(prop.table(table(cat_var))*100)
  b[,2] <- paste(round(b[,2], 2), "%", sep="")
  colnames(a)[1] <- var_name
  colnames(b)[1] <- var_name
  agg_default <- merge(a, b, by = var_name)
  agg_default <- data.frame(agg_default)
  colnames(agg_default) <- c(var_name, "Default","count")
  agg_default[, 2] <- round(agg_default[, 2], 2)
  agg_default <- arrange(agg_default, desc(Default))
  
  p.plot <- ggplot(agg_default, aes(agg_default[, 1], Default, label = count)) +
    geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_text(size = 3, vjust = -0.5) + xlab(var_name) 
  
  return(list((agg_default[1, 2] - agg_default[nrow(agg_default), 2]),p.plot))
  
}

# -------------------Demographic variables

# Employment length
# Creating three separate data frames
credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

# credit
segmented_defaults(credit, credit$emp_length, "Emp Length")
segmented_defaults(debt, debt$emp_length, "Emp Length")
segmented_defaults(home, home$emp_length, "Emp Length")

grid.arrange(segmented_defaults(credit, credit$emp_length, "Emp Length")[[2]],
             segmented_defaults(debt, debt$emp_length, "Emp Length")[[2]], 
             segmented_defaults(home, home$emp_length, "Emp Length")[[2]], 
             ncol = 3)

#ANALYSIS------------------------
# In credit, medium work exp and 10 years applicants tend to default more 
# In debt, 10 years work exp default is extremely high
# In home imp, ~ 1 years work exp default more

## home ownership
grid.arrange(segmented_defaults(credit, credit$home_ownership, "Home")[[2]],
             segmented_defaults(debt, debt$home_ownership, "Home")[[2]], 
             segmented_defaults(home, home$home_ownership, "Home")[[2]], 
             ncol = 3)


#ANALYSIS------------------------
# In credit and debt, there's almost no effect
# In home improvement, the 9% applicants who have rented houses default 15% 
# (the avg default of home-imp is only 12%)
# seems like people who live in rented houses tend to default, which makes sense
# One wouldn't be as willing to improve someone else's house 

## annual income
# we should bin the annual income into categories for better plots
summary(loan$annual_inc)


loan$binned_income = factor(cut(loan$annual_inc, breaks = seq(0, 140000, 20000)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_income, "Income")[[2]],
             segmented_defaults(debt, debt$binned_income, "Income")[[2]], 
             segmented_defaults(home, home$binned_income, "Income")[[2]], 
             ncol = 3)
median(loan$annual_inc)


#ANALYSIS------------------------
# 60k is the median income
# In credit cards, income between 20k and 40k dollars default the highest
# In debt, low income groups (< 60k) default more than 15%
# In home imp, low income groups (< 60k) default more than 15%

# DTI
summary(loan$dti)
loan$binned_dti = factor(cut(loan$dti, breaks = seq(0, 30, 5)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_dti, "DTI")[[2]],
             segmented_defaults(debt, debt$binned_dti, "DTI")[[2]], 
             segmented_defaults(home, home$binned_dti, "DTI")[[2]], 
             ncol = 3)
mean(loan$dti)

#ANALYSIS------------------------
# In credit, DTI > 15 is high risk (> 10% default)
# In debt, DTI > 15 is > 15% default rate
# In home imp, DTI > 15 is > 12.5% default


### Loan variables

# grade
grid.arrange(segmented_defaults(credit, credit$grade, "grade")[[2]],
             segmented_defaults(debt, debt$grade, "grade")[[2]], 
             segmented_defaults(home, home$grade, "grade")[[2]], 
             ncol = 3)



# loan amount
summary(loan$loan_amnt)
loan$binned_amnt = factor(cut(loan$loan_amnt, breaks = seq(0, 35000, 5000)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_amnt, "amount")[[2]],
             segmented_defaults(debt, debt$binned_amnt, "amount")[[2]], 
             segmented_defaults(home, home$binned_amnt, "amount")[[2]], 
             ncol = 3)
#----- it seems there is no trend

# int rate
summary(loan$int_rate)
loan$binned_int = factor(cut(loan$int_rate, breaks = seq(5, 25, 2)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_int, "int")[[2]],
             segmented_defaults(debt, debt$binned_int, "int")[[2]], 
             segmented_defaults(home, home$binned_int, "int")[[2]], 
             ncol = 3)


#ANALYSIS------------------------
# In debt and home imp, there's a clear upward trend
# In credit, it is not that clear

summary(loan$installment)
loan$binned_instalment = factor(cut(loan$installment, breaks = seq(20, 1400, 100)))

credit = filter(loan, loan$purpose == "credit_card")
debt = filter(loan, loan$purpose == "debt_consolidation")
home = filter(loan, loan$purpose == "home_improvement")

grid.arrange(segmented_defaults(credit, credit$binned_instalment, "instalment")[[2]],
             segmented_defaults(debt, debt$binned_instalment, "instalment")[[2]], 
             segmented_defaults(home, home$binned_instalment, "instalment")[[2]], 
             ncol = 3)


# cross check
grid.arrange(segmented_defaults(credit, credit$verification_status, "verification")[[2]],
             segmented_defaults(debt, debt$verification_status, "verfication")[[2]], 
             segmented_defaults(home, home$verification_status, "verification")[[2]], 
             ncol = 3)

write.csv(loan, file = "loanfinaldata.csv")

