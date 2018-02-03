library(stringr)
library(ggplot2)
uberdata<-read.csv("uber request data.csv")
View(uberdata)


#checking if there are any duplicate values.
sum(duplicated(uberdata$Request.id))

#checking missing values in respective columns that needs to be evaluated
sum(is.na(uberdata$Request.id))
sum(is.na(uberdata$Pickup.point))
sum(is.na(uberdata$Status))
sum(is.na(uberdata$Request.timestamp))


#making Time format consistent throughout column
#as the data/time formulaes are giving errors

uberdata$req <- str_replace_all(uberdata$Request.timestamp, "[/]", "-")
uberdata$drop <- str_replace_all(uberdata$Drop.timestamp, "[/]", "-")

str(uberdata)

#converting time format into standard Date Time format i.e. "YYYY-DD-MM HH:MM:SS" for further analysis

uberdata$req <- as.POSIXlt(uberdata$req , format = "%d-%m-%Y %H:%M")
uberdata$drop <- as.POSIXlt(uberdata$drop , format = "%d-%m-%Y %H:%M")

str(uberdata)

#extracting hour and day of request time.
uberdata$reqhour <- format(uberdata$req , "%H")
uberdata$reqday <- format(uberdata$req , "%d")

#plotting the data for 5 days as nrow=5
Plot1 <- ggplot(uberdata, aes(x = as.factor(reqhour),fill = Status))+geom_bar(position = "dodge")
Plot1 + facet_wrap( ~ uberdata$reqday, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Status" )

Plot2 <- ggplot(uberdata, aes(x = as.factor(reqhour),fill = Pickup.point))+geom_bar(position = "dodge")
Plot2 + facet_wrap( ~ uberdata$reqday, nrow =5, ncol = 1) + labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )

#plotting hour vs no of trips in a day
ggplot(uberdata, aes(x = as.factor(reqhour),fill = Pickup.point))+geom_bar(position = "dodge")+labs(x = "Hour", y = "Number of Requests", fill = "Pickup Point" )


#converting reqhour to numeric
uberdata$reqhour <- as.numeric(uberdata$reqhour)
#making slots by number of trips as per timings
uberdata$timeslot = ifelse(uberdata$reqhour <= 4,
"PreMorning", ifelse(uberdata$reqhour <= 8,"Morning",
                     ifelse(uberdata$reqhour <= 12,"Day",
                            ifelse(uberdata$reqhour <= 16,"Afternoon",
                                   ifelse(uberdata$reqhour <= 20,"Evening",
                                          ifelse(uberdata$reqhour<=24,"LateNight"))))))


#number of trips made in each timeslot
nrow(subset(uberdata, uberdata$timeslot == "PreMorning"))
nrow(subset(uberdata, uberdata$timeslot == "Morning"))
nrow(subset(uberdata, uberdata$timeslot == "Day"))
nrow(subset(uberdata, uberdata$timeslot == "Afternoon"))
nrow(subset(uberdata, uberdata$timeslot == "evening"))
nrow(subset(uberdata, uberdata$timeslot == "LateNight"))

#plotting the graph between timeslot and status of request
ggplot(uberdata, aes(x = as.factor(timeslot), fill= as.factor(uberdata$Status))) + geom_bar()+labs(x = "Time Slot", y = "Number of Requests", fill = "Status" )


#Issue Number 1 - High cancellations in the morning
uberdata1 <- subset(uberdata,timeslot=="Morning")
ggplot(uberdata1, aes(x = as.factor(Pickup.point), fill= as.factor(uberdata1$Status))) + geom_bar() +labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )

#Problem in pick up points by location i.e. Airport or City
nrow(subset(uberdata1, uberdata1$Pickup.point == "Airport" & uberdata1$Status == "Cancelled"))
nrow(subset(uberdata1, uberdata1$Pickup.point == "City" & uberdata1$Status == "Cancelled"))

#percentage calculation
uberdata_issue1 <- subset(uberdata1, Pickup.point %in% "City")
ggplot(uberdata_issue1, aes(x = uberdata_issue1$Pickup.point, fill= as.factor(uberdata_issue1$Status))) + geom_bar() + coord_polar(theta = "y", start=0)+ labs( y = "Number of Requests", x = "", fill = "Status")

#Supply and Demand of Uber Rides/requests
nrow(subset(uberdata1, uberdata1$Pickup.point == "City" & uberdata1$Status == "Trip Completed"))
nrow(subset(uberdata1, uberdata1$Pickup.point == "City"))

#Issue Number 2
uberdata2 <- subset(uberdata,timeslot=="Evening")
ggplot(uberdata2, aes(x = as.factor(Pickup.point), fill= as.factor(uberdata2$Status))) + geom_bar()+labs(x = "Pickup Point", y = "Number of Requests", fill = "Status" )


#problem in pickup points by location
nrow(subset(uberdata2, uberdata2$Pickup.point == "Airport" & uberdata2$Status == "No Cars Available"))
nrow(subset(uberdata2, uberdata2$Pickup.point == "City" & uberdata2$Status == "No Cars Available"))

#percentage calculation
uberdataissue2 <- subset(uberdata2, Pickup.point %in% "Airport")
ggplot(uberdataissue2, aes(x = uberdataissue2$Pickup.point, fill= as.factor(uberdataissue2$Status))) + geom_bar() + coord_polar(theta = "y", start=0) + labs( y = "Number of Requests", x = "", fill = "Status")

#Supply and Demand of Uber rides/requests
nrow(subset(uberdata2, uberdata2$Pickup.point == "Airport" & uberdata2$Status == "Trip Completed"))
nrow(subset(uberdata2, uberdata2$Pickup.point == "Airport"))

write.csv(uberdata, file = "uberfinaldata.csv")
