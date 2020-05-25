install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggmap")
install.packages("maps")
library(lubridate)
library(tidyverse)s
library(ggplot2)
library(dplyr)
library(ggmap)
library(maps)
# Import thd data file 
data<-read.csv("./train.csv")
str(data)
#Converting Dates columt to date format as it is a factor
data$Date<-as.Date(data$Dates,format= "%Y-%m-%d")
data$Time <- format(as.POSIXct(data$Dates,format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
data<-subset(data, select= -Dates)
str(data)
# Subsetting data based on date
data_date<-subset(data2, Date>"2013-12-31" & Date<"2014-12-31")
nrow(data_date)

# Creating a csv file with the filetered data
write.csv(data_date,file="data_group5.csv")

# Count based on category 
nrow(data)
data <- read.csv("data_group5.csv")
category <- as.data.frame(table(data$Category))
reso<-as.data.frame(table(data$Resolution))
pdist<-as.data.frame(table(data$PdDistrict))
dayweek<-as.data.frame(table(data$DayOfWeek))
descript<-as.data.frame(table(data$Descript))
address<-as.data.frame(table(data$Address))
datecount<-as.data.frame(table(data$Date))
hour<-as.data.frame(table(data$Time))
num_crime_date<-as.data.frame(table(data$Date))
names(category) <- c("Category", "Frequency")
# plotting histogram for number of crimes 
ggplot(crime,aes(x=Category)) + geom_bar() + plot_theme + xlab("Crime Category") + ylab("Number of Crimes")
?ggplot
str(category)
# Histogram based on crime Category
ggplot(data=category,aes(x= Category, y=Frequency))+ geom_bar(stat = "identity") + ylab("Number of  Crimes") + xlab("Crime Category") + theme(axis.text.x = element_text(angle=60,hjust = 1,vjust = 0.98 ))
# Outcome: #Larcenecy/Theft, Other Offences, Assault & Non Criminal were major crimes
# Histogram based on day of week
dayweek2<-ordered(dayweek$Var1,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
ggplot(dat=dayweek, aes(x=Var1,y=Freq ))+geom_bar(stat="identity")+ ylab("Number of Crime")+xlab("")
#Friday and Saturday had most crime cases and lowest on sunday
# Histogram of crime by police district
ggplot(dat=pdist, aes(x=Var1,y=Freq ))+geom_bar(stat="identity")+ ylab("Number of Crime")+xlab("")  + theme(axis.text.x = element_text(angle=60,hjust = 1,vjust = 0.98 ))
#  Outcome: Most crimes committed in southern district and least in Richnomd
# Histogram of Resolution
ggplot(data=reso, aes(x=Var1,y=Freq ))+geom_bar(stat="identity")+ ylab("Number of Crime")+xlab("")  + theme(axis.text.x = element_text(angle=60,hjust = 1,vjust = 0.98 )) 
# 
head(data$Time)
# Box plot of Crimes happening during hour of day
head(data)
# Group by - Ask Mohit 
?strftime
?strptime
data$Hour = strftime(strptime(data$Time,"%H:%M:%S"),"%H")
data_box= data %>%
          group_by(Hour,Date)%>%
          summarise(count=n())
?group_by
?geom_boxplot
ggplot(data = data_box, aes(x=Hour,y=count, fill=Hour)) + geom_boxplot(alpha=0.5) + xlab("Hour of the day")+ ylab("Number of crimes") + theme(legend.position = "none" )
# There is a dip in crime in the midnight. The crime rate increaces during the day 
# with most crimes in the evening between 5 to 7 pm
##########
## Read data.

#data <- read.csv("sf_crime.csv")

dim(data)

head(data)
#?dim
#summary(dta)

n <- nrow(data)

## Separate out and filter the ’Category’ and ’PdDistrict’ variables.

category2 <- data$Category

district2 <- data$PdDistrict

tt <- table(category2)

oo <- order(tt, decreasing = TRUE)

ii_keep <- (1:n)[category2 %in% names(tt)[oo][1:12]]

category2 <- factor(as.character(category2)[ii_keep])

district2 <- factor(as.character(district2)[ii_keep])

## Joint and marginal distributions.
#Joint Probabiliy of category and cdistrict
f_cat_dis <- prop.table(table(category2, district2))
write.csv(f_cat_dis,file="jointprob.csv")
f_cat <- rowSums(f_cat_dis)
write.csv(f_cat,file="marginal_Category.csv")
str(f_cat)
as.data.frame(f_cat)
g_cat<-sum(f_cat)
#
f_dis <- colSums(f_cat_dis)
write.csv(f_dis,file="marginal_dist.csv")
as.data.frame(f_dis)
h_dis<-sum(f_dis)
# Joint probability independence property
#f(x,y)=g(x)*h(y)
as.data.frame(f_cat_dis)
f_cat_dis_sum<-sum(f_cat_dis)
# Hence verified
# Conditional distributions of ’category’ given ’district’.
# Using the  formula 
# P(B|A)= p(A & B)/ P(A)
?t
f_cat_given_dis <- t(f_cat_dis) / f_dis
write.csv(f_cat_given_dis,file="conditional_cat_given_district.csv")
f_dis_given_cat <- f_cat_dis / f_cat
write.csv(f_dis_given_cat,file="conditional_district_given_category.csv")
#Consider a randomly-selected incident from this population:
#(a) What is the probability the incident was drug / narcotic-related and occurred in the Tenderloin district? 
#(b)What is the probability the incident occurred in the Tenderloin district, regardless of the incident category? Answer to four significant figures.
#(c)Given that a crime occurred in the Richmond district, what is the probability it was drug / narcotic-related? Answer to four significant figures.
#(d)Are Category and PdDistrict independent?
# Scatter plot for crimes each month
?strftime
data$Month = strftime(strptime(data$Date,"%Y-%m-%d"),"%m")
data_Scatter= data %>%
  group_by(Month)%>%
  summarise(count=n())

plot(data_Scatter$Month,data_Scatter$count,
     xlab = "Month",
     ylab = "Number of crimes committed")
#
num_crime_date$crime_rate_hour<-num_crime_date$Freq/24
hist(num_crime_date$crime_rate_hour)
##The goodness-of-fit
#Calculate statistics of crime rate per hour
descdist(num_crime_date$crime_rate_hour)

#Get paramter estimates for normal distribution
fit_n <- fitdist(num_crime_date$crime_rate_hour, "norm")
summary(fit_n)

#Get paramter estimates for log-normal distribution
fit_ln <- fitdist(num_crime_date$crime_rate_hour, "lnorm")
summary(fit_ln)


#Goodness-of-Fit Plots for normal distribution
par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour', xlegend = 'topleft')
cdfcomp (list(fit_n), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')
qqcomp (list(fit_n), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')
ppcomp (list(fit_n), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')

##Goodness-of-Fit Plots for log-normal distribution
par(mfrow=c(2,2))
plot.legend <- c("lnorm")
denscomp(list(fit_ln), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour', xlegend = 'topleft')
cdfcomp (list(fit_ln), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')
qqcomp (list(fit_ln), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')
ppcomp (list(fit_ln), legendtext = plot.legend, xlab = 'num_crime_date$crime_rate_hour')
## Hypothesis Testing ##
# The average crime rate in San Francisco is = 17.116 crimes /hour for the year 2014. It is known
# that the specific region____ has higher crime rate than the entire SF. If a sample of ___ is taken
# Does it exceed the five average with 5% significance level.
avg_CR<- sum(num_crime_date$crime_rate_hour)/nrow(num_crime_date)
## Hypothesis Testing ##
# The average crime rate in San Francisco is = 17.116 crimes /hour for the year 2014. It is known
# that the specific region____ has higher crime rate than the entire SF. If a sample of ___ is taken
# Does it exceed the five average with 5% significance level.
avg_CR<- sum(num_crime_date$crime_rate_hour)/nrow(num_crime_date)
# Average crime rate in Southern police District
#H0 - 
#H1
data$PdDistrict
  head(data)
data_south<-data[which(data$PdDistrict=='SOUTHERN'),]
head(data_south)
nrow(data_south)
southern_crime_day<-table(data_south$Date)
nrow(southern_crime_day)
str(southern_crime_day)
southern_crime_day<-as.data.frame(southern_crime_day)
southern_crime_day
southern_crime_day$Crime_Rate<-(southern_crime_day$Freq)/24
southern_crime_day
sum(southern_crime_day$Freq)
Average_CR_Southern<-sum(southern_crime_day$Crime_Rate)/nrow(southern_crime_day)
#
n=nrow(num_crime_date)
var=var(num_crime_date$crime_rate_hour)
z = (Average_CR_Southern - avg_CR) / (sqrt(var/(n)))
head(data)
###################
# for 
data$PdDistrict
head(data)
data_Richmond<-data[which(data$PdDistrict=='RICHMOND'),]
head(data_Richmond)
nrow(data_Richmond)
Richmond_crime_day<-table(data_Richmond$Date)
nrow(Richmond_crime_day)
str(Richmond_crime_day)
Richmond_crime_day<-as.data.frame(Richmond_crime_day)
Richmond_crime_day
Richmond_crime_day$Crime_Rate<-(Richmond_crime_day$Freq)/24
Richmond_crime_day
sum(Richmond_crime_day$Freq)
Average_CR_Richmond<-sum(Richmond_crime_day$Crime_Rate)/nrow(Richmond_crime_day)
#
n=nrow(num_crime_date)
var=var(num_crime_date$crime_rate_hour)
sqrt(var)
z2 = (Average_CR_Richmond - avg_CR) / (sqrt(var/(n)))
head(data)
#
#T Test 
t.test(Richmond_crime_day$Crime_Rate, mu =avg_CR, alternative = "greater") 


