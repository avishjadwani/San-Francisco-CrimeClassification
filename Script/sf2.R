library(readr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(fitdistrplus)
library(RColorBrewer)
# Import thd data file 
data<-read.csv("train.csv")
str(data)
#Converting Dates columt to date format as it is a factor
data$Date<-as.Date(data$Dates,format= "%Y-%m-%d")
data$Time <- format(as.POSIXct(data$Dates,format="%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
data<-subset(data, select= -Dates)
str(data)
# Subsetting data based on date
data_date<-subset(data, Date>"2013-12-31" & Date<"2014-12-31")
nrow(data_date)

# Creating a csv file with the filetered data
write.csv(data_date,file="data_group5.csv")
# Import thd data file 
data <- read.csv("data_group5.csv")

##count number of crimes
#1.Count based on category 
category <- data$Category %>% 
  table() %>% 
  data.frame() %>% 
  rename('Category'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

#plot histogram of number of crimes each category
ggplot(category) + 
  geom_histogram(aes(x=reorder(Category,Frequency), y=Frequency),fill = "#5497C7",stat='identity')+
  coord_flip()+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")+
  ggtitle("Number of crimes in individual category")+
  ylab("Number of crimes")+
  xlab("Category of crime") 
# Outcome: #Larcenecy/Theft, Other Offences, Assault & Non Criminal were major crimes

#2. Count based on resolution
resolution <- data$Resolution %>% 
  table() %>% 
  data.frame() %>% 
  rename('Resolution'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

#plot histogram of number of crimes each resolution
ggplot(resolution) + 
  geom_histogram(aes(x=reorder(Resolution,Frequency), y=Frequency),fill="#E79315",stat='identity')+
  coord_flip()+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")+
  ggtitle("Number of crimes in individual resolution")+
  ylab("Number of crimes")+
  xlab("Resolution of crime") 
#outcome: Most crimes are not be solved. Of all the cases solved, most get arrest booked.

#3. pdDistrict count
pddistrict <- data$PdDistrict %>% 
  table() %>% 
  data.frame() %>% 
  rename('PdDistrict'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

#plot histogram of number of crimes each police district
ggplot(pddistrict) + 
  geom_histogram(aes(x=reorder(PdDistrict,Frequency), y=Frequency, fill = PdDistrict),stat='identity')+
  coord_flip()+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")+
  scale_fill_brewer(palette = "Spectral")+
  ggtitle("Number of crimes in each PdDistrict")+
  ylab("Number of crimes")+
  xlab("PdDistrict of crime")
#  Outcome: Most crimes committed in southern district and least in Richnomd

#The top five crime categories in Southern district
category_S= data %>%
  filter(PdDistrict=='SOUTHERN')%>% 
  group_by(Category)%>%
  summarise(count=n()) %>% 
  arrange(desc(count))
category_5 <- category_S[1:5,]
#Plot histogram of number of top five crimes 
ggplot(category_5) + 
  geom_bar(aes(x=reorder(Category,count), y=count),fill="#74C58D",stat='identity')+
  coord_flip()+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")+
  ggtitle("Number of crimes in Southern district")+
  ylab("Number of crimes")+
  xlab("Category of crime")

#4.dayweek count
dayofweek <- data$DayOfWeek %>% 
  table() %>% 
  data.frame() %>% 
  rename('DayOfWeek'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

#plot histogram of number of crimes each day of week
ggplot(dayofweek) + 
  geom_histogram(aes(x=reorder(DayOfWeek,Frequency), y=Frequency, fill = DayOfWeek),stat='identity')+
  coord_flip()+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")+
  scale_fill_brewer(palette = "Paired")+
  ggtitle("Number of crimes in each day of week")+
  ylab("Number of crimes")+
  xlab("Day of week of crime")
# Outcome: Friday and Wednesday had most crime cases and lowest on Thursday

#5.Crimes during hour of day
data$Hour = strftime(strptime(data$Time,"%H:%M:%S"),"%H")
data_box= data %>%
  group_by(Hour,Date)%>%
  summarise(count=n())

# Box plot of Crimes happening during hour of day
ggplot(data = data_box, aes(x=Hour,y=count, fill=Hour)) + 
  geom_boxplot(alpha=0.5) + xlab("Hour of the day")+ 
  ylab("Number of crimes") + 
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")
#Outcome: There is a dip in crime in the midnight. The crime rate increaces during the day 
# with most crimes in the evening between 5 to 7 pm

#6.Descript count
descript <- data$Descript %>% 
  table() %>% 
  data.frame() %>% 
  rename('Descript'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

#7. Address
address <- data$Address %>% 
  table() %>% 
  data.frame() %>% 
  rename('Address'='.') %>% 
  rename('Frequency'='Freq') %>% 
  arrange(-Frequency)

dim(data)
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

f_cat_dis <- prop.table(table(category2, district2))

f_cat <- rowSums(f_cat_dis)

f_dis <- colSums(f_cat_dis)
# Conditional distributions of ’category’ given ’district’.

f_cat_given_dis <- t(f_cat_dis) / f_dis

f_dis_given_cat <- f_cat_dis / f_cat
#Consider a randomly-selected incident from this population:
#(a) What is the probability the incident was drug / narcotic-related and occurred in the Tenderloin district? 
#(b)What is the probability the incident occurred in the Tenderloin district, regardless of the incident category? Answer to four significant figures.
#(c)Given that a crime occurred in the Richmond district, what is the probability it was drug / narcotic-related? Answer to four significant figures.

#Count based on category 
crimes_by_type <- data %>% 
  group_by(data$Category) %>% 
  summarise(crime_count = length(Category))
names(crimes_by_type) <- c("Crime Type", "Count")
crimes_by_type
# Add one attribute about month of the date
#data$Month = strftime(strptime(data$Date,"%Y:%m:%d"),"%m") the valuse of Month are all the NA.
data <- data %>% 
  mutate(Month = month(Date))  
#Count the number of crimes committed each day of the month
data_box= data %>%
  group_by(Month,Date)%>%
  summarise(count=n())

#Scatter plot of number of crimes each month
#Count the number of crimes committed each month
data_Scatter= data %>%
  group_by(Month)%>%
  summarise(count=n())

#Scatter plot
plot(data_Scatter$Month,data_Scatter$count,
     xlab = "Month",
     ylab = "frequency of crimes committed")


#Count number of crimes based on date 
is.na(data$Date)
num_crime_date<-as.data.frame(table(data$Date))

#Crime rate per hour
num_crime_date$crime_rate_hour<-num_crime_date$Freq/24
num_crime_date$crime_rate_hour

#num_crime_date$crime_rate_hour<- as.data.frame(num_crime_date$crime_rate_hour)
#num_crime_date frequency histogram
ggplot(num_crime_date, aes(num_crime_date$crime_rate_hour, labels=num_crime_date$crime_rate_hour) )+
  geom_histogram(bins = 50, fill = '#725A7A')+ 
  xlab("Crime rate per hour") + 
  ylab("frequency of crime rate")+
  theme(panel.background=element_rect(fill = 'transparent',color='gray'),legend.position="None")

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
