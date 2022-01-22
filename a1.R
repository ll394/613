install.packages("tidyverse")
install.packages("reader")
install.packages("dplyr")

library(tidyverse)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
setwd("~/Desktop")

install.packages("data.table")
library(data.table)

#Exercise 1 Basic Statistics#
#1 
#Number of households surveyed in 2007
dathh2007 <- read.csv("dathh/dathh2007.csv")
nrow(dathh2007)
#10498

#2
#Number of households surveyed in 2007
dathh2005 <- read.csv("dathh/dathh2005.csv")
length(dathh2005$mstatus[dathh2005$mstatus == "Couple, with Kids"])
#3374

#3 
#Number of households surveyed in 2007
datind2008 <- read.csv("datind/datind2008.csv")
nrow(datind2008)
#25510

#4
#Number of individuals aged between 25 and 35 in 2016
datind2016 <- read.csv("datind/datind2016.csv")
d1 <- length(which(datind2016$age == 25))
d2 <- length(which(datind2016$age == 26))
d3 <- length(which(datind2016$age == 27))
d4 <- length(which(datind2016$age == 28))
d5 <- length(which(datind2016$age == 29))
d6 <- length(which(datind2016$age == 30))
d7 <- length(which(datind2016$age == 31))
d8 <- length(which(datind2016$age == 32))
d9 <- length(which(datind2016$age == 33))
d10 <- length(which(datind2016$age == 34))
d11 <- length(which(datind2016$age == 35))
sum(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11)
#2765

#5
#Cross-table gender/profession in 2009
datind2009 <- read.csv("datind/datind2009.csv")
mytable <- table(datind2009$gender,datind2009$profession)
print(mytable)

#6
#Distribution of wages in 2005 and 2019. 
#Report the mean, the standard deviation, the inter-decile ratio D9/D1 and the Gini coefficient:
datind2005 <- read.csv("datind/datind2005.csv")
is.na(datind2005$wage)
wage2005 <- na.omit(datind2005$wage)
newwage2005 <- wage2005[wage2005 !=0 ]
#mean & sd
mean(newwage2005)
sd(newwage2005)
#Inter_decile ratio
quantile(newwage2005, probs = 0.9)/quantile(newwage2005, probs = 0.1)
library(ineq)
ineq(newwage2005, type ="Gini")

datind2019 <- read.csv("datind/datind2019.csv")
is.na(datind2019$wage)
wage2019 <- na.omit(datind2019$wage)
newwage2019 <- wage2019[wage2019 !=0 ]
#mean & sd
mean(newwage2019)
sd(newwage2019)
#Inter_decile ratio
quantile(newwage2019, probs = 0.9)/quantile(newwage2019, probs = 0.1)
library(ineq)
ineq(newwage2019, type ="Gini")

#7
#Distribution of age in 2010. Plot a histogram. Is there any difference between men and women? 
datind2010 <- read.csv("datind/datind2010.csv")
View(datind2010)
hist(datind2010$age)
male <- datind2010[datind2010$gender == "Male",]
female <- datind2010[datind2010$gender =="Female",]
hist(male$age)
hist(female$age)

#8
# Number of individuals in Paris in 2011
dathh2011 <- read.csv("dathh/dathh2011.csv")
datind2011 <- read.csv("datind/datind2011.csv")
data2011 <- merge(dathh2011, datind2011,by ="idmen")
dat_2011 <- distinct(data2011)
length(data2011$location[m_2011$location == "Paris"])
#3531


#Exercise 2 Merge Data sets#
#1 
#Read all individual datasets from 2004 to 2019. Append all these datasets
datind2004 <- read.csv("datind/datind2004.csv")
datind2005 <- read.csv("datind/datind2005.csv")
datind2006 <- read.csv("datind/datind2006.csv")
datind2007 <- read.csv("datind/datind2007.csv")
datind2008 <- read.csv("datind/datind2008.csv")
datind2009 <- read.csv("datind/datind2009.csv")
datind2010 <- read.csv("datind/datind2010.csv")
datind2011 <- read.csv("datind/datind2011.csv")
datind2012 <- read.csv("datind/datind2012.csv")
datind2013 <- read.csv("datind/datind2013.csv")
datind2014 <- read.csv("datind/datind2014.csv")
datind2015 <- read.csv("datind/datind2015.csv")
datind2017 <- read.csv("datind/datind2017.csv")
datind2018 <- read.csv("datind/datind2018.csv")
datind2019 <- read.csv("datind/datind2019.csv")

datind_csv <- rbind(datind2004,datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,
                    datind2012,datind2013,datind2014,datind2015,datind2016,datind2017,datind2018,datind2019)

#2
#Read all household datasets from 2004 to 2019. Append all these datasets.
dathh2004 <- read.csv("dathh/dathh2004.csv")
dathh2005 <- read.csv("dathh/dathh2005.csv")
dathh2006 <- read.csv("dathh/dathh2006.csv")
dathh2007 <- read.csv("dathh/dathh2007.csv")
dathh2008 <- read.csv("dathh/dathh2008.csv")
dathh2009 <- read.csv("dathh/dathh2009.csv")
dathh2010 <- read.csv("dathh/dathh2010.csv")
dathh2011 <- read.csv("dathh/dathh2011.csv")
dathh2012 <- read.csv("dathh/dathh2012.csv")
dathh2013 <- read.csv("dathh/dathh2013.csv")
dathh2014 <- read.csv("dathh/dathh2014.csv")
dathh2015 <- read.csv("dathh/dathh2015.csv")
dathh2016 <- read.csv("dathh/dathh2016.csv")
dathh2017 <- read.csv("dathh/dathh2017.csv")
dathh2018 <- read.csv("dathh/dathh2018.csv")
dathh2019 <- read.csv("dathh/dathh2019.csv")
dathh_csv<- rbind(dathh2004,dathh2005,dathh2006,dathh2007,dathh2008,dathh2009,dathh2010,dathh2011,
                  dathh2012,dathh2013,dathh2014,dathh2015,dathh2016,dathh2017,dathh2018,dathh2019)

#3
#List the variables that are simultaneously present in the individual and household datasets.
Common_variables <- intersect(colnames(datind_csv), colnames(dathh_csv))
Common_variables

#4
#Merge the appended individual and household datasets.
Total_data <- left_join(datind_csv, dathh_csv, by = c("idmen","year"))
Total_data

#5
#Number of households in which there are more than four family members
member_data<- Total_data %>% group_by(idmen, year)%>% dplyr::summarise(count = n())
nrow(filter(member_data,count>4))
#12436

#6
#Number of households in which at least one member is unemployed
unemp_data<- Total_data %>% group_by(idmen, year)%>% filter(empstat == "Unemployed")
unemp_data1 <- subset(unemp_data,respondent>=1)
nrow(unemp_data1)
#8702

#7
#Number of households in which at least two members are of the same profession
profess_data <- Total_data %>% drop_na(profession)%>% group_by(idmen, year,profession )%>% dplyr::summarise(count = n())
nrow(filter(profess_data, count >=2))
#7651

#8
#Number of individuals in the panel that are from household-Couple with kids
nrow(filter(Total_data,mstatus == "Couple, with Kids"))
#209382

#9
#Number of individuals in the panel that are from household-Couple with kids
nrow(filter(Total_data,location == "Paris"))
#51904

#10
#Find the household with the most number of family members. Report its idmen
most_member <- (Total_data) %>% group_by(idmen,year)%>% dplyr::summarise(count = n())%>% arrange(desc(count))
sort(most_member$count)[length(most_member$count)]
sort(most_member$count)[length(most_member$count)-1]
#2.207811e+15 in year 2007
#2.510263e+15 in year 2010

#11
#Number of households present in 2010 and 2011
Total_csv %>% filter(year == 2010,
                     year == 2011)
length(Total_csv$idmen)
data1011 = Total_csv[!is.na(Total_csv$year) & (Total_csv$year==2010 |Total_csv$year==2011 ),]
length(intersect(data1011[data1011$year==2010,]$idmen,data1011[data1011$year==2011,]$idmen))
#8984


#Exercise 3 Migration#
#1 Find out the year each household enters and exit the panel. 
# Report the distribution of the time spent in the survey for each household.
year_enex <- Total_csv %>% group_by(idmen)%>% summarise(max = max(year),
                                                      min = min(year),
                                                      duration = max - min
)
plot(density(year_enex$duration))
par("mar")
par(mar = c(1,1,1,1))


#2 Based on datent, identify whether or not a household moved into its current dwelling at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across years.  
na.omit(dathh_csv$datent)
household_moved <- dathh_csv %>% mutate(move_atyear = 
                       case_when(dathh_csv$year == dathh_csv$datent ~"1",
                                 dathh_csv$year != dathh_csv$datent ~"0")
)
head(household_moved,10)

household_moved1 <- dathh_csv %>% group_by (year) %>% summarise(share = sum(move_atyear == 1,na.rm = TRUE)/n())
ggplot(household_moved1,aes(x=year, y=share))+ geom_line()

#3
#Based on myear and move, identify whether or not household migrated at the year of survey. 
#Report the first 10 rows of your result and plot the share of individuals in that situation across years. 
na.omit(dathh_csv$myear)
na.omit(dathh_csv$year)
move_bef2014 <- dathh_csv %>% filter(year <= 2014)%>% mutate(house_migrated = ifelse(move_bef2014$year == move_bef2014$myear,1,0))
move_af2014 <- dathh_csv %>% filter(year > 2014) %>% mutate(house_migrated = ifelse(move_af2014$move == 2,1,0))                                                                                                            
move_data <- rbind(move_bef2014,move_af2014)
head(move_data,10)

move_data1  %>% group_by(year) %>% summarise(sum(share1 = dathh_csv$house_migrated == 1,na.rm = TRUE)/n())

#4                                                            
#Mix the two plots you created above in one graph, clearly label the graph. 
#Do you prefer one method over the other? Justify.

#5
#For households who migrate, find out how many households had at least one family member changed his/her profession or employment status. 

#Exercise 4 Attrition#
  

