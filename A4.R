library(tidyverse)
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(VGAM)
library(AER)
library(panelr)
library(plm)

setwd("/Users/liulu/Desktop/A4")
dat <- read.csv(file = 'dat_A4.csv')
options(scipen = 999)

#Exercise 1 Preparing the Data#
#(1) Create additional variable for the age of the agent "age", total work experience measured in years"work_exp"
#Assume the final year of survey is 2019
dat <- dat %>% mutate(age = 2019 - KEY_BDATE_Y_1997)
#Assume there are 52 weeks in a year
dat$work_exp <- rowSums(dat[,18:28], na.rm = TRUE) / 52
                                    
#(2) Create additional education variables indicating total years of schooling from all variables related to education in our data set
#Assume None: 0 years; GED: 12 years; High school: 12 years; Associate College: 14 years; Bachelor's degree:16 years; Master's degree: 18 years; PhD:23 years; Professional degree:20 years
BIO_DAD <- which(dat$CV_HGC_BIO_DAD_1997 == 95)
BIO_MOM <- which(dat$CV_HGC_BIO_MOM_1997 == 95)
RED_DAD <- which(dat$CV_HGC_RES_DAD_1997 == 95)
RED_MOM <- which(dat$CV_HGC_RES_MOM_1997 == 95)

dat$CV_HGC_BIO_DAD_1997[BIO_DAD] = 0
dat$CV_HGC_BIO_MOM_1997[BIO_MOM] = 0
dat$CV_HGC_RES_DAD_1997[RED_DAD] = 0
dat$CV_HGC_RES_MOM_1997[RED_MOM] = 0

dat1 <- dat %>% mutate(individual_edu = case_when(dat$YSCH.3113_2019 == 1 ~ "0",
                                                  dat$YSCH.3113_2019 == 2 ~ "12",
                                                  dat$YSCH.3113_2019 == 3 ~ "12",
                                                  dat$YSCH.3113_2019 == 4 ~ "14",
                                                  dat$YSCH.3113_2019 == 5 ~ "16",
                                                  dat$YSCH.3113_2019 == 6 ~ "18",
                                                  dat$YSCH.3113_2019 == 7 ~ "23",
                                                  dat$YSCH.3113_2019 == 8 ~ "20"))

dat1 <- dat1 %>% drop_na(CV_HGC_BIO_DAD_1997)
dat1 <- dat1 %>% drop_na(CV_HGC_BIO_MOM_1997)
dat1 <- dat1 %>% drop_na(CV_HGC_RES_DAD_1997)
dat1 <- dat1 %>% drop_na(CV_HGC_RES_MOM_1997)
dat1 <- dat1 %>% drop_na(individual_edu)


#(3) Provide the following visualizations
#Plot the income data(where income is positive) by  i) age groups, ii) gender groups and iii) number of children
dat1$YINC_1700_2019 <- as.factor(dat$YINC_1700_2019)
dat_posincome <- subset(dat1, dat1$YINC_1700_2019 > 0 & dat1$YINC_1700_2019 != "NA" )
ggplot(dat_posincome, aes(as.factor(age), 
                          YINC_1700_2019,
                          group = age)) +
  geom_boxplot() +
  labs(title = "Income distribution by age groups",
       x = "age", y = " income")

# gender groups
dat_gender <- dat_posincome[!(is.na(dat_posincome$KEY_SEX_1997)),]
ggplot(dat_gender, aes(as.factor(KEY_SEX_1997),
                      YINC_1700_2019,
                      group = KEY_SEX_1997)) +
  geom_boxplot() +
  labs(title = "Income distribuction by gender groups", x = "gender", y = "income")

# number of children
dat_child <- dat_posincome[!(is.na(dat_posincome$CV_BIO_CHILD_HH_U18_2019)),]
ggplot(dat_child, aes(as.factor(CV_BIO_CHILD_HH_U18_2019),
                      YINC_1700_2019,
                      group = CV_BIO_CHILD_HH_U18_2019)) + 
         geom_boxplot() +
         labs(title = "Income distribution by child groups",
              x = "number of children", y = "income")

# (4) Table the share of ”0” in the income data by i) age groups, ii) gender groups, iii) number of children and marital status
# age groups
age_share <- dat %>% group_by(age) %>% summarize(zero_income = length(which(YINC_1700_2019 == 0)), total = n()) %>% mutate(share = zero_income / total)
# gender groups
gender_share <- dat %>% group_by(KEY_SEX_1997) %>% summarize(zero_income = length(which(YINC_1700_2019 == 0)), total = n()) %>% mutate(share = zero_income / total)
# children groups and marital status
dat$CV_MARSTAT_COLLAPSED_2019 <- as.factor(dat$CV_MARSTAT_COLLAPSED_2019)
childandmarital_share <- dat %>% group_by(CV_BIO_CHILD_HH_U18_2019, CV_MARSTAT_COLLAPSED_2019) %>% summarize(zero_income = length(which(YINC_1700_2019 == 0)), total = n()) %>% mutate(share = zero_income / total)


#Exercise 2 Heckman Selection Model#
# (1) Using the variables created above, estimate the following models.
# Specify and estimate an OLS model to explain the income variable (where income is positive).
dat_new <- dat_posincome[!(is.na(dat_posincome$age)),]
dat_new <- dat_new[!(is.na(dat_posincome$work_exp)),]
dat_new <- dat_new[!(is.na(dat_posincome$KEY_SEX_1997)),]
dat_new <- dat_new[!(is.na(dat_posincome$CV_BIO_CHILD_HH_U18_2019)),]
dat_new <- dat_new[!(is.na(dat_posincome$CV_MARSTAT_COLLAPSED_2019)),]
dat_new$parent_edu <- rowSums(dat_new[, 8:11])
dat_new$parent_edu <- as.numeric(dat_new$parent_edu)
dat_new$individual_edu <- as.numeric(dat_new$individual_edu)
dat_new$total_edu <- dat_new$individual_edu + dat_new$parent_edu

reg <- lm(YINC_1700_2019 ~ age + work_exp + total_edu + KEY_SEX_1997 + CV_BIO_CHILD_HH_U18_2019
                                + CV_MARSTAT_COLLAPSED_2019, dat_new)
summary(reg)

#(3) Estimate a Heckman selection model
# Stage 1 Probit Model
reg2 <- glm(dumvar ~ age + work_exp + total_edu + KEY_SEX_1997 + CV_BIO_CHILD_HH_U18_2019 + CV_MARSTAT_COLLAPSED_2019, family = binomial(link = "probit"), data = dat2)
summary(reg2)

# Inverse Mills Ratio
IMR <- dnorm(predict(reg2)) / pnorm(predict(reg2))
summary(IMR)

# Heckman Selection Model
dat2 <- dat2 %>% filter(YINC_1700_2019 >0)
reg_heckman <- lm(YINC_1700_2019 ~ age + work_exp + total_edu + KEY_SEX_1997 + CV_BIO_CHILD_HH_U18_2019 + CV_MARSTAT_COLLAPSED_2019 + IMR, dat2)
summary(reg_heckman)



# Exercise 3 Censoring
# (1) Plot a histogram to check whether the distribution of the income variable. What might be the censored value here?
ggplot(dat_posincome, aes(x = YINC_1700_2019)) +
  geom_histogram(binwidth = 3000) +
  labs(title = "Histogram of Income Distribution", x = "income", y = "density")
# The censored value is $100,000

# (2）Propose a model to deal with the censoring problem.
# The Tobit model is proper to deal with the censoring problem.

# (3) Estimate the appropriate model with the censored data(please write down the likelihood function and optimize yourself without using the pre-programmed package)
dat3 <- dat1[!(is.na(dat1$YSCH.3113_2019)),]
dat3 <- subset(dat3, dat3$YINC_1700_2019 > 0 & dat3$YINC_1700_2019 != "NA" )
dat3$ind <- 0
dat3$ind[which(dat3$YINC_1700_2019 < 100000)] <- 1
dat3$ind <- as.numeric(dat3$ind)
dat3$intercept <- 1
names(dat3)[30] <- 'income'

dat3 <- dat3[!(is.na(dat3$age)),]
dat3<- dat3[!(is.na(dat3$work_exp)),]
dat3 <- dat3[!(is.na(dat3$KEY_SEX_1997)),]
dat3<- dat3[!(is.na(dat3$CV_BIO_CHILD_HH_U18_2019)),]
dat3<- dat3[!(is.na(dat3$CV_MARSTAT_COLLAPSED_2019)),]
dat3$parent_edu <- rowSums(dat3[, 8:11])
dat3$parent_edu <- as.numeric(dat3$parent_edu)
dat3$individual_edu <- as.numeric(dat3$individual_edu)
dat3$total_edu <- dat3$individual_edu + dat3$parent_edu

functionlike = function(param,intercept,x1,x2,x3,x4,x5,x6,y,ind)
  {
  yhat = param[1]*intercept + param[2]*x1 + param[3]*x2 + param[4]*x3 + param[5]*x4 + param[6]*x5 + param[7]*x6 
  residual = y - yhat
  standard = (100000 - yhat) / exp(param[7])
  like = dat3$ind*log(dnorm(residual/exp(param[7]))/exp(param[7])) + (1-dat3$ind)*log(1-pnorm(standard))
  return(-sum(like))
}

start = runif(7,-100,100)
tobit_fun <- optim(start, fn = functionlike, method = "BFGS", control= list(trace=6,REPORT=1,maxit=1000),intercept = dat3$intercept,
            x1=dat3$age,x2=dat3$work_exp,x3=dat3$total_edu,x4=dat3$KEY_SEX_1997,x5=dat3$CV_BIO_CHILD_HH_U18_2019,x6=dat3$CV_MARSTAT_COLLAPSED_2019,ind=dat3$ind,y=dat3$income, hessian=TRUE)
tobit_fun$par


# Exercise 4 Panel Data#
# (1) Explain the potential ability bias when trying to explain to understand the determinants of wages
pdat <- read.csv(file ='dat_A4_panel.csv')

pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1998 = CV_HIGHEST_DEGREE_9899_1998)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_1999 = CV_HIGHEST_DEGREE_9900_1999)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2000 = CV_HIGHEST_DEGREE_0001_2000)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2001 = CV_HIGHEST_DEGREE_0102_2001)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2002 = CV_HIGHEST_DEGREE_0203_2002) 
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2003 = CV_HIGHEST_DEGREE_0304_2003)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2004 = CV_HIGHEST_DEGREE_0405_2004)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2005 = CV_HIGHEST_DEGREE_0506_2005)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2006 = CV_HIGHEST_DEGREE_0607_2006)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2007 = CV_HIGHEST_DEGREE_0708_2007)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2008 = CV_HIGHEST_DEGREE_0809_2008)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2009 = CV_HIGHEST_DEGREE_0910_2009)
pdat <- pdat %>% rename(CV_HIGHEST_DEGREE_EVER_EDT_2010 = CV_HIGHEST_DEGREE_1011_2010)

lp_dat <- long_panel(pdat, prefix = "_", begin = 1997, end = 2019,label_location = "end")

lp_dat <- lp_dat %>% mutate(age = wave - KEY_BDATE_Y)
lp_dat <- lp_dat %>% mutate(work_exp = sum(across(contains("CV_WKSWK_JOB"))/52, na.rm = TRUE))

lp_dat <- lp_dat %>% rename(year = wave)
lp_dat <- lp_dat %>% rename(income = YINC.1700)
lp_dat <- lp_dat %>% rename(marital = CV_MARSTAT_COLLAPSED)

longp_dat <- as.data.frame(lp_dat)
longp_dat$id <- as.numeric(longp_dat$id)
longp_dat$income <- as.numeric(longp_dat$income)
longp_dat$marital <- as.numeric(longp_dat$marital)
longp_dat$CV_HIGHEST_DEGREE_EVER_EDT <- as.numeric(longp_dat$CV_HIGHEST_DEGREE_EVER_EDT)
longp_dat$work_exp <- as.numeric(longp_dat$work_exp)

longp_dat <- longp_dat %>% mutate(edu = case_when(longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 0 ~ "0",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 1 ~ "12",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 2 ~ "12",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 3 ~ "14",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 4 ~ "16",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 5 ~ "18",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 6 ~ "23",
                                                  longp_dat$CV_HIGHEST_DEGREE_EVER_EDT == 7 ~ "23"))
longp_dat$edu <- as.numeric(longp_dat$edu)

longp_dat <- longp_dat %>% mutate(mar = case_when(longp_dat$marital == 1 ~ "1",
                                                  longp_dat$marital == 0 ~ "0",
                                                  longp_dat$marital == 2 ~ "0",
                                                  longp_dat$marital == 3 ~ "0",
                                                  longp_dat$marital == 4 ~ "0"))
longp_dat$mar <- as.numeric(longp_dat$mar)
longp_dat <- longp_dat %>% select(id,year,income,mar,edu,work_exp) %>% drop_na()


# (2) Exploit the panel dimension of the data to propose a model to correct for the ability bias

# Within Estimator
me_income<- longp_dat%>% group_by(id) %>% summarise(m_income = mean(income))
me_mar <- longp_dat%>% group_by(id) %>% summarise(m_mar = mean(mar))
me_edu <- longp_dat%>% group_by(id) %>% summarise(m_edu = mean(edu))
me_work <- longp_dat%>% group_by(id) %>% summarise(m_work = mean(work_exp))

mean_data <- longp_dat %>% left_join(me_income,by = "id")
mean_data <- mean_data %>% left_join(me_mar,by = "id")
mean_data <- mean_data %>% left_join(me_edu,by = "id")
mean_data <- mean_data %>% left_join(me_work,by = "id")

mean_data$income_diff <- mean_data$income - mean_data$m_income
mean_data$marital_diff <- mean_data$mar - mean_data$m_mar
mean_data$edu_diff <- mean_data$edu - mean_data$m_edu
mean_data$work_diff <- mean_data$work_exp - mean_data$m_work

within_estimator <- lm(income_diff~ marital_diff + edu_diff + work_diff, mean_data)
summary(within_estimator)

# package
within_model <- plm(income ~ mar + edu + work_exp,mean_data, model = "within")
summary(within_model)


# Between Estimator
between_data <- me_income %>% left_join(me_mar, by = "id")
between_data <- between_data %>% left_join(me_edu, by = "id")
between_data <- between_data %>% left_join(me_work, by = "id")
between_estimator <- lm(m_income ~ me_edu + me_mar + me_work, between_data)
summary(between_estimator)

# package
between_model <- plm(income ~ edu + mar + work_exp,longp_dat,model = "between")
summary(between_model)

# Difference Estimator
# package
difference_model <- plm(income ~ edu + mar + work_exp,longp_dat,model = "fd")
summary(difference_model)



