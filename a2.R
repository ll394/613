library(tidyverse)
library(readr)
library(dplyr)
library(plyr)

getwd()
setwd("~/Desktop")

# Exercise 1 OLS estimate
#1 calculate the correlation between Y and X
datind2009 <- read.csv("datind/datind2009.csv")
datind_1 <- datind2009 %>% select(c("empstat", "age", "wage")) %>% na.omit(datind_1) 
datind_2 <- subset(datind_1, datind_1$wage !=0)

cor(datind_2$age,datind_2$wage)
# 0.143492

#2 
X <- as.matrix(cbind(1, datind_2$age))
Y <- as.matrix(datind_2$wage)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
beta_hat
#230.9923

#3
#using the standard formulas of the OLS
Y_hat <- X %*% beta_hat
error <- Y - Y_hat
sigma_2 <- (t(error)%*%error)/(nrow(X) - ncol(X))
var_betah <- c(sigma_2)*solve(t(X) %*% X)
se_betah <- sqrt(diag(var_betah))
se_betah
#14.8774

#using bootstrap with 49 and 499 replications respectively.Comment on the difference between the two strategies
#bootstrap with 49 replications
R = 49
reg <-  lm(wage ~ age, data = datind_2 )
novar <- length(reg$coefficients)

outs = mat.or.vec(R,2)
set.seed(123)

  for (i in 1:R) {
    sample <- sample(1:nrow(datind_2),nrow(datind_2), replace = TRUE)
    datasam <- datind_2[sample,]
    reg1 <-  lm(wage ~ age, data = datasam)
    outs[i,] <- reg1$coefficients
  }
  estmean <- apply(outs,2,mean)
  estsd <- apply(outs,2,sd)
  
  estimate <-  cbind(summary(reg1)$coefficients[,1],
                   summary(reg1)$coefficients[,2],
                   estmean,
                   estsd)
  colnames(estimate) <- c("BT: estimate","BT:sd")
estimate

#bootstrap with 499 replications
R2 = 499
reg2 <-  lm(wage ~ age, data = datind_2 )
novar2 <- length(reg2$coefficients)

outs2 = mat.or.vec(R,2)
set.seed(123)

for (i in 1:R) {
  sample2 <- sample(1:nrow(datind_2),nrow(datind_2), replace = TRUE)
  datasam2 <- datind_2[sample2,]
  reg2 <-  lm(wage ~ age, data = datasam2)
  outs2[i,] <- reg2$coefficients
}
estmean2 <- apply(outs2,2,mean)
estsd2 <- apply(outs2,2,sd)

estimate2 <-  cbind(summary(reg2)$coefficients[,1],
                   summary(reg2)$coefficients[,2],
                   estmean2,
                   estsd2)
colnames(estimate2) <- c("BT: estimate","BT:sd")
estimate2


# Exercise 2 Detrend Data
#1 create a categorical variable ag, which bins the age variables into the following groups
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
datind2016 <- read.csv("datind/datind2016.csv")
datind2017 <- read.csv("datind/datind2017.csv")
datind2018 <- read.csv("datind/datind2018.csv")

datind_3 <- (rbind(datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,
                datind2014,datind2015,datind2016,datind2017,datind2018))
datind_3$idind <- format(datind_3$idind,scientific = F)
datind_3$idmen <- format(datind_3$idmen,scientific = F)

datind_4 <- datind_3[!(datind_3$wage == 0),] %>% na.omit(datind_3) 

breaks <- c(18,26,31,36,41,46,51,56,60,100)
tags <- c("[18-25]", "[26-30]", "[31-35]", "[36-40]","[41-45]","[46-50]","[51-55]","[56-60]","[60+]")
ag <- data.frame(datind_4, agegroup = cut(datind_4$age, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags))
ag$year <- as.character(ag$year)

#2 plot the wage of each age across years. Is there a trend?
library(ggplot2)
library(colorspace)

ggplot(ag, aes(year,
               wage,
               fill = agegroup)) +
  ylim(0,300000) +
  geom_boxplot() + 
  theme_bw() +
  scale_x_discrete() +
  scale_color_discrete(na.translate=FALSE) +
  scale_fill_discrete_qualitative(palette = "Set 3") +
  labs(title = "Boxplot of the wage with age group",
       subtitle = "From 2005 to 2018",
       x = "Year", y = "Wage")

#3 consider Yit = βXit + γit + eit. After including a time fixed effect, how to the estimated coefficients change? 
oldreg <- lm(wage ~ age, data = ag)
summary(oldreg)
newreg <- lm(wage ~ age + year, data = ag)
summary(newreg)


#Exercise 3 Numerical Optimization
datind2007 <- read.csv("datind/datind2007.csv")
datind2007$idind <- format(datind2007$idind,scientific = F)
datind2007$idmen <- format(datind2007$idmen,scientific = F)
datind_5 <- datind2007 %>% na.omit(datind_5) 
datind_6 <- subset(datind_5, datind_5$wage !=0 & datind_5$age !=0)

#1 Exclude all individuals who are inactive
no_inactive <- subset(datind_6, datind_6$empstat !="Inactive" & datind_6$empstat != "Retired")

#2 Write a function that returns the likelihood of the probit of being employed.
no_inactive$empstat <- ifelse(no_inactive$empstat == "Employed",1,0)
summary(no_inactive)

fnlike<- function(par, age, empstat)
{
  xb = par[1] + par[2]*age
  p = pnorm(xb)
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like = empstat*log(p) +(1-empstat)*log(1-p)
  return(-sum(like))
}

#3 Optimize the model and interpret the coefficients.
x1 <- no_inactive$age
y1 <- no_inactive$employed
noftry = 100
outs = mat.or.vec(noftry, 3)
for (i in 1:noftry)
{
  start = runif(2,-10,10)
  res = optim(start, fn = fnlike, method = "BFGS", control = list(trace = 6, maxit = 1000), x = x1 , y =y1 )
  outs_1[i,] = c(res$par,res$value) 
} 

#4 Can you estimate the same model including wages as a determinant of labor market participation? Explain.
dat <- no_inactive
fnlike1<- function(par, age, wage, empstat)
{
  xb = par[1] + par[2]*age + par[3]*wage
  p = pnorm(xb)
  p[p>0.999999] = 0.999999
  p[p<0.000001] = 0.000001
  like = empstat*log(p) +(1-empstat)*log(1-p)
  return(-sum(like))
}

#Exercise 4 Discrete Choice
#1 Exclude all individuals who are inactive
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

datind_7 <- (rbind(datind2005,datind2006,datind2007,datind2008,datind2009,datind2010,datind2011,datind2012,datind2013,
                   datind2014,datind2015))
datind_7$idind <- format(datind_7$idind,scientific = F)
datind_7$idmen <- format(datind_7$idmen,scientific = F)

datind_8 <- datind_7[!(datind_7$wage == 0),] %>% na.omit(datind_7)
datind_9 <- subset(datind_8, datind_8$empstat !="Inactive" & datind_8$empstat != "Retired")

#2 Write and optimize the probit, logit, and the linear probability models
# Probit Model
