install.packages("mlogit")
install.packages("nnet")
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(mlogit)
library(nnet)

getwd()
setwd("/Users/liulu/Desktop/Data")

datj <- read.csv(file = 'datjss.csv')
dats <- read.csv(file = 'datsss.csv')
datstu <- read.csv(file = 'datstu_v2.csv')
dats[!(is.na(dats$schoolname) | dats$schoolname=="" | is.na(dats$sssdistrict) | dats$sssdistrict==""), ]


# Exercise 1 Basic Statistics #
#1 Number of students, schools, programs
nrow(datstu)
length(unique(dats$schoolcode))

programs <- c(datstu$choicepgm1, datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)
programs1 <- nrow(unique(data.frame(programs)))
programs1

#2 Number of choices(school, program)
long_datstu <- datstu %>% pivot_longer("schoolcode1":"schoolcode6", names_to = "schoolcode", values_to = "code") %>% pivot_longer("choicepgm1":"choicepgm6", names_to = "choicepgm", values_to = "program")
long_datstu1 <- long_datstu %>%
  mutate(schoolcode = recode(schoolcode,
                             "schoolcode1" = "1",
                             "schoolcode2" = "2",
                             "schoolcode3" = "3",
                             "schoolcode4" = "4",
                             "schoolcode5" = "5",
                             "schoolcode6" = "6"))
long_datstu2 <- long_datstu1 %>%
  mutate(choicepgm = recode(choicepgm,
                            "choicepgm1" = "1",
                            "choicepgm2" = "2",
                            "choicepgm3" = "3",
                            "choicepgm4" = "4",
                            "choicepgm5" = "5",
                            "choicepgm6" = "6"))

long_datstu3 <- subset(long_datstu2,long_datstu2$schoolcode==long_datstu2$choicepgm) %>% drop_na(code)
long_datstu3[!long_datstu3$program == "",]
long_datstu4 <- long_datstu3[c('code','program')]
nrow(unique(long_datstu4))


#3 Number of students applying to at least one senior high schools in the same district to home
same_district_home <- datstu %>% select(V1, contains("school"), jssdistrict) %>% pivot_longer(cols = schoolcode1:schoolcode6, names_to = "school", values_to = "schoolcode")
highsch_district <- dats[,c(3,4)]
sameplace <- left_join(same_district_home, highsch_district, by = "schoolcode") %>% mutate(jssdistrict == sssdistrict) %>%
  distinct(V1, .keep_all = TRUE)
names(sameplace)[names(sameplace) == 'jssdistrict == sssdistrict'] <-'match'
sameplace$match <- as.numeric(sameplace$match)
count(sameplace, sameplace$match == 1)

#4 Number of students each senior high school admitted
numofs <- long_datstu1 %>% select(V1,score,rankplace,schoolcode,code) %>%
  filter(!is.na(rankplace) & rankplace != 99)
as.numeric(unlist(numofs))
numofs$admitted <- ifelse(numofs$rankplace == numofs$schoolcode,1,0)
student_admit <- numofs %>% filter(admitted == 1) %>% select(code) %>% group_by(code) %>% dplyr::summarise(count = n())
student_admitted <- unique(student_admit)

#5 The cutoff of senior high schools (the lowest score to be admitted)
cutoff <- numofs %>% filter(admitted == 1) %>% select(code,score) %>% group_by(code) %>% summarise(min(score))
cutoff

#6 The quality of senior high schools (the average score of students admitted)
quality<- numofs %>% filter(admitted == 1) %>% select(code,score) %>% group_by(code) %>% summarise(mean(score))
quality

# Exercise 2 Data #
school_program <- long_datstu4 %>% mutate(choice = paste0(code,program))
names(school_program)[names(school_program) == 'code'] <- 'schoolcode'
sdistrict <- dats[,3:6] %>% select(schoolcode, sssdistrict, ssslong, ssslat)
district <- sdistrict %>% distinct(schoolcode, sssdistrict, ssslong, ssslat, .keep_all = TRUE)
district <- na.omit(district)
schooldata <-school_program %>% left_join(district, by = "schoolcode") 

names(cutoff)[names(cutoff) == 'code'] <- 'schoolcode'
schooldata <-schooldata %>% left_join(cutoff, by = "schoolcode") 

names(quality)[names(quality) == 'code'] <- 'schoolcode'
schooldata <-schooldata %>% left_join(quality, by = "schoolcode") 

names(student_admitted)[names(student_admitted) == 'code'] <- 'schoolcode'
schooldata <-schooldata %>% left_join(student_admitted, by = "schoolcode")


#Exercise 3 Distance
dist <- same_district_home %>% left_join(datj, by = "jssdistrict") 
dats1 <- dats %>% select(schoolcode, sssdistrict, ssslong, ssslat)
dist1 <- dist %>% left_join(dats1, by = "schoolcode")
dist2 <- na.omit(dist1) %>% distinct(schoolcode, sssdistrict, ssslong, ssslat, .keep_all = TRUE)
dist2 %>% mutate(distance = sqrt((69.172 *(ssslong-point_x)*cos(point_y/57.3))^2 + (69.172*(ssslat - point_y))^2))
  
#Exercise 4 Dimensionality Reduction 
# 1 Recode the school code into its first three digits (substr). Call this new variable scode_rev.
newdata <- na.omit(long_datstu)
newdata[!apply(newdata == "",1,all),]
newdata1 <- newdata %>% filter(!is.na(rankplace) & rankplace != 99) %>% mutate(scode_rev = substr(code,1,3))

#2 Recode the program variable into 4 categories: arts (general arts and visual arts), economics (business and home economics), science (general science) and others. 
#Call this new variable pgm rev.
newdata1 <- newdata1 %>% mutate(program = case_when(
                                      program =="General Arts" ~ "arts",
                                      program =="Visual Arts" ~ "arts",
                                      program =="Business" ~ "economics",
                                      program =="Home Economics" ~ "economics",
                                      program =="General Science" ~ "science",
                                      TRUE ~ "others"))
                                  
names(newdata1)[names(newdata1) == 'program'] <- 'pgm_rev'

#3 Create a new choice variable choice rev.
newdata2 <- newdata1 %>% mutate(choice_rev = paste0(newdata1$scode_rev,newdata1$pgm_rev))

#4 Recalculate the cutoff and the quality for each recoded choice.
newdata2$admitted <- ifelse(newdata2$rankplace == newdata2$schoolcode,1,0)
newdata2_admitted <- newdata2 %>% filter(admitted == 1)
new_cutoff <- newdata2_admitted %>% group_by(choice_rev) %>% summarise(min(score))
new_cutoff
new_quality <- newdata2_admitted %>% group_by(choice_rev) %>% summarise(mean(score))
new_quality

#5 Consider the 20,000 highest score students.
highest <- newdata2 %>% arrange(desc(score)) %>% slice(1:2000)

# Exercise 5 First Model
#1 Propose a model specification. Write the Likelihood function.
m_data <-highest %>% select(V1,score,rankplace,schoolcode,code,choicepgm,scode_rev,pgm_rev,choice_rev)
m_data %>% filter(schoolcode == "schoolcode1")


