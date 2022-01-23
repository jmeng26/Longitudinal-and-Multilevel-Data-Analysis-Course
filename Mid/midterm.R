## PART I. Exploratory Data Analysis------------------------------------------------------------------

## install required packadges
library(lme4)
library(table1)
library(tidyr)
## import the dataset
dat <- get(load('/Users/mengjiawei/Desktop/2020fall/525/Midterm/Midterm2020_Data.RData'))
## add id to the data
dat$id <- 1:nrow(dat)

## 1. summary statistics for gender, race, dad_edu, age and mathscore------------------------------------------------
## use table 1 function 
label(dat$t1) <- "Period 1 score"
label(dat$t2) <- "Period 2 score"
label(dat$t3) <- "Period 3 score"
label(dat$t4) <- "Period 4 score"
label(dat$t5) <- "Period 5 score"
label(dat$t6) <- "Period 6 score"
label(dat$t7) <- "Period 7 score"
##convert age to years
dat$age<- round(dat$age*0.08333333,1)
dat$race<- factor(dat$race, levels = c(1, 2, 3, 5), labels = c("White, non-Hispanic","Black or African American, non-Hispanic", "Hispanic","Asian"))
dat$gender <- factor(dat$gender, levels = c(1, 2), labels = c("male", "female")) 

# descriptive table
table1(~ gender + race + dad_edu + age + t1 + t2 + t3 + t4 + t5 + t6 + t7, data=dat)


## 2. checking missing pattern------------------------------------------------------------------------------
# 2.1 missing value for each column
library(mice)
md.pattern(dat, plot = FALSE, rotate.names = FALSE)

colMeans(is.na(dat))
#       race      gender     dad_edu         age          t1          t2          t3          t4          t5 
#0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 0.009722222 0.711944444 0.033611111 0.145000000 
#t6          t7          id 
#0.301666667 0.375000000 0.000000000 

# t3 has NA more than 71%, maybe need to drop that variable

# 2.2 study pattern focusing on score variable, group by gender*education
# write a loop to calculate the number of missing value in score for each id 
#change the wide form to long form
dat1 = gather (dat, key=sample, value = mathscore, c(t1,t2,t3,t4,t5,t6,t7))
dat1
dat1$id
dat1$period = rep (c(1,2,3,4,5,6,7), 3600)
dat1 <- dat1[order(dat1$id, dat1$sample, dat1$period),]
dat1
num <- length(unique(dat$id))
nmv <- vector()
for (i in 1:num) {
  nmv[i] = nrow(dat1[dat1$id==unique(dat1$id)[i] & is.na(dat1$mathscore),])
}

nmv
# create a subset containing the id, gender, edu, # of NA in math score for each patient
ref = dat1[!duplicated(dat1$id),1:3]
ref$num_NA = nmv
ref
# get summary statistics for missing value for different groups according to gender*edu
library(dplyr)
sta = ref %>%
  filter(!is.na(dad_edu)) %>%
  group_by(gender,dad_edu) %>%
  summarise(N=length(num_NA),n=length(num_NA[num_NA!=0]), mu = mean(num_NA[num_NA!=0]))
sta

#drop t3 because of many NA
dat3<-subset(dat,select = -t3)
colMeans(is.na(dat3))
#Still many NA in mathscore, delete the row if they have NA in t1-t7
dat4<-na.omit(dat3, cols=c("t1", "t2","t4","t5","t6","t7"))
dim(dat4)

#change the long form to wide form
library (tidyr)
dat5 = gather (dat4, key=sample, value = mathscore, c(t1,t2,t4,t5,t6,t7))
dat5$period = rep (c(1,2,4,5,6,7), 2129)
dat5 <- dat5[order(dat5$id, dat5$sample, dat5$period),]

#add different age according to actual year to baseline
for (f in 1:max(dat5$id)){
  dat5$age[dat5$id == f][2]<-dat5$age[dat5$id == f][1] + 0.5
  dat5$age[dat5$id == f][3]<-dat5$age[dat5$id == f][2] + 1
  dat5$age[dat5$id == f][4]<-dat5$age[dat5$id == f][3] + 2
  dat5$age[dat5$id == f][5]<-dat5$age[dat5$id == f][4] + 2
  dat5$age[dat5$id == f][6]<-dat5$age[dat5$id == f][5] + 3
}

#plot to see trend for male and female mathscore vs age
library (ggplot2)
p <- ggplot(dat5, aes(x = age, y = mathscore, group = id, color = factor(id)))
p + geom_line() + facet_grid(. ~ gender, labeller = labeller(gender=c('1'="Male", '2'="Female")))+
  guides(colour=FALSE)


## 3. check trend of math score with agefit model --------------------------------------------------------------
# 3.1 three visualize plots
plot(mathscore~age,data=dat5)
boxplot(mathscore~age,data=dat5,ylab = "mathscore",col="red")  ## outliers
hist(dat5$mathscore)  

# 3.2 univariate association between age and mathscore
## fit SLR model
dat5$dad_edu <- relevel(factor(dat5$dad_edu),ref = "less HS") # set reference group
dat5$race <- relevel(factor(dat5$race),ref = "White, non-Hispanic") # set reference group
dat5[dat5$gender==2,]$gender=0
model1 = lm(mathscore~age,data=dat5)
summary(model1)

## conduct residual analysis 
library(car)
qqnorm(model1$residuals)
qqline(model1$residuals)

## check modifiers and confounders
#1) for interaction and confounder
#########gender
M1 <- lm(mathscore ~ age,  data=dat5)
M2 <- lm(mathscore ~ age + gender + age*gender, data=dat5)
summary(M1)
summary(M2) #gender has interaction
#confounder
M3<- lm(mathscore ~ age + gender, data=dat5)
((coef(M1)[2]-coef(M3)[2])/coef(M1)[2])*100 #gender not  confounder
#########race
M4 <- lm(mathscore ~ age + factor(race) + age*factor(race), data=dat5)
summary(M4) #race has interaction
#confounder
M5 <- lm(mathscore ~ age + factor(race), data=dat5)
((coef(M1)[2]-coef(M5)[2])/coef(M1)[2])*100 #race not confounder
#########dad_edu
M6 <- lm(mathscore ~ age + factor(dad_edu) + age*factor(dad_edu), data=dat5)
summary(M6) #edu has interaction
#confounder
M7 <- lm(mathscore ~ age + factor(dad_edu) , data=dat5)
((coef(M1)[2]-coef(M7)[2])/coef(M1)[2])*100 #edu not confounder
#########gender, race and dad_edu 
M8 <- lm(mathscore ~ age + gender + factor(race) + factor(dad_edu), data=dat5)
summary(M8) #if put them in the model together, the gender, race and edu are all confounder for the mathscore so add all of them in the model

#final model for univariate analysis
model.int = lm(mathscore ~ age + gender + factor(race) + factor(dad_edu) , data=dat5)
summary(model.int)
#this is the primary final model

## conduct residual analysis 
library(car)
qqnorm(model.int$residuals)
qqline(model.int$residuals)
## need transformation

#outlier plot
par(mfrow=c(2,2))
plot(model.int)

#dffits
IDs<-dat5$id
n <- nrow(dat5)
p <- 6
dat5$fits <- dffits(model.int)
sub_fit <- dat5[abs(dat5$fits) > 2*sqrt(p/n),]
plot(IDs, dat5$fits, xlab="ID", ylab="DFFITS")
points(sub_fit$id,sub_fit$fits,col = "red")


# 3.3 remedy measures
#1) poisson regression 
model.fn = glm(mathscore ~ age + gender + factor(race) + factor(dad_edu),data=dat5,family = poisson)
summary(model.fn)
qqnorm(model.fn$residuals)
qqline(model.fn$residuals)
## not good 
par(mfrow=c(2,2))
plot(model.fn)

#2) sqrt transformation on math score
model.sq = lm(sqrt(mathscore) ~ age + gender + factor(race) + factor(dad_edu), data=dat5) ## due to 0 income
summary(model.sq)
qqnorm(model.sq$residuals)
qqline(model.sq$residuals)
par(mfrow=c(2,2))
plot(model.sq)


#3) log transformation on math score
dat6 = na.omit(dat5)
dat6[dat6$mathscore==0,"mathscore"]=1
model.log = lm(log(mathscore) ~ age + gender + factor(race) + factor(dad_edu), data=dat6) 
summary(model.log)
qqnorm(model.log$residuals)
qqline(model.log$residuals)
## not good 


#4) add higher order term of age
model.ho = lm(mathscore ~ age + age^2 + gender + factor(race) + factor(dad_edu), data=dat5) ## due to 0 mathscore
summary(model.ho)
qqnorm(model.ho$residuals)
qqline(model.ho$residuals)
## not good 



## PART II. MODELING-------------------------------------------------------------------------------------------
##1) rates in math score as a function of age
## fit random effect model, with three interaction terms, group by id
library(lmerTest)
fit_ran <- lmer((sqrt(mathscore))~(1|id)+age + gender + factor(race) + factor(dad_edu) + age*gender + age*factor(race) + age*factor(dad_edu), data=dat5)
summary(fit_ran)
vcov(fit_ran)

#id       (Intercept) 0.4244   0.6515  
#Residual             1.1099   1.0535     


## calculate 95% ci for each change rate in 8 gender*education group
#1. female dad_edu1
est1 =  7.839e-01
ci.f1 =  7.839e-01+c(-1,1)*1.96*1.530e-02 
#2. female dad_edu2
est2 = 7.839e-01 + 1.334e-02
ci.f2 = est2+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-2.210905e-04)+ 1.592e-02^2)
#3. female dad_edu3
est3 = 7.839e-01 + 1.304e-02
ci.f3 = est3+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-2.217271e-04)+1.547e-02^2)
#4. female dad_edu4
est4 = 7.839e-01 - 7.546e-04
ci.f4 = est4+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-2.225971e-04)+1.669e-02^2)


#5. male dad_edu1
est5 = 7.839e-01 + 7.901e-03
ci.f5 = est5+c(-1,1)*1.96*sqrt(1.530e-02^2+2*1.557137e-04+6.211e-03^2)
#6. male dad_edu2
est6 = est5+1.334e-02
ci.f6 = est6+c(-1,1)*1.96*sqrt(1.530e-02^2+1.592e-02^2+6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-2.210905e-04)+
                                 2*1.579553e-05)
#7. male dad_du3
est7 = est5+1.304e-02
ci.f7 = est7+c(-1,1)*1.96*sqrt(1.530e-02^2+1.547e-02^2+6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-2.217271e-04)+
                                 2*1.749730e-05)
#8. male dad_edu4
est8 = est5-7.546e-04
ci.f8 = est8+c(-1,1)*1.96*sqrt(1.530e-02^2+1.669e-02^2++6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-2.225971e-04) +
                                 2*1.625832e-05 )

est = c(est1,est2,est3,est4,est5,est6,est7,est8)
ci = c(ci.f1,ci.f2,ci.f3,ci.f4,ci.f5,ci.f6,ci.f7,ci.f8)
round(est,2)
round(ci,2)
## intraclass correlation
rou <- 1.1099/(1.1099+0.4244) #0.7234
rou

## calculate 95% ci for each change rate in 8 race*education group
#1. female race1
est11 =  7.839e-01
ci.f11 =  7.839e-01+c(-1,1)*1.96*1.530e-02 
#2. female race2
est22 = 7.839e-01 -1.390e-02
ci.f22 = est22+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-3.522905e-05)+ 1.745e-02^2)
#3. female race3
est33 = 7.839e-01 + 1.415e-02
ci.f33 = est33+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-4.093082e-05)+1.522e-02^2)
#4. female race5
est44 = 7.839e-01 + 3.453e-02
ci.f44 = est44+c(-1,1)*1.96*sqrt(1.530e-02^2+2*(-2.201436e-05)+1.544e-02^2)


#5. male race1
est55 = 7.839e-01 + 7.901e-03
ci.f55 = est55+c(-1,1)*1.96*sqrt(1.530e-02^2+2*1.557137e-04+6.211e-03^2)
#6. male race2
est66 = est55-1.390e-02
ci.f66 = est66+c(-1,1)*1.96*sqrt(1.530e-02^2+1.745e-02^2+6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-2.210905e-04)+
                                 2*1.048046e-06)
#7. male race3
est77 = est55+1.415e-02
ci.f77 = est77+c(-1,1)*1.96*sqrt(1.530e-02^2+1.522e-02^2+6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-3.522905e-05)+
                                 2*3.967480e-06)
#8. male race5
est88 = est55 + 3.453e-02
ci.f88 = est88+c(-1,1)*1.96*sqrt(1.530e-02^2+1.544e-02^2++6.211e-03^2+
                                 2*1.557137e-04+
                                 2*(-2.201436e-05) +
                                 2*1.790134e-05)

estr = c(est11,est22,est33,est44,est55,est66,est77,est88)
cir = c(ci.f11,ci.f22,ci.f33,ci.f44,ci.f55,ci.f66,ci.f77,ci.f88)
round(estr,2)
round(cir,2)

