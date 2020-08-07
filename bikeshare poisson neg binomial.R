## ISOM5610 Final Project

rm(list = ls())  # remove the variables in R's memory
setwd("C:/Users/lokho/Desktop/ISOM5610 - business analytics/Final Project/") #set directory

#bsd <- read.table("BikeShareByDay.csv",header = T, sep=",") #Read data by day
bsh <- read.csv("BikeShareByHour.csv",header = T) #Read data by hour

attach(bsh)
summary(bsh)

#remove date as we us use instant as time index
bsh[,2] <- NULL
#remove working day as it is overlap with weekday and holiday
bsh[,8]<-NULL
#rearrange the order
bsh<-bsh[,c(13:15,1:12)]


#transform categorical as factor
bsh$season<-as.factor(season)
bsh$hr<-as.factor(hr)
bsh$mnth<-as.factor(mnth)
bsh$holiday<-as.factor(holiday)
bsh$weekday<-as.factor(weekday)
bsh$weathersit<-as.factor(weathersit)

#TimePlot of count of bike use
par(mfrow=c(1,1))
plot(instant,cnt, main='TimePlot of daily usage')

#pair relationship between reg vs causal and hours
pairs(cbind(casual,registered,hr))

#initial fit of Poisson regression
# for registered user
poi.reg.fit=glm(registered~.-cnt-casual-instant-temp,data=bsh,family='poisson')
summary(poi.reg.fit)
#for casual user
poi.cas.fit=glm(casual~.-cnt-registered-instant-temp,data=bsh,family='poisson')
summary(poi.cas.fit)

#check VIF
library(car)
vif(poi.reg.fit)
vif(poi.cas.fit)
#multi-collinearity problem observed.

#Checking Overdispersion
c(mean(bsh$registered),var(bsh$registered))
c(mean(bsh$casual),var(bsh$casual))
#overdispersion observed.

#negative biominal regression
library(MASS)
nb.reg.fit=glm.nb(registered~.-cnt-casual-instant-temp,data=bsh)
summary(nb.reg.fit)

nb.cas.fit=glm.nb(casual~.-cnt-registered-instant-temp,data=bsh)
summary(nb.cas.fit)

#overdispersion test between poisson regression and NB regression
#LR test
#install.packages("pscl")
library(pscl)
odTest(nb.reg.fit)
odTest(nb.cas.fit)

#Cameron and Trivedi test
#install.packages("AER")
library(AER)
dispersiontest(poi.reg.fit,trafo=2)
dispersiontest(poi.cas.fit,trafo=2)


#LM test
LM_reg=sum((poi.reg.fit$y-poi.reg.fit$fitted.values)^2-poi.reg.fit$y)^2/(2*sum((poi.reg.fit$fitted.values)^2))
p_LM_reg=1-pchisq(LM_reg,1)
p_LM_reg

LM_cas=sum((poi.cas.fit$y-poi.cas.fit$fitted.values)^2-poi.cas.fit$y)^2/(2*sum((poi.cas.fit$fitted.values)^2))
p_LM_cas=1-pchisq(LM_cas,1)
c(p_LM_reg,p_LM_cas)



#deviance goodness of fit test
1-pchisq(nb.reg.fit$deviance,nb.reg.fit$df.residual,lower.tail=FALSE)

1-pchisq(nb.cas.fit$deviance,nb.cas.fit$df.residual,lower.tail=FALSE)

#1 indicate the model are good enough compare to the saturated model

#Persudo R-sq
reg_prsq = 1- nb.reg.fit$deviance/nb.reg.fit$null.deviance

cas_prsq = 1- nb.cas.fit$deviance/nb.cas.fit$null.deviance

c(reg_prsq, cas_prsq)

#ANOVA
anova(nb.reg.fit)
anova(nb.cas.fit)

#model diagnostoc plots
par(mfrow=c(2,2))
plot(nb.reg.fit)
plot(nb.cas.fit)

#additional
stdreg=rstandard(nb.reg.fit)
par(mfrow=c(2,2))
hist(stdreg,main='Histogram - Registered',xlab='Standardized Residual')
plot(instant,stdreg,type="o",main='Versus Order - Registered',xlab='TimeIndex',ylab='Standardized Residual')
abline(0,0,lty=3)
stdcas=rstandard(nb.cas.fit)
hist(stdcas,main='Histogram - Casual',xlab='Standardized Residual')
plot(instant,stdcas,type="o",main='Versus Order - Casual',xlab='TimeIndex',ylab='Standardized Residual')
abline(0,0,lty=3)
par(mfrow=c(1,1))


#exp(beta) with 95% CI
exp(confint(nb.reg.fit))

exp(confint(nb.cas.fit))

#conditional mean for 4 seasons


