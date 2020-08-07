rm(list = ls())
#setwd("C:/Users/leung/Dropbox/MSBA/ISOM5610/Project")  # set directory

BikeShare <- read.table("BikeShareByDay.csv",header = T, sep=",") # Read Data
BikeShare2 <- BikeShare # for Lasso use

attach(BikeShare) # Attach Table

par(mfrow=c(1,1))
plot(instant,casual,main='TimePlot of Count',type="l",col="blue",xlab="Instant",ylab="Bike Usage",ylim=c(0, 8000))
lines(instant,registered,type="l",col="red")
lines(instant,cnt,type="l",col="black")
legend("topleft",c("casual","registered","cnt"),fill=c("blue","red","black"))

library(corrplot)
corrplot(cor(BikeShare[10:16]), method = "ellipse",type = "upper")

#pairs(cnt~instant+factor(season)+yr+factor(mnth)+holiday+factor(weekday)+factor(weathersit)+temp+atemp+hum+windspeed)
pairs(cbind(cnt,instant,season,yr,mnth,holiday,weekday,weathersit,temp,atemp,hum,windspeed))

### transform variables into factors
BikeShare$season<-as.factor(season)
BikeShare$mnth<-as.factor(mnth)
BikeShare$holiday<-as.factor(holiday)
BikeShare$weekday<-as.factor(weekday)
BikeShare$workingday<-as.factor(workingday)
BikeShare$weathersit<-as.factor(weathersit)

##############################################
# split training and test set

row80pc<-.8*nrow(BikeShare)
sample <- 1:row80pc
train <- BikeShare[sample, ]
test  <- BikeShare[-sample, ]

# model 1 is the full model on CNT
Model.1T_cnt<-lm(cnt~instant+season+yr+mnth+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train)
summary(Model.1T_cnt)

#forward selection with AIC
start_model<-lm(cnt~1,data=train)

Model.2T_cnt<-step(start_model, direction = "forward", scope=formula(Model.1T_cnt))
summary(Model.2T_cnt) # Display an overview of the forward selection
coef(Model.2T_cnt)

# test for multicollinearity
library(car)
vif(Model.1T_cnt)

# take our instant and temp,  build a smaller model
Model.5_cnt<-lm(cnt~season+yr+mnth+holiday+weekday+weathersit+atemp+hum+windspeed,data=train)
anova(Model.5_cnt)
summary(Model.5_cnt)


vif(Model.5_cnt)

#PCR
library(pls)

Model.3T_cnt=pcr(cnt~instant+season+yr+mnth+holiday+weekday+weathersit+temp+atemp+hum+windspeed,data=train, scale = FALSE, validation = "CV" , segments=10, segment.type = "consecutive")
summary(Model.3T_cnt)

par(mfrow=c(1,1))
validationplot(Model.3T_cnt, val.type="RMSEP",ncomp = 1:28,legend='topright')
coefplot(Model.3T_cnt,ncomp = 1:25)


#selected PCR model with 25 components.
pcr.fit=pcr(cnt~instant+season+yr+mnth+holiday+weekday+weathersit+temp+atemp+hum+windspeed,ncomp=25,data=train, scale = FALSE, validation = "CV",segments=10, segment.type = "consecutive")
summary(lm(train[,16]~pcr.fit$scores[,1:25]))
#pcr.fit["loadings"]
coefficients(pcr.fit,25)
# this is the coefficients

#Convert season to dummies
SDummies<-sapply(unique(unlist(BikeShare$season)), function(j) as.numeric(grepl(j, BikeShare$season)))
for (col in 2:ncol(SDummies)){
  BikeShare2 <- cbind(BikeShare2, SDummies[,col])
  names(BikeShare2)[ncol(BikeShare2)]<-paste("SeasonDummies_",col,sep = "")
}
BikeShare2$season<-NULL

#Convert mnth to dummies
MDummies=sapply(unique(unlist(BikeShare$mnth)), function(j) as.numeric(grepl(j, BikeShare$mnth)))
for (col in 2:ncol(MDummies)){
  BikeShare2 <- cbind(BikeShare2, MDummies[,col])
  names(BikeShare2)[ncol(BikeShare2)]<-paste("MnthDummies_",col,sep = "")
}
BikeShare2$mnth<-NULL

#Convert weekday to dummies
WDDummies=sapply(unique(unlist(BikeShare$weekday)), function(j) as.numeric(grepl(j, BikeShare$weekday)))
for (col in 2:ncol(WDDummies)){
  BikeShare2 <- cbind(BikeShare2, WDDummies[,col])
  names(BikeShare2)[ncol(BikeShare2)]<-paste("WeekdayDummies_",col,sep = "")
}
BikeShare2$weekday<-NULL

#Convert weathersit to dummies
WSDummies=sapply(unique(unlist(BikeShare$weathersit)), function(j) as.numeric(grepl(j, BikeShare$weathersit)))
for (col in 2:ncol(WSDummies)){
  BikeShare2 <- cbind(BikeShare2, WSDummies[,col])
  names(BikeShare2)[ncol(BikeShare2)]<-paste("WeathersitDummies_",col,sep = "")
}
BikeShare2$weathersit<-NULL

BikeShare2$dteday<-NULL
BikeShare2$workingday<-NULL
BikeShare2$casual<-NULL
BikeShare2$registered<-NULL

# Now Selecting 80% of data as sample from total 'n' rows of the data  
train2 <- BikeShare2[sample, ]
test2  <- BikeShare2[-sample, ]

library(glmnet)
Model.4T_cnt=cv.glmnet(as.matrix(train2[,-8]), as.matrix(train2[,8]),alpha=1,family="gaussian", nfolds=5)
plot(Model.4T_cnt)
abline( v = log(Model.4T_cnt$lambda.1se),col="blue", lty=2)
plot(glmnet(as.matrix(train2[,-8]), as.matrix(train2[,8]),alpha=1,family="gaussian"),xvar='lambda')
abline( v = log(Model.4T_cnt$lambda.1se),col="blue", lty=2)
lasso.fit = glmnet(as.matrix(train2[,-8]), as.matrix(train2[,8]),alpha=1,family="gaussian",lambda=Model.4T_cnt$lambda.1se)
summary(lasso.fit)
lasso.fit$beta

## Calculate test set MSE for each of the model built

Model1MSE=mean((predict(Model.1T_cnt,test)-test[,16])^2) # full-model
Model2MSE=mean((predict(Model.2T_cnt,test)-test[,16])^2) # AIC
Model3MSE=mean((predict(pcr.fit,test)-test[,16])^2) # PCR
Model4MSE=mean((predict(Model.4T_cnt,s=Model.4T_cnt$lambda.1se, as.matrix(test2[,-8]))-test2[,8])^2) # Lasso
Model5MSE=mean((predict(Model.5_cnt,test)-test[,16])^2) # small model manually remove instant and temp

summary(Model1MSE)
summary(Model2MSE)
summary(Model3MSE)
summary(Model4MSE)
summary(Model5MSE)

## Model Diagostic on Model 5
#qqplot and residual plot
par(mfrow=c(1,2))
plot(Model.5_cnt,which=1:2)


stdr5=rstandard(Model.5_cnt)
par(mfrow=c(1,2))
hist(stdr5,main='Histogram - CNT',xlab='Standardized Residual')
plot(train$instant,stdr5,type="o",main='Versus Order - CNT',xlab='TimeIndex',ylab='Standardized Residual')
abline(0,0,lty=3)

#BP test
library(car)
ncvTest(Model.5_cnt)


#additional diagostic on model 2
library(MASS)
summary(lm(Model.5_cnt$residuals~lag(Model.5_cnt$residuals,1)))



par(mfrow=c(1,2))

# test for auto-correlation
acf(Model.5_cnt$residual, main="ACF Plot")
pacf(Model.5_cnt$residual,main="PACF Plot")

par(mfrow=c(1,1))

# AIC for AR
AR<-ar(Model.5_cnt$residual) 
ts.plot(AR$aic[1:10],xlab=" ",main="AIC Plot")
AR$order

#fit ARIMA model for prediction on residual
fit_residual  <- arima(Model.5_cnt$residual,order = c(9,0,0))
fit_residual

### model diagnostics
tsdiag(fit_residual)
qqnorm(fit_residual$residuals)
plot(train$instant,fit_residual$residuals,type="o",main='AR6 - residual on Model5 res',xlab='TimeIndex',ylab='Residual')
acf(fit_residual$residuals)

# residual error after applying ARIMA
fit_residual$sigma2^(1/2)
Model5MSE^0.5

plot(Model.5_cnt$residual,xlab="Instant",ylab="Residuals",main="Residuals vs Time")
plot(Model.5_cnt$fitted.values,Model.5_cnt$residual,xlab="Fitted Values",ylab="Residuals",main="Residuals vs Fitted")
qqnorm(Model.5_cnt$residuals)


par(mfrow=c(1,2))
plot(train$instant,Model.5_cnt$residual,type="o",main='Versus Order - model 5 residual',xlab='TimeIndex',ylab='Standardized Residual')
plot(train$instant,fit_residual$residuals,type="o",main='Versus Order - Fitted residual',xlab='TimeIndex',ylab='Standardized Residual')

### Model on Registered and Casual user
Model.5_reg<-lm(registered~season+yr+mnth+holiday+weekday+weathersit+atemp+hum+windspeed,data=train)
anova(Model.5_reg)
summary(Model.5_reg)

Model.5_cas<-lm(casual~season+yr+mnth+holiday+weekday+weathersit+atemp+hum+windspeed,data=train)
anova(Model.5_cas)
summary(Model.5_cas)

