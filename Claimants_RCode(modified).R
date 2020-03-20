claimants <- read.csv(file.choose()) # Choose the claimants Data set
View(claimants)
attach(claimants)

#Perform EDA
#Perform glm
fit1<-glm(ATTORNEY~CLMSEX+CLMINSUR+SEATBELT+CLMAGE+LOSS,data = claimants,family = "binomial")
summary(fit1)
# Linear regression technique can not be employed
prob1 <- predict(fit1,type="response")
prob1
# Logistic Regression 
str(claimants)
logit<-glm(factor(ATTORNEY)~factor(CLMSEX)+factor(CLMINSUR)+factor(SEATBELT)+CLMAGE+LOSS,family=binomial,data = claimants)
#factor is used to get the data as it is without any modifying it. 
summary(logit)

#building the model with seatbelt variable
logit1<-glm(ATTORNEY~factor(CLMSEX)+factor(CLMINSUR)+CLMAGE+LOSS,family=binomial,data = claimants)
summary(logit1)

exp(coef(logit1)) #convert the log values to probability values
table(claimants$ATTORNEY)

# Confusion matrix table 
prob <- predict(logit1,type=c("response"),claimants)
prob
confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prob,claimants$ATTORNEY)
rocrperf<-performance(rocrpred,'tpr','fpr')
#to check the performance of rocrpred wrt to tpr(true positive rate) & fpr(false positive rate)
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained
#text.adj is the position of the axes titles


###work on these concepts
library(aod)
library(InformationValue)

Concordance(claimants$ATTORNEY,confusion)
optimalCutoff(claimants,confusion)
