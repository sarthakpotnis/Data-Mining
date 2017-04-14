#2)	The goal is to use logistic regression to predict whether or not a new credit will result in a profitable account.  Create a new variable to use as the dependent variable in the model.


setwd("C:/Users/sarth/Desktop/Data mining/data files")
credit= read.csv("credit3.csv")
credit$NPV<-as.numeric(gsub(",","",credit$NPV))
credit$Profit=rep(0,nrow(credit ))
credit$Profit[credit$NPV >0]=1
credit$Profit=as.factor(credit$Profit)
credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))

#3)	Create appropriate dummy variables for all categorical variables with more than 2 values.

library("dummies", lib.loc="~/R/win-library/3.2")
checkaccount <- matrix(dummy(credit$CHK_ACCT, sep = "."), dimnames = list(nrow(0),c("ca0","ca1","ca2","ca3")), nrow=1000, ncol=4)
saveaccount <- matrix(dummy(credit$SAV_ACCT, sep = "."), dimnames = list(nrow(0),c("sa0","sa1","sa2","sa3","sa4")), nrow=1000, ncol=5)
numcredit <- matrix(dummy(credit$NUM_CREDITS, sep = "."), dimnames = list(nrow(0),c("nc1","nc2","nc3","nc4")), nrow=1000, ncol=4)
history1<- matrix(dummy(credit$HISTORY, sep = "."), dimnames = list(nrow(0),c("his0","his1","his2","his3","his4")), nrow=1000, ncol=5)
present <- matrix(dummy(credit$PRESENT_RESIDENT, sep = "."), dimnames = list(nrow(0),c("pr1","pr2","pr3","pr4")), nrow=1000, ncol=4)
employee <- matrix(dummy(credit$EMPLOYMENT, sep = "."), dimnames = list(nrow(0),c("emp0","emp1","emp2","emp3","emp4")), nrow=1000, ncol=5)
job <- matrix(dummy(credit$JOB, sep = "."), dimnames = list(nrow(0),c("job0","job1","job2","job3")), nrow=1000, ncol=4)
install <- matrix(dummy(credit$INSTALL_RATE, sep = "."), dimnames = list(nrow(0),c("ins1","ins2","ins3","ins4")), nrow=1000, ncol=4)
type <- matrix(dummy(credit$TYPE, sep = "."), dimnames = list(nrow(0),c("type0","type1","type2","type3","type4","type5","type6")), nrow=1000, ncol=7)

credit <- cbind(credit, checkaccount,saveaccount, numcredit, history1, present, employee, job, install, type)

#4)	Split the data into 2 parts, training (70%) and validation (30%). Set the seed to 12345.

set.seed(12345)
inTrain <- sample(nrow(credit), 0.7*nrow(credit))
train <- data.frame(credit[inTrain,])
test <- data.frame(credit[-inTrain,])

#5)	Fit a Logistic Regression Model with all the relevant variables. 

Performance= glm(Profit~ AGE+ca0+ca1+ca2+NUM_CREDITS+sa0+sa1+sa2+sa3+DURATION+his0+his1+his2+his3+pr1+pr2+pr3+emp0+emp1+emp2+emp3+job0+job1+job2+NUM_DEPENDENTS+RENT+GUARANTOR+INSTALL_RATE+type0+type1+type2+type3+type4+type5+OWN_RES+OTHER_INSTALL+OWN_RES+TELEPHONE+FOREIGN+REAL_ESTATE+AMOUNT_REQUESTED,data = train, family = binomial)
Performance

#What are the odds associated with the purpose of the credit request being education (TYPE=5)?
odds<-exp(-1.2558732)
odds

cutoff <- 0.5
Actual<- train$Profit
predicted.probability.train <- predict(Performance, type = "response")
Predicted <- ifelse( predicted.probability.train > cutoff, 1, 0)
confusion1 <- table(Actual, Predicted)
confusion1
specificity1 <- confusion1[1]/(confusion1[1]+confusion1[3])
specificity1
sensitivity1 <- confusion1[4]/(confusion1[4]+confusion1[2])
sensitivity1
predicted.probability.test <- predict(Performance, type = "response", newdata = test)
Predicted <- ifelse( predicted.probability.test > cutoff, 1, 0)
Actual <- test$Profit
confusion<- table(Actual, Predicted)
confusion
sensitivity2 <- confusion[4]/(confusion[4]+confusion[2])
sensitivity2
specificity2 <- confusion[1]/(confusion[1]+confusion[3])
specificity2

#6)	We now want to compare the predictive performance of the model on the training sample and the validation sample. 
#Create a figure that shows the ROC curves for both the training sample and the validation sample. 
#Attach the figure as Exhibit 2.

cutoff <- seq(0, 1, length = 1000)
fpr <- numeric(1000)
tpr <- numeric(1000)
Actual <- test$Profit

roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
for (i in 1:1000) {
  roc.table$FPR[i] <- sum(predicted.probability.test > cutoff[i] & Actual == 0)/sum(Actual == 0)
  roc.table$TPR[i] <- sum(predicted.probability.test > cutoff[i] & Actual == 1)/sum(Actual == 1)
}
plot(TPR ~ FPR, data = roc.table, type = "s",xlab="1 - Specificity",ylab="Sensitivity",col="blue")
abline(a = 0, b = 1, lty = 2,col="red")
cutoff <- seq(0, 1, length = 1000)
fpr <- numeric(1000)
tpr <- numeric(1000)
Actual <- train$Profit
for (i in 1:1000) {
  roc.table$FPR[i] <- sum(predicted.probability.train > cutoff[i] & Actual == 0)/sum(Actual == 0)
  roc.table$TPR[i] <- sum(predicted.probability.train > cutoff[i] & Actual == 1)/sum(Actual == 1)
}

lines(TPR~FPR,data = roc.table, type="s",col="green")


#Is there a difference in the prediction accuracy of two samples (the training data and the validation data)?

accuracy.train<-(confusion1[4]+ confusion1[1])/(confusion1[1]+confusion1[3]+confusion1[4]+confusion1[2])
accuracy.train
accuracy.test<-(confusion[4]+ confusion[1])/(confusion[1]+confusion[3]+confusion[4]+confusion[2])
accuracy.test
#7)	Based on the training data, if the goal was to maximize accuracy, what cut-off value would you choose? 

train0<-train
train.actual<-train0$Profit
train.prob<-predict(Performance,newdata=train0,type="response")
t<-seq(0,1,by=0.01)
accuracy.train<-c()
cutoff.train<-c()
for (i in t) {
  cutoff0<-i
  train.predicted<-ifelse(predicted.probability.train>cutoff0,1,0)
  A<-sum(train.actual == train.predicted)/nrow(train0)
  accuracy.train<-append(accuracy.train,A)
  cutoff.train<-append(cutoff.train,i)
  flush.console()
}
cbind(accuracy.train, cutoff.train)
max(accuracy.train)

#8)	Applying the cut-off value to the validation data, what is the estimated accuracy of the model you selected?

predicted.probability.test <- predict(Performance, type = "response", newdata = test)
Predicted <- ifelse( predicted.probability.test > 0.48, 1, 0)
Actual <- test$Profit
confusion3<- table(Actual, Predicted)
confusion3

#9)	Based on the training data, what is the average profit of a profitable account? 

accuracy.train.2<-(confusion3[4]+ confusion3[1])/(confusion3[1]+confusion3[3]+confusion3[4]+confusion3[2])
accuracy.train.2
averageprofit<- sum(train$NPV[train$NPV>0])/length(train$NPV[train$NPV>0])
averageprofit

#10)	Based on the training data, what is the average loss of a delinquent account? 

averageloss<- (sum(train$NPV[train$NPV<=0])/length(train$Profit[train$NPV<=0]))*(-1)
averageloss

#11)	If we classify a non-profitable account as profitable, our estimated loss is your answer to 10). 
#     If you classify a profitable account as non-profitable, our opportunity loss is the answer to 9).

c<-seq(0,1,by=0.01)
cutoff.valid<-c()
x.valid<-c()
y.valid<-c()
for (i in c) {
  cutoff.valid<-append(cutoff.valid,i)
  x.valid<-append(x.valid,averageloss*(sum(predicted.probability.test > i & Actual == 0)))
  y.valid<-append(y.valid,averageprofit*(sum(predicted.probability.test< i & Actual == 1)))
}
cost.valid<-x.valid+y.valid
miss.cost<-data.frame(cutoff.valid,cost.valid)
plot(cost.valid~cutoff.valid,data=miss.cost,type="l",xlab="cutoff",ylab="Misclassification Costs",lwd=2,lty=2,main="Misclassification Cost")
miss.cost$cost.valid[which.min(miss.cost$cost.valid)] 
miss.cost$cutoff.valid[which.min(miss.cost$cost.valid)]

#12)	What is the misclassification cost associated with doing nothing (that is not offering any accounts)?
test$baseline2= "0"
t2<-table(test$Profit,test$baseline2, dnn=c("Actual","Predicted"))
t2
miss.costdoingnthng<-t2[2,1]*averageprofit
miss.costdoingnthng

test$baseline1= "1"
t1<-table(test$Profit,test$baseline1, dnn=c("Actual","Predicted"))
t1
miss.costdoingall <- averageloss*t1[1,1]
miss.costdoingall

#As we saw in our comparison of the ROC curves of the training and validation samples, 
#there was a gap between these two curves. This can be a sign of overfitting,
#which is not surprising given the number of dummy variables and other variables that we included in the model. 
#One way to fight overfitting is variable selection - that is, 
#carefully select which variables enter a model and so build a more parsimonious model. 
#Can you find a set of predictor variables that give you better performance on the validation data set?

step(Performance)
Performance_new<- glm(formula = Profit ~ ca0 + ca1 + sa0 + sa1 + DURATION + his0 + 
      his1 + his2 + pr1 + emp1 + emp2 + RENT + INSTALL_RATE + type1 + 
      type2 + type5 + OTHER_INSTALL + FOREIGN + AMOUNT_REQUESTED, 
    family = binomial, data = train)

#14) What is the cut off value c that maximizes the accuracy on the training data?

train1<-train
train.actual1<-train1$Profit
train.prob1<-predict(Performance_new,newdata=train1,type="response")
t<-seq(0,1,by=0.01)
accuracy.train1<-c()
cutoff.train1<-c()
for (i in t) {
  cutoff1<-i
  train.predicted1<-ifelse(predicted.probability.train>cutoff1,1,0)
  A<-sum(train.actual1 == train.predicted1)/nrow(train1)
  accuracy.train1<-append(accuracy.train1,A)
  cutoff.train1<-append(cutoff.train1,i)
  flush.console()
}
cbind(accuracy.train1, cutoff.train1)

#15)	What is the corresponding accuracy?

max(accuracy.train1)

#16)	What is the accuracy on the validation sample using the cut off value you found in 14)?

predicted.probability.test <- predict(Performance, type = "response", newdata = test)
Predicted <- ifelse( predicted.probability.test > 0.49, 1, 0)
Actual <- test$Profit
confusion3<- table(Actual, Predicted)
confusion3
accuracy.train.2<-(confusion3[4]+ confusion3[1])/(confusion3[1]+confusion3[3]+confusion3[4]+confusion3[2])
accuracy.train.2
