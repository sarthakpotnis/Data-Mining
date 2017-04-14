
setwd("C:/Users/sarth/Desktop/Data mining/data files")
credit= read.csv("credit3.csv")
credit$NPV<-as.numeric(gsub(",","",credit$NPV))
credit$Profit=rep(0,nrow(credit ))
credit$Profit[credit$NPV >0]=1

credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED)
credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
credit$Profit<- as.numeric(credit$Profit)


####################################################33

library("dummies", lib.loc="~/R/win-library/3.2")
checkaccount <- matrix(dummy(credit$CHK_ACCT, sep = "."), dimnames = list(nrow(0),c("ca0","ca1","ca2","ca3")), nrow=1000, ncol=4)
saveaccount <- matrix(dummy(credit$SAV_ACCT, sep = "."), dimnames = list(nrow(0),c("sa0","sa1","sa2","sa3","sa4")), nrow=1000, ncol=5)
history1<- matrix(dummy(credit$HISTORY, sep = "."), dimnames = list(nrow(0),c("his0","his1","his2","his3","his4")), nrow=1000, ncol=5)
job <- matrix(dummy(credit$JOB, sep = "."), dimnames = list(nrow(0),c("job0","job1","job2","job3")), nrow=1000, ncol=4)
type <- matrix(dummy(credit$TYPE, sep = "."), dimnames = list(nrow(0),c("type0","type1","type2","type3","type4","type5","type6")), nrow=1000, ncol=7)
credit <- cbind(credit, checkaccount,saveaccount, history1,job, type)

str(credit)

credit<- credit[,-c(1,3,4,7,10,20,23)]

normalize <- function(x){ 
  m <- mean(x) 
  std <- sd(x) 
  (x - m)/(std) 
} 
str(credit)
credit[,1:16] <- apply(credit[,c(1:16)],2,normalize)
credit[,18:42] <- apply(credit[,c(18:42)],2,normalize)

library("caret")
set.seed(12345)
inTrain <- createDataPartition(credit$Profit, p=0.7, list=FALSE)
#
dftrain <- data.frame(credit[inTrain,])
dfvalidation <- data.frame(credit[-inTrain,])


library(class)

train_input <- as.matrix(dftrain[,-17])
train_output <- as.vector(dftrain[,17])
validate_input <- as.matrix(dfvalidation[,-17])




kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, validate_input,train_output, k=i)

    # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain$Profit)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation$Profit)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}


plot(c(1,kmax),c(0,0.3),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
legend(9, 0.1, c("Training","Validation"),lty=c(1,1), col=c("red","blue"))
z <- which.min(ER2)
cat("Minimum Validation Error k:", z)

#
prediction <- knn(train_input, train_input,train_output, k=z)
prediction2 <- knn(train_input, validate_input,train_output, k=z)


CM1 <- table( dftrain$Profit,prediction)
CM2 <- table( dfvalidation$Profit, prediction2)

CM1
CM2
####################################################

class0error<- CM2[1,2]/(CM2[1,1]+CM2[1,2])
class0error

class1error<- CM2[2,1]/(CM2[2,1]+CM2[2,2])
class1error

par(mfrow=c(1,1))

##################################################
##################################################

library("caret")
par(mfrow=c(3,4))
for(i in 1:10)
{
  set.seed(i)
inTrain <- createDataPartition(credit$Profit, p=0.7, list=FALSE)
#
dftrain <- data.frame(credit[inTrain,])
dfvalidation <- data.frame(credit[-inTrain,])


library(class)

train_input <- as.matrix(dftrain[,-17])
train_output <- as.vector(dftrain[,17])
validate_input <- as.matrix(dfvalidation[,-17])

kmax <- 15
ER1 <- rep(0,kmax)
ER2 <- rep(0,kmax)
#
for (i in 1:kmax){
  prediction <- knn(train_input, train_input,train_output, k=i)
  prediction2 <- knn(train_input, validate_input,train_output, k=i)
  
  # The confusion matrix for training data is:
  CM1 <- table(prediction, dftrain$Profit)
  # The training error rate is:
  ER1[i] <- (CM1[1,2]+CM1[2,1])/sum(CM1)
  # The confusion matrix for validation data is: 
  CM2 <- table(prediction2, dfvalidation$Profit)
  ER2[i] <- (CM2[1,2]+CM2[2,1])/sum(CM2)
}


plot(c(1,kmax),c(0,0.3),type="n", xlab="k",ylab="Error Rate")
lines(ER1,col="red")
lines(ER2,col="blue")
z <- which.min(ER2)
cat("  Minimum Validation Error k: ", z)

}

##########################################################################################

par(mfrow=c(1,1))
setwd("C:/Users/sarth/Desktop/Data mining/data files")
credit= read.csv("credit3.csv")
credit$NPV<-as.numeric(gsub(",","",credit$NPV))
credit$Profit=rep(0,nrow(credit ))
credit$Profit[credit$NPV >0]=1

credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED)
credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
credit$Profit<- as.numeric(credit$Profit)
str(credit)
credit<- credit[,-c(23)]

credit$Profit<- as.factor(credit$Profit)
credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$NUM_CREDITS <- as.factor(credit$NUM_CREDITS)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$PRESENT_RESIDENT <- as.factor(credit$PRESENT_RESIDENT)
credit$EMPLOYMENT <- as.factor(credit$EMPLOYMENT)
credit$JOB <- as.factor(credit$JOB)
credit$NUM_DEPENDENTS <- as.factor(credit$NUM_DEPENDENTS)
credit$RENT <- as.factor(credit$RENT)
credit$INSTALL_RATE <- as.factor(credit$INSTALL_RATE)
credit$GUARANTOR  <- as.factor(credit$GUARANTOR )
credit$OTHER_INSTALL  <- as.factor(credit$OTHER_INSTALL)
credit$OWN_RES <- as.factor(credit$OWN_RES)
credit$TELEPHONE<- as.factor(credit$TELEPHONE)
credit$FOREIGN<- as.factor(credit$FOREIGN)
credit$REAL_ESTATE<- as.factor(credit$REAL_ESTATE)
credit$TYPE<- as.factor(credit$TYPE)

library("caret")
set.seed(12345)
inTrain <- createDataPartition(credit$Profit, p=0.7, list=FALSE)
#
dftrain <- data.frame(credit[inTrain,])
dfvalidation <- data.frame(credit[-inTrain,])

####################################################

library(e1071)
# Can handle both categorical and numeric input, 
# but output must be categorical
model <- naiveBayes(dftrain$Profit~., data=dftrain)
model
prediction <- predict(model, newdata = dfvalidation[,-24])

####################################################

table(dfvalidation$Profit,prediction,dnn=list('actual','predicted'))
model$apriori

# For class probabilities
predicted.probability <- predict(model, newdata = dfvalidation[,-24], type="raw")
class1_Prob<-mean(predicted.probability[,2])
class1_Prob
class0_Prob<-mean(predicted.probability[,1])
class0_Prob

####################################################
test<- data.frame(cbind(27,1,4,1,12,1,1,1,2,1,1,3,0,0,0,1,0,0,2,4500,0))
predicted.probability <- predict(model, newdata = test[], type="raw")
predicted.probability


#########################################
########naïve Bayes model
par(mfrow=c(1,1))
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)
roc.table <- data.frame(Cutoff = cutoff,FPR = fpr,TPR = tpr)
p_prob <- predict(model, dfvalidation[,-18],type="raw")[,2]
for (i in 1:100) {
  roc.table$FPR[i] <- sum(p_prob > cutoff[i] & dfvalidation$Profit == 0)/sum(dfvalidation$Profit == 0)
  roc.table$TPR[i] <- sum(p_prob > cutoff[i] & dfvalidation$Profit == 1)/sum(dfvalidation$Profit == 1)
}
plot(TPR ~ FPR, data = roc.table, type = "l",xlab="1 - Specificity",ylab="Sensitivity",col="blue")

##############################################
##logistic regression model
Performance= glm(dfvalidation$Profit~ dfvalidation$AGE+dfvalidation$CHK_ACCT+dfvalidation$NUM_CREDITS+dfvalidation$SAV_ACCT+dfvalidation$DURATION+dfvalidation$HISTORY+dfvalidation$PRESENT_RESIDENT+dfvalidation$EMPLOYMENT+dfvalidation$JOB+dfvalidation$NUM_DEPENDENTS+dfvalidation$RENT+dfvalidation$GUARANTOR+dfvalidation$INSTALL_RATE+dfvalidation$TYPE+dfvalidation$OWN_RES+dfvalidation$OTHER_INSTALL+dfvalidation$OWN_RES+dfvalidation$TELEPHONE+dfvalidation$FOREIGN+dfvalidation$REAL_ESTATE+dfvalidation$AMOUNT_REQUESTED,data = dfvalidation, family = binomial)
Performance

cutoff <- 0.5
predicted.probability.test <- predict(Performance, type = "response")
Predicted <- ifelse( predicted.probability.test > cutoff, 1, 0)
Actual <-  dfvalidation$Profit


cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)
Actual <- dfvalidation$Profit

roc.table <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
for (i in 1:100) {
  roc.table$FPR[i] <- sum(predicted.probability.test > cutoff[i] & Actual == 0)/sum(Actual == 0)
  roc.table$TPR[i] <- sum(predicted.probability.test > cutoff[i] & Actual == 1)/sum(Actual == 1)
}
lines(TPR ~ FPR, data = roc.table,col="green")

##############################################
##Knn
cutoff <- seq(0, 1, length = 100)
fpr <- numeric(100)
tpr <- numeric(100)
roc.table3 <- data.frame(Cutoff = cutoff, FPR = fpr,TPR = tpr)
Actual= dfvalidation$Profit
Actual
prediction5 <- knn(train_input, validate_input,train_output, prob = T, k=9)
prediction5
pred_prob = attr(prediction5, "prob")
pred_prob
for (i in 1:100) {
  roc.table3$FPR[i] <- sum(pred_prob >= cutoff[i] & Actual == 0)/sum(Actual == 0)
  roc.table3$TPR[i] <- sum(pred_prob >= cutoff[i] & Actual == 1)/sum(Actual == 1)
}

lines(TPR ~ FPR, data = roc.table3, col="violet")
abline(a = 0, b = 1, lty = 2,col="red")

legend(0.5, 0.3, c("logistic regression model","naïve Bayes model","KNN"),lty=c(1,1), col=c("green","blue","violet"))







