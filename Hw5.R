
#1)Open the file credit3.xlsx. Create the outcome variable 
#  (PROFITABLE=1 if NPV>0, =0 otherwise), create factors (or dummy variables)
# for CHK_ACCT, SAV_ACCT, HISTORY, JOB and TYPE variables. Split the data using the 
# sample function; 70% as training data and 30% as test data; setting the seed to 12345. 

setwd("C:/Users/sarth/Desktop/Data mining/data files")

library(tree)
library(ISLR)

credit= read.csv("credit3.csv")
credit$NPV<-as.numeric(gsub(",","",credit$NPV))

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


credit$profitable = ifelse(credit$NPV>0, 1, 0)

credit$CHK_ACCT = as.factor(credit$CHK_ACCT)
credit$SAV_ACCT = as.factor(credit$SAV_ACCT)
credit$HISTORY = as.factor(credit$HISTORY)
credit$JOB = as.factor(credit$JOB)
credit$TYPE = as.factor(credit$TYPE)
credit$profitable= as.factor(credit$profitable)
credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED)
credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
set.seed(12345)
training = sample(nrow(credit), 0.7*nrow(credit))
credit$OBS.=NULL




#changing numeric values to factors
credit$profitable<- as.factor(credit$profitable)

df_train=credit[training,]
df_val=credit[-training,]


#2)	Run the Classification Tree algorithm using the data,
#    with the PROFITABLE as the output variable. Use the K-fold cross-validation 
#    (with K = 10) to prune back the tree. Attach the classification confusion matrix 
#    for the test as Exhibit 1 and a figure of the pruned tree as Exhibit 2. 

#install.packages("tree")
library("tree", lib.loc="~/R/win-library/3.2")
attach(credit)
tree.credit=tree(profitable~.-NPV-CREDIT_EXTENDED,data=df_train)
summary(tree.credit)
plot(tree.credit)
text(tree.credit,pretty=0)
tree.pred=predict(tree.credit,df_val,type="class")
table(tree.pred,df_val$profitable)
(46+180)/300

cv.credit=cv.tree(tree.credit,FUN=prune.misclass)
names(cv.credit)
cv.credit
par(mfrow=c(1,2))
plot(cv.credit$size,cv.credit$dev,type="b")
plot(cv.credit$k,cv.credit$dev,type="b")
par(mfrow=c(1,1))
prune.credit=prune.misclass(tree.credit,best=9)
plot(prune.credit)
text(prune.credit,pretty=0)
tree.pred1=predict(prune.credit,df_val,type="class")
table(tree.pred1,df_val$profitable)
(33+189)/300

#4)	How would the tree classify our student from the previous example
#(the student is 27 years old domestic, has $100 in her checking account 
#but no savings account. The applicant has 1 existing credits, and a credit 
#duration of 12 months, and the credit was paid back duly.  The applicant has
##been renting her current place for less than 12 months, does not own any real 
#estate, just started graduate school (the present employment variable is set to
#1 and nature of job to 2). The applicant has no dependents and no guarantor. 
#The applicant wants to buy a used car and has requested $4,500 in credit, and 
#therefore the Installment rate is quite high or 2.5%, however the applicant does 
#not have other installment plan credits. Finally, the applicant has a phone in her name)


df3 <- data.frame(AGE=27,CHK_ACCT="1",SAV_ACCT="0",NUM_CREDITS="1",DURATION=12,
                  HISTORY="2",PRESENT_RESIDENT="1",EMPLOYMENT="1",JOB="2",NUM_DEPENDENTS="0",
                  RENT="1",INSTALL_RATE="3",GUARANTOR="0",OTHER_INSTALL="0",OWN_RES="0",
                  TELEPHONE="1",FOREIGN="0",REAL_ESTATE="0",TYPE="2",AMOUNT_REQUESTED=4500,
                  CREDIT_EXTENDED = 0, NPV=0)
tree.pred.profitable=predict(prune.credit,df3,type="class")
tree.pred.profitable
prune.credit
#5)	Find the best pruned tree with only 4 terminal nodes. Describe the rule in 
  #words (in English)
prune.credit=prune.misclass(tree.credit,best=4)
plot(prune.credit)
text(prune.credit,pretty=0)


#6)	Run a Regression Tree Algorithm to predict the NPV of each applicant.
#Use a pruned tree to score the data samples. Attach the pruned tree as Exhibit 3.

credit= read.csv("credit3.csv")
credit$NPV<-as.numeric(gsub(",","",credit$NPV))

credit$CHK_ACCT = as.factor(credit$CHK_ACCT)
credit$SAV_ACCT = as.factor(credit$SAV_ACCT)
credit$HISTORY = as.factor(credit$HISTORY)
credit$JOB = as.factor(credit$JOB)
credit$TYPE = as.factor(credit$TYPE)
credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED)
credit$CREDIT_EXTENDED <- as.numeric(credit$CREDIT_EXTENDED)
set.seed(12345)
training = sample(nrow(credit), 0.7*nrow(credit))
credit$OBS.=NULL



#changing numeric values to factors

df_train=credit[training,]
df_val=credit[-training,]

#
#
tree.credit.reg = tree(NPV~.,df_train)
summary(tree.credit.reg)
#
plot(tree.credit.reg)
text(tree.credit.reg,pretty=0)
#
# Prune the tree by computing validation data error for this and pruned trees
# This tree has eight terminal nodes
#

cv.credit=cv.tree(tree.credit.reg)
names(cv.credit)
plot(cv.credit$size,cv.credit$dev,type="b")
prune.credit=prune.tree(tree.credit.reg,best=9)
plot(prune.credit)
text(prune.credit,pretty=0)

#7)

tree.pred1=predict(prune.credit,df_val)
df_val<- data.frame(df_val,tree.pred1)
df<-table(tree.pred1)
df
data<- tapply(df_val$NPV, tree.pred1, sum)
data
npv.total<-cbind(df,data )
colnames(npv.total)=c("number of values", "total of actual NPV")
npv.total

profit.customer<- length(tree.pred1[tree.pred1>0])
profit.customer

average.profit<- mean(tree.pred1[tree.pred1>0])
average.profit
overall.profit<- sum(tree.pred1[tree.pred1>0])
overall.profit
extend.credit.everyone<- sum(df_val$NPV)
extend.credit.everyone

#9)

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

df_train1=credit[training,]
df_val1= credit[-training,]

performance = lm(NPV~.-CREDIT_EXTENDED,data=df_train1)
summary(performance)
predicted <- predict(performance) 
df_train1$predNPV = predicted

predictedNPV_lm=df_train1$predNPV
min.npv<-min(predictedNPV_lm)

max.npv<-max(predictedNPV_lm)
sum1= c()
co = c()
cutoff <- seq(min.npv, max.npv, by= 1 )
for (d in cutoff) {
  NPV1=c()
  NPV1= ifelse((predictedNPV_lm > d),df_train1$NPV,0)
  a=sum(NPV1)
  sum1=append(sum1,a)
  co= append(co,d)
}

z= data.frame(sum1,co)
z$co[which.max(z$sum1)]


#10)	Apply the cut-off value to the test sample.
#How many customers in the test sample would you extend credit to? 

predicted2 <- predict(performance,newdata= df_val1) 
df_val1$predNPV = predicted2
predictedNPV_lm_val=df_val1$predNPV
profit.customer.actual<- length(predicted2[predictedNPV_lm_val>-286])
profit.customer.actual

overall.profit<- sum(df_val1$NPV[predictedNPV_lm_val>-286])
overall.profit
overall.profit.actual<- sum(predictedNPV_lm_val[predictedNPV_lm_val>-286])
overall.profit.actual

average.profit<-overall.profit/204
average.profit
average.profit.actual<-overall.profit.actual/204
average.profit.actual



#11) Optional

bank = read.csv("credit3.csv")
bank$CHK_ACCT <- as.factor(bank$CHK_ACCT)
bank$SAV_ACCT <- as.factor(bank$SAV_ACCT)
bank$NUM_CREDITS <- as.factor(bank$NUM_CREDITS)
bank$HISTORY <- as.factor(bank$HISTORY)
bank$PRESENT_RESIDENT <- as.factor(bank$PRESENT_RESIDENT)
bank$EMPLOYMENT <- as.factor(bank$EMPLOYMENT)
bank$JOB <- as.factor(bank$JOB)
bank$NUM_DEPENDENTS <- as.factor(bank$NUM_DEPENDENTS)
bank$RENT <- as.factor(bank$RENT)
bank$INSTALL_RATE <- as.factor(bank$INSTALL_RATE)
bank$GUARANTOR  <- as.factor(bank$GUARANTOR )
bank$OTHER_INSTALL  <- as.factor(bank$OTHER_INSTALL)
bank$OWN_RES <- as.factor(bank$OWN_RES)
bank$TELEPHONE<- as.factor(bank$TELEPHONE)
bank$FOREIGN<- as.factor(bank$FOREIGN)
bank$REAL_ESTATE<- as.factor(bank$REAL_ESTATE)
bank$TYPE<- as.factor(bank$TYPE)
is_profitable = rep(0, nrow(bank))
bank$NPV<-as.numeric(gsub(",","",bank$NPV))
is_profitable[bank$NPV > 0]= 1
is_profitable = as.factor(is_profitable)
bank$is_profitable <- is_profitable

bank$AMOUNT_REQUESTED<-as.numeric(gsub(",","",bank$AMOUNT_REQUESTED))
bank$CREDIT_EXTENDED <- NULL
bank$OBS. <- NULL
View(bank)
set.seed(12345)
training = sample(nrow(bank), 0.7*nrow(bank))

df_train_new=bank[training,]
df_val_new= bank[-training,]

perf_glm = glm(is_profitable~. -NPV,family = binomial,data=df_train_new)

glm_model =predict(perf_glm,type="response")
glm_model
p_prof <- rep(0,nrow(df_val_new))
p_prof[glm_model>0.5] <- 1
p_prof

glm_model.2 =predict(perf_glm,df_val_new, type="response")
glm_model.2
p_prof.2 <- rep(0,nrow(df_val_new))
p_prof.2[glm_model.2>0.5] <- 1
p_prof.2
table(glm_model.2,df_val_new$is_profitable)

conf_matrix <- confusionMatrix(p_prof.2,df_val_new$is_profitable)
conf_matrix
#12)
glm_model =predict(perf_glm,df_val_new, type="response")
df_val_new = data.frame(df_val_new,glm_model)
df_val_new$sample1 = "s"
credit_new.test.profitable = df_val_new[which(df_val_new$glm_model > 0.5),]


glm_model =predict(perf_glm,df_train_new, type="response")
df_train_new <- data.frame(df_train_new,glm_model)
df_train_new$sample1 = "t"
credit_new.train.profitable = df_train_new[which(df_train_new$glm_model > 0.5),]


df_new <- rbind(credit_new.train.profitable,credit_new.test.profitable)
View(df_new)

#13)

training <- createDataPartition(df_new$sample1, p=0.7, list=FALSE)
df_train_reg<- data.frame(df_new[training,])
df_val_reg <- data.frame(df_new[-training,])


#14)
model <- lm(NPV~.-sample1-is_profitable-glm_model,data=df_train_reg)
predictnew <- predict(model)
df_train_reg$predNPV = predictnew
predictedNPV_lm=df_train_reg$predNPV
min.npv<-min(predictedNPV_lm)
max.npv<-max(predictedNPV_lm)
max.npv
sum_cut= c()
co = c()
cutoff <- seq(min.npv, max.npv, by= 1 )
for (d in cutoff) {
  NPV1=c()
  NPV1= ifelse((predictedNPV_lm > d),df_train_reg$NPV,0)
  a=sum(NPV1)
  sum_cut=append(sum_cut,a)
  co= append(co,d)
}
z = data.frame(sum_cut,co)
z$co[which.max(z$sum_cut)]


predicted2 <- predict(model,newdata= df_val_reg) 
df_val_reg$predNPV = predicted2
predictedNPV_lm_val=df_val_reg$predNPV
profit.customer.actual<- length(predicted2[predictedNPV_lm_val>-106.6819])
profit.customer.actual

overall.profit<- sum(df_val_reg$NPV[predictedNPV_lm_val>-106.6819])
overall.profit
overall.profit.actual<- sum(predictedNPV_lm_val[predictedNPV_lm_val>-106.6819])
overall.profit.actual

average.profit<-overall.profit/189
average.profit
average.profit.actual<-overall.profit.actual/189
average.profit.actual

