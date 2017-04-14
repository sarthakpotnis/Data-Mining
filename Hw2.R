Question1

setwd("C:/Users/sarth/Desktop/Data mining/data files")
credit= read.csv("credit3.csv")
fix("college")
rownames (college )=college [,1]
fix(college)
college= college[,-1]
fix("college")
set.seed(123457)
df<-college
train<- sample(nrow(college), 466)
df_train <- df[train,] 
df_validation <- df[-train,] 
summary(df_train$Grad.Rate)
sd(df_train$Grad.Rate)
summary(df_validation$Grad.Rate)
sd(df_validation$Grad.Rate)
fit = lm(Grad.Rate~Apps+Top25perc +Private+Accept+ Top10perc+F.Undergrad+ Books+Personal+PhD+ Terminal+ P.Undergrad + Outstate +S.F.Ratio+ Enroll+ Room.Board+perc.alumni+Expend,data = df_train)
prediction<- predict(fit, newdata = df_validation)
actual<- df_validation$Grad.Rate
Metrics<- c("AE","RMSE")
x1<- mean(actual - prediction)
x2<- sqrt(mean((actual - prediction)^2))
values<-c(x1,x2)
x<- data.frame(Metrics, values)
x
fit1 = lm(Grad.Rate~Apps+Top25perc +Private+Personal+ Terminal+ P.Undergrad + Outstate + Room.Board+Expend,data = df_train)
prediction1<- predict(fit1, newdata = df_validation)
actual1<- df_validation$Grad.Rate
Metrics1<- c("AE1","RMSE1")
x1<- mean(actual1 - prediction1)
x2<- sqrt(mean((actual1 - prediction1)^2))
values1<-c(x1,x2)
x1<- data.frame(Metrics1, values1)
x1 

Question2


beer = read.csv("Beer+Preferences.csv")
str(beer)
beer$Income= as.numeric(sub('$','',as.character(beer$Income),fixed = TRUE))
str(beer)
pref=rep(0,nrow(beer))
pref[beer$Preference== 'Light']=1
beer$pref=pref

summary(prefer)
coefficients(prefer)
predict_preference = 0.396696 - (0.047006 * beer$Gender) + (0.022751 * beer$Married) + (0.028564 * beer$Income) - (0.023256 * beer$Age)
plot(predict_preference,beer$pref, col = "violetred", pch=16)
abline(v=0.5, col = "blue", lwd = 2)
n11 = length((predict_preference)[predict_preference>0.5 & beer$pref==1])
n12 = length((predict_preference)[predict_preference<0.5 & beer$pref==1])
n21 = length((predict_preference)[predict_preference>0.5 & beer$pref==0])
n22 = length((predict_preference)[predict_preference<0.5 & beer$pref==0])
TotalErrorRate=(n12+n21)/(n11+n12+n21+n22)
TotalErrorRate
ErrorRate_LightBeer= n12/(n12+n11)
ErrorRate_LightBeer
ErrorRate_RegularBeer= n21/(n21+n22)
ErrorRate_RegularBeer
beer$predict <- predict_preference
beer = beer[order(beer$predict,decreasing = TRUE),]
beer$rank <- rank(-beer$predict)
beer$cumsumactual <- cumsum(beer$pref)
plot(beer$rank,beer$cumsumactual, col = "red")
lines(beer$rank,beer$cumsumactual)
lines(0:1,0:1)
lines(c(0,100),c(0,50))

Question3
prefer_logistic = glm(beer$pref~beer$Gender+beer$Married+beer$Income+beer$Age,data = beer, family = "binomial")
summary(prefer_logistic)
coefficients(prefer_logistic)
predict_preference_logistic = 1/(1+(exp(-(-0.6234069 + (0.2762640* beer$Income) - (0.8203141*beer$Gender)+(0.1819966* beer$Married) - (0.2271951 * beer$Age)))))
plot(predict_preference_logistic,beer$pref, col = "violetred", pch=16)
abline(v=0.5, col = "blue", lwd = 2)
nlog11 = length((predict_preference_logistic)[predict_preference_logistic>0.5 & beer$pref==1])
nlog12 = length((predict_preference_logistic)[predict_preference_logistic<0.5 & beer$pref==1])
nlog21 = length((predict_preference_logistic)[predict_preference_logistic>0.5 & beer$pref==0])
nlog22 = length((predict_preference_logistic)[predict_preference_logistic<0.5 & beer$pref==0])
TotalErrorRate_log=(nlog12+nlog21)/(nlog11+nlog12+nlog21+nlog22)
TotalErrorRate_log
ErrorRate_LightBeer_log= nlog12/(nlog12+nlog11)
ErrorRate_LightBeer_log
ErrorRate_RegularBeer_log= nlog21/(nlog21+nlog22)
ErrorRate_RegularBeer_log
beer$predict_log <- predict_preference_logistic
beer = beer[order(beer$predict_log,decreasing = TRUE),]
beer$rank1 <- rank(-beer$predict_log)
beer$cumsumactual1 <- cumsum(beer$pref)
plot(beer$rank1,beer$cumsumactual1, col = "blue")
lines(beer$rank1,beer$cumsumactual1)
lines(0:1,0:1)
lines(c(0,100),c(0,50))
