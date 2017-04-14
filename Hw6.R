setwd("C:/Users/sarth/Desktop/Data mining/data files")

#1.	Create dummy variables for all categorical variables that are not ordered (CHK_ACCT, SAV_ACCT, HISTORY, JOB and TYPE). 

credit= read.csv("credit3.csv")
credit <- credit[,-1]
credit$NPV<-as.numeric(gsub(",","",credit$NPV))
credit$CHK_ACCT <- as.factor(credit$CHK_ACCT)
credit$SAV_ACCT <- as.factor(credit$SAV_ACCT)
credit$HISTORY <- as.factor(credit$HISTORY)
credit$JOB <- as.factor(credit$JOB)
credit$TYPE<- as.factor(credit$TYPE)
credit$AMOUNT_REQUESTED<-as.numeric(gsub(",","",credit$AMOUNT_REQUESTED))
credit$CREDIT_EXTENDED <- gsub(",","",credit$CREDIT_EXTENDED)
credit$profitable = ifelse(credit$NPV>0, 1, 0)

#2.	Perform a K-Means clustering of the data with K = 5 using all of the data except NPV and the created PROFITABLE dummy variable. Include the table of cluster centers as Exhibit 1. Use 20 random starts to ensure that you have good clusters.

credit_scaled <- scale(credit[,c(-2,-3,-6,-9,-19, -21, -22,-23)])

credit_scaled <- data.frame(credit_scaled,credit$CHK_ACCT)
credit_scaled <- data.frame(credit_scaled,credit$SAV_ACCT)
credit_scaled <- data.frame(credit_scaled,credit$TYPE)
credit_scaled <- data.frame(credit_scaled,credit$JOB)
credit_scaled <- data.frame(credit_scaled,credit$HISTORY)

set.seed(12345) 
k_model = kmeans(credit_scaled,5,nstart=20) 
summary(k_model)
dist(k_model$centers)

Profitable <- ifelse(credit$NPV > 0, 1, 0) 
credit_scaled$Cluster <- k_model$cluster
table(Profitable, credit_scaled$Cluster)

#4.	The output allows you to identify, for each individual, the cluster they belong to. Combine this with the NPV column from the original data (making sure that the Row Id matches). 

#a.	Create a bar chart showing the percentage of people in each cluster.
#b.	Create a table showing average of NPV split up by Cluster Id. 
#c.	Attach these as Exhibit 2.

hist(credit_scaled$Cluster,col='light blue')

aggregate(credit$NPV, by=list(credit_scaled$Cluster), FUN=mean, simplify=TRUE)
#6.	Experiment with two other values of K (4 and 6) and decide which set of clusters are most meaningful to you. 

set.seed(12345) 
k_model = kmeans(credit_scaled,4,nstart=20) 
credit_scaled$Cluster <- k_model$cluster
table(Profitable, credit_scaled$Cluster)

aggregate(credit$NPV, by=list(credit_scaled$Cluster), FUN=mean, simplify=TRUE)

set.seed(12345) 
k_model = kmeans(credit_scaled,6,nstart=20) 
credit_scaled$Cluster <- k_model$cluster
table(Profitable, credit_scaled$Cluster)

aggregate(credit$NPV, by=list(credit_scaled$Cluster), FUN=mean, simplify=TRUE)

#7.	Conduct an association rule analysis of the data to identify attributes that are related
#    to profitability. Attach any supporting output as Exhibit 3. [One approach would be to include 
#    the PROFITABLE dummy variable and focus on rules for which this variable is the consequent.]

install.packages('arules')
install.packages('arulesViz')


library(arules)
library(arulesViz)

credit_ar <- credit[,c(2,3,6,8,9,15)]
credit_ar<- data.frame(credit_ar,Profitable)

credit_ar$EMPLOYMENT <- as.factor(credit_ar$EMPLOYMENT)
credit_ar$OWN_RES <- as.factor(credit_ar$OWN_RES)
credit_ar$Profitable <- as.factor(credit_ar$Profitable)

rules <- apriori(credit_ar, parameter = list(supp = 0.05, conf = 0.8))
summary(rules)


rules<-sort(rules, by="confidence", decreasing=TRUE)


inspect(rules[1])


#8.	List five good rules and describe their properties (in terms of criteria discussed in class). 
#   Describe the rules in wordsinspect(rules[1:5])

inspect(rules[1:5])

#9.	Suppose you were to use the best rule identified above as a guide whether to extend loans or not for the 
#   cases in your data set. What would the total profits be?

# Total Profit : 

sum(ifelse(((CHK_ACCT == 3) & (EMPLOYMENT == 4) & (JOB == 2) & (OWN_RES == 1)),NPV,0))
#10.Suppose you were to only use cluster membership (and PROFITABLE) to define association rules. 
#   Does this lead to good rules? How does the profitability of these rules compare with the profitability 
#   of rules that use the more detailed information?
#

set.seed(12345) 
k_model = kmeans(credit_scaled,4,nstart=20) 
credit_scaled$Cluster <- k_model$cluster
table(Profitable, credit_scaled$Cluster)

aggregate(credit$NPV, by=list(credit_scaled$Cluster), FUN=mean, simplify=TRUE)

credit <- data.frame(credit,Profitable)
Cluster2 <- credit[credit_scaled$Cluster == 2,]

Cluster2 <- Cluster2[,c(2,3,6,8,9,15,23)]


Cluster2$EMPLOYMENT <- as.factor(Cluster2$EMPLOYMENT)
Cluster2$OWN_RES <- as.factor(Cluster2$OWN_RES)
Cluster2$profitable <- as.factor(Cluster2$profitable)


rules<-apriori(data=Cluster2, parameter=list(supp=0.05,conf = 0.08), appearance = list(default="lhs",rhs="profitable=1"), 
               control = list(verbose=F))
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1])


sum(ifelse((CHK_ACCT == 3),NPV,0))
  