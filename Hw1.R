setwd("C:/Users/sarth/Desktop/Data mining/data files")
college= read.csv("college.csv")
college()
fix("college")
rownames (college )=college [,1]
fix(college)
college= college[,-1]
fix("college")
summary(college)
pairs(college[,1:10])
boxplot(college$Outstate~college$Private, col= c("blue", "red"), main = " Outstate vs Private")
Elite=rep("No",nrow(college ))
Elite[college$Top10perc >50]=" Yes"
Elite=as.factor(Elite)
college=data.frame(college , Elite)
boxplot(college$Outstate~college$Elite, col= c("blue", "red"), main = " Outstate vs Elite")
par(mfrow=c(2,2))
n1=20
n2=10
n3=10
n4=50
h= hist(college$Grad.Rate,n1, col = c("red"), main = "Grad Rate Variation")
h= hist(college$Accept,n4, col = c("Blue"), main = "Acceptation Variation")
h= hist(college$perc.alumni,n3, col = c("Green"), main = "Percentage of Alumni")
h= hist(college$PhD,n2, col = c("Yellow"), main = "Phd Students")
par(mfrow=c(2,2))
scatter.smooth(college$Apps,college$Enroll, main = "Variation of Enrollment Vs Application", col= c("blue"))
scatter.smooth(college$Enroll,college$Books, main = "Variation of Enrollment Vs Spending in Books", col= c("red"))
scatter.smooth(college$F.Undergrad,college$S.F.Ratio, main = " F.Students Vs S.F.Ratio", col= c("green"))
scatter.smooth(college$P.Undergrad,college$Expend, main = "P.students Vs Expend per student", col= c("yellow"))
fit = lm(college$Grad.Rate~college$Apps+college$Top25perc + college$Private+ college$Apps+ college$Accept+ college$Top10perc+college$F.Undergrad+ college$Books+college$Personal+college$PhD+ college$Terminal+ college$P.Undergrad + college$Outstate + college$Room.Board+college$perc.alumni+college$Expend,data = college)
summary(fit)
fit = lm(college$Grad.Rate~college$Apps+college$Top25perc + college$P.Undergrad + college$Outstate + college$Room.Board+college$perc.alumni+college$Expend,data = college)
summary(fit) 
fit = lm(log(college$Grad.Rate)~college$Apps+college$Top25perc + college$P.Undergrad + college$Outstate + college$Room.Board+college$perc.alumni+college$Expend,data = college)
summary(fit) 
