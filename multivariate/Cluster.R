library(fpc)
library(cluster)

drop=c("IsHoliday")
sale2=sale1[ ,!(names(sale1) %in% drop)]

#data normalization
sale_norm = scale(sale2)

#removing na presences
sum(is.na(sale2)==TRUE)

set.seed(500)
#to find out how many clusters
wss <- (nrow(sale_norm)-1)*sum(apply(sale_norm,2,var))
for (i in 1:5) wss[i] <- sum(kmeans(sale_norm, centers=i)$withinss)#i no of clusters.Find within clusters sum of squares
plot(1:5, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#forming clusters
results= kmeans(sale_norm,5)
results
clusplot(sale_norm[,1:10], results$cluster, color=TRUE, shade=TRUE,labels=2, lines=0) #pca 

results$cluster
a=data.frame(sale2,results$cluster)
View(a)

data1 = split(a,a$results.cluster) #Splits data based on results$cluster

#creating data frames from the clusters 
firstcluster = data1$`1`
secondcluster=data1$`2`
thirdcluster=data1$`3`
fourthcluster=data1$`4`
fivecluster=data1$`5`

library(psych)
desc1 = data.frame(round(describe(firstcluster),2))
desc2 = data.frame(round(describe(secondcluster),2))
desc3 = data.frame(round(describe(thirdcluster),2))
desc4 = data.frame(round(describe(fourthcluster),2))
desc5 = data.frame(round(describe(fivecluster),2))

#Print the above created table in pdf format
library(gridExtra)
pdf("Descriptive_1stcluster.pdf",height = 8,width = 10)
grid.table(desc1[-11,-c(1,6,7,11)])
dev.off()

pdf("Descriptive_2ndcluster.pdf",height = 8,width = 10)
grid.table(desc2[-11,-c(1,6,7,11)])
dev.off()

pdf("Descriptive_3rdcluster.pdf",height = 8,width = 10)
grid.table(desc3[-11,-c(1,6,7,11)])
dev.off()

pdf("Descriptive_4thcluster.pdf",height = 8,width = 10)
grid.table(desc4[-11,-c(1,6,7,11)])
dev.off()

pdf("Descriptive_5thcluster.pdf",height = 8,width = 10)
grid.table(desc5[-11,-c(1,6,7,11)])
dev.off()



##############################################################################

library(randomForest)
library(e1071)
library(MASS)
############################### First Cluster ################################

#stepwise Regression
fit1 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment, data=firstcluster)
step_firstcluster <- stepAIC(fit1, direction="both")
summary(step_firstcluster)

#Random forest
ran_fit1 = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + 
                          MarkDown4 + MarkDown5 + CPI + Unemployment, data=firstcluster, ntree=500, importance=T)

plot(ran_fit1)
# Variable Importance Plot
varImpPlot(ran_fit1,
           sort = T,
           main="Variable Importance",
           n.var=5)

#Svm
svm1 = svm(Weekly_Sales ~ CPI + Unemployment + MarkDown4 + MarkDown1, data = firstcluster)
summary(svm1)
predicted1 <- predict(svm1, firstcluster)

#variance explained with SVM
100-mean(abs((firstcluster$Weekly_Sales-predicted1)/firstcluster$Weekly_Sales)*100)


############################### Second Cluster ################################

#stepwise Regression
fit2 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment, data=secondcluster)
step_secondcluster <- stepAIC(fit2, direction="both")
summary(step_secondcluster)


#Random forest
ran_fit2 = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + 
                          MarkDown4 + MarkDown5 + CPI + Unemployment, data=secondcluster, ntree=500, importance=T)

plot(ran_fit2)

# Variable Importance Plot
varImpPlot(ran_fit2,
           sort = T,
           main="Variable Importance",
           n.var=5)

#Svm
svm2 = svm(Weekly_Sales ~ CPI + Unemployment + MarkDown4 + MarkDown5, data = secondcluster)
summary(svm2)
predicted2 <- predict(svm2, secondcluster)

#variance explained with SVM
100-mean(abs((secondcluster$Weekly_Sales-predicted2)/secondcluster$Weekly_Sales)*100)


############################### Third Cluster ################################

#stepwise Regression
fit3 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment, data=thirdcluster)
step_thirdcluster <- stepAIC(fit3, direction="both")
summary(step_thirdcluster)


#Random forest
ran_fit3 = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + 
                          MarkDown4 + MarkDown5 + CPI + Unemployment, data=thirdcluster, ntree=500, importance=T)

plot(ran_fit3)
# Variable Importance Plot
varImpPlot(ran_fit3,
           sort = T,
           main="Variable Importance",
           n.var=5)

#Svm
svm3 = svm(Weekly_Sales ~ CPI + MarkDown1 + MarkDown5 + Fuel_Price, data = thirdcluster)
summary(svm3)
predicted3 <- predict(svm3, thirdcluster)

#variance explained with SVM
100-mean(abs((thirdcluster$Weekly_Sales-predicted3)/thirdcluster$Weekly_Sales)*100)


############################### Forth Cluster ################################

#stepwise Regression
fit4 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment, data=fourthcluster)
step_fourthcluster <- stepAIC(fit4, direction="both")
summary(step_fourthcluster)


#Random forest
ran_fit4 = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + 
                          MarkDown4 + MarkDown5 + CPI + Unemployment, data=fourthcluster, ntree=500, importance=T)

plot(ran_fit4)
# Variable Importance Plot
varImpPlot(ran_fit4,
           sort = T,
           main="Variable Importance",
           n.var=5)

#Svm
svm4 = svm(Weekly_Sales ~ Temperature + MarkDown2 + MarkDown3 + Fuel_Price, data = fourthcluster)
summary(svm4)
predicted4 <- predict(svm4, fourthcluster)

#variance explained with SVM
100-mean(abs((fourthcluster$Weekly_Sales-predicted4)/fourthcluster$Weekly_Sales)*100)


############################### Fifth Cluster ################################

#stepwise Regression
fit5 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment, data=fivecluster)
step_fivecluster <- stepAIC(fit5, direction="both")
summary(step_fivecluster)


#Random forest
ran_fit5 = randomForest(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + 
                          MarkDown4 + MarkDown5 + CPI + Unemployment, data=fivecluster, ntree=500, importance=T)

plot(ran_fit5)
# Variable Importance Plot
varImpPlot(ran_fit5,
           sort = T,
           main="Variable Importance",
           n.var=5)

#Svm
svm5 = svm(Weekly_Sales ~ MarkDown1 + MarkDown3 + MarkDown5 + CPI, data = fivecluster)
summary(svm5)
predicted5 <- predict(svm5, fivecluster)

#variance explained with SVM
100-mean(abs((fivecluster$Weekly_Sales-predicted5)/fivecluster$Weekly_Sales)*100)

