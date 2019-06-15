sale1
install.packages("rpart.plot")
install.packages("rattle")
# Regression Tree Example
library(rpart)
library(rattle)
library(rpart.plot)

# grow tree 
fit <- rpart(Weekly_Sales~., method="anova", data=sale1)

printcp(fit) # display the results  cp is complexity parameter
summary(fit) # detailed summary of splits 

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results  .
Produces 2 plots.The first plots the r-square (apparent and apparent - from cross-validation)
versus the number of splits. The second plots the Relative Error(cross-validation) +/- 1-SE from cross-validation versus the number of splits. 
fit

# plot tree 
fancyRpartPlot(fit,uniform=TRUE,main="Regression Tree for Weekly_sales")