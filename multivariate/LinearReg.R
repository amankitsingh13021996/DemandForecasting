.libPaths( c( "C:\Users\ankit singh\Documents\R\win-library\3.4" , .libPaths() ) )
 sales = read.csv("F://r_git_upload//multivariate//data//Data.csv",header = T)
View(sales)
str(sales)
drop=c("Date")
sale1=sales[ ,!(names(sales) %in% drop)]
str(sale1)
sale1$MarkDown2 = ifelse(is.na(sale1$MarkDown2), mean(sale1$MarkDown2, na.rm=TRUE), sale1$MarkDown2)
cor(sale1)

install.packages("corrplot")
library("corrplot")
corrplot(cor(sale1))

#Performing regression on the data
attach(sale1)
fit0 <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1+ MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + CPI + Unemployment + as.factor(IsHoliday), data=sale1)
summary(fit0)


#VIF calculation
library("car")
vif(fit0)

#reaching a proper fit by removing markdown3 because of high pr value 
fit <- lm(Weekly_Sales ~ Temperature + Fuel_Price + MarkDown1 + MarkDown2 + MarkDown4 + MarkDown5 + CPI + Unemployment + as.factor(IsHoliday), data=sale1)
summary(fit)
vif(fit)

#reaching a proper fit by removing markdown1, Fuel_Price and IsHoliday because of high pr value as well
fit <- lm(Weekly_Sales ~ Temperature + MarkDown2 + MarkDown4 + MarkDown5 + CPI + Unemployment , data=sale1)
summary(fit)
vif(fit)

#removing Temperature of high pr value
fit <- lm(Weekly_Sales ~ MarkDown2 + MarkDown4 + MarkDown5 + CPI + Unemployment , data=sale1)
summary(fit)
vif(fit)

# we can see atleast right now the adjusted R-square value is positive but none of the variables
# are significant. SO we are trying for a automatic regression, which is stepwise regression
# function.


library(MASS)
step_fit = stepAIC(fit0, direction = "both")
summary(step_fit)

# So we can see that there are nothing significant variable to predict our dependent variable.