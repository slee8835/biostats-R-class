#1. Read the file weather_data.csv into a dataframe named temper.
setwd("c:/USF MSHI/HS 631/Assignment 6")
temper <- read.csv(file="weather.data.csv", header=T) 
temper

#2. Perform EDA: Examine the environment and execute a summary of the dataset.
summary(temper)

#3. Perform EDA: Create plots of the distribution of each variable.
hist(temper$upper, main = "Upper Temperature Distribution", xlab = "Temperatures in Celsius", ylab = "counts")
hist(temper$lower, main = "Lower Temperature Distribution", xlab = "Temperatures in Celsius", ylab = "counts")
hist(temper$rain, breaks = 30, main = "Rain Fall Distribution", xlab = "Rain fall in mm", ylab = "counts")
hist(temper$month, main = "Months Distribution", xlab = "Month", ylab = "counts")
hist(temper$yr, main = "Years Distribution", xlab = "Year", ylab = "counts")

#4. Perform EDA: Execute the command plot(temper) to plot the pairwise relations of
#the variables in temper. 
plot(temper)

#5. In order to produce a linear model of upper temperature as a function of month,
#some data preparation will be required. Consider whether the data types of the
#variables appropriately represent temperature, rainfall, month and year. Write a
#brief comment identifying any variable that is typed incorrectly. Do not modify
#dataset temper.
summary(temper$month)
summary(temper$yr)
##month and year should be categorical 

#6. Use the lm command to produce a linear regression model of upper temperature as a
#function of month for temper. Store your model in a variable named fit0.
fit0 <- lm(upper ~ month, data = temper )

#7. Execute a summary of fit0 and briefly describe its goodness of fit. Be sure to
#include what quantity in the summary leads to your statement.
summary(fit0)

#the fitted coefficient is 0.32559 with the R-square value of 0.03057. The linear 
#regression model is not a good fit. It is caused by the fact that months are labeled 
#as int variables here. Instead, they should be categorical

#8. Execute plot(fit0,1). Note that a parabolic shape of the plot Residuals vs Fitted
#often, but not always, is indicative of 2nd order effects.
plot(fit0,1)

#9. Write out the equation of the fitted line for model fit0.
##y = ax +b 
##upper = 0.32559 * month + 12.82991

#10. Write out what the regression line of fit0 models: what is the effect on response
#variable upper of a unit increase in predictor variable month?
##one unit increase of month, will cause an 0.32559 unit increase of upper 

#11. Create a scatter plot of upper temperature as a function of month in dataset
#temper. Color the points by month.
color <- c("sky blue","pink", "green", "gray", "white", "yellow", "brown", "black", "red", "purple", "orange", "gold")
plot(upper ~ month, main = "Upper Temperature and Month Scatter Plot", xlab = "month", ylab = "Upper temperature in Celcius",
     data = temper, col=color)
plot(data = temper, upper~month, col = month)
#12. On your plot add an abline of the fitted regression line whose equation you wrote
#above. You may use the command abline(lm(fit0)) to accomplish this.
abline((lm(fit0)))

#13. Create a new dataset temper2 that is a copy of temper. Then modify temper2 so
#as to correct the data types of variable(s) that are of inappropriate type. 
temper2 <- data.frame(temper)
temper2
num_month <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
temper2$month <- factor(temper$month, levels = num_month, ordered = FALSE)
temper2$month

#14. Create a plot to visualize upper temperature by month in dataset temper2. Use a
#plot type that is appropriate for the types of these 2 variables in temper2. Briefly
#compare this plot to the one you created above to visualize upper temperature as a
#function of month in dataset temper2.
plot(upper ~ month, main = "Upper Temperature for Each Month", xlab = "month", ylab = "Upper temperature in Celcius", 
     data = temper2)

##instead of getting a scatter plot because both month and upper were numeric/int variables, now we have a box plot
##which is a lot easier to understand.

#15. Use the lm command to produce a linear regression model of upper temperature as a
#function of month for temper2. Store your model in a variable named fit1.
fit1 <- lm(upper ~ month, data = temper2)

#16. Execute plot(fit1,1). Compare this plot to the Residuals vs Fitted plot of fit0.
plot(fit1,1)

##the residuals vs fitted line is straight compared to fit0 was a parabolic line. The residuals are scattered evenly 
##above and under the line.

#17. Execute a summary of fit1 and briefly describe its goodness of fit in comparison
#with fit0.
summary(fit1)

##the goodness of fit is better than fit0: the adjusted R-squared value has improved from
##0.32559 to 0.7084

#18. Write out three equations of the fitted line for model fit1: one for the month of
#January, one for June, and one for December.
## Upper = January + 7.9301
## Upper = 12.3 *June + 7.9301
## Upper = 0.3698 * December + 7.9301