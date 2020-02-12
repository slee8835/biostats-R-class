setwd("c:/USF MSHI/HS 631/Assignment 5")

baby_IQ <- read.csv(file="baby_IQ_data200.csv", header=T)  
baby_IQ

#perform EDA. Be sure to calculate measures of frequency,
#centrality and spread for all variables where appropriate. 
summary(baby_IQ)
cor(baby_IQ, use = "pairwise.complete.obs")

#Produce plots to visualize the distribution of each variable. 
hist(baby_IQ$MA, breaks = 20, main = "Mother's Age Distribution", xlab = "Mother's age in years", ylab = "counts")
hist(baby_IQ$PSS, breaks = 20, main = "Family Social Status Distribution", xlab = "Social Status", ylab = "counts")
hist(baby_IQ$BE, main = "Breadwinners Post High School Education Distribution", 
     xlab = "Breadwinners post high school education", ylab = "counts")
hist(baby_IQ$CC, main = "Cigarette Consumption of Mothers Distribution", xlab = "Cigarettes per day", 
     ylab = "counts")
hist(baby_IQ$NP, main = "No. of Pregnancies Distribution", xlab = "Numbers of pregnancies", ylab = "counts")
hist(baby_IQ$GA, main = "Gestational Age of Baby at Birth Distribution", xlab = "Gestational age of baby in weeks", 
     ylab = "counts")
hist(baby_IQ$BW, main = "Baby's Birth Weight Distribution", xlab = "Birth weight in g", ylab = "counts")
hist(baby_IQ$BL, main = "Baby's Birth Height Distribution", xlab = "Birth height in cm", ylab = "counts")
hist(baby_IQ$IQ, breaks = 20, main = "Baby's IQ Score Distribution", xlab = "IQ score", ylab = "counts")

#Produce 4 plots to visualize relationships between variables
#of your choosing.
plot(data = baby_IQ, BL ~ MA, main = "Correlation between Birth Height of Baby and Mother's Age",
     xlab = "Mother's age in years", ylab = "Birth height in cm", col = "red")
plot(data = baby_IQ, IQ ~ MA, main = "Correlation between Baby's IQ and Mother's Age",
     xlab = "Mother's age in years", ylab = "Baby's IQ Score", col = "blue")
plot(data = baby_IQ, IQ ~ CC, main = "Correlation between Baby's IQ and Cigarette Consumption of Mom per Day",
     xlab = "Cigarette consumption per day", ylab = "Baby's IQ Score", col = "grey")
plot(data = baby_IQ, PSS ~ MA, main = "Correlation between Social Status of Family and Mother's Age",
     xlab = "Mother's age in years", ylab = "Social social of family", col = "plum")

#Produce a main effects linear regression model using the lm function to model baby IQ as
#a function of the other variables. In order to produce a final main effects model, begin with
#all main effects, and remove the insignificant variables one by one, the most insignificant
#first, until you arrive at a model where all variables are significant.
IQ_all <- lm(data = baby_IQ, IQ ~ MA + PSS + BE + CC + NP + GA + BW + BL)
summary(IQ_all)

IQ_fit1 <- lm(data = baby_IQ, IQ ~ MA + PSS + BE + CC)
summary(IQ_fit1)
plot(IQ_fit1, 1)

#When you have a final main effects model, inspect the Residuals vs Fitted plot by
#typing: plot(<your model>, 1)
IQ_fit2 <- lm(data = baby_IQ, IQ ~ MA + BE + CC)
summary(IQ_fit2)
plot(IQ_fit2, 1)

#if your final main effects model contains: var1, var2, var3 your formula would be:
#IQ ~ (var1+var2+var3)^2 + I(var1^2)+I(var2^2)+I(var3^2)
IQ_fit3 <- lm(data = baby_IQ, IQ ~ (MA + BE + CC)^2 + I(MA^2)+I(BE^2)+I(CC^2))
summary(IQ_fit3)
plot(IQ_fit3, 1)
plot(IQ_fit3, 5)





