setwd("c:/USF MSHI/HS 631/Assignment 10")

#1. From a clean environment, install the following packages if you do not have them:
#rgl, corrplot, pwr, pscl, car
#install.packages("rgl")
#install.packages("corrplot")
#install.packages("pwr")
#install.packages("pscl")
#install.packages("car")
library("rgl")
library("corrplot")
library("pwr")
library("pscl")
library("car")
library(ggplot2)

#2. Read file "babies_age_birthsize.csv" into a dataframe named babies.
babies <- read.csv("babies_age_birthsize.csv", header = T)

#3. Run the following commands
babies_numeric <- babies[,c(1:7)]
babies_numeric$birth.size <- as.numeric(babies$birth.size )
babies_numeric$age.level <- as.numeric(babies$age.level )

#4. Find the correlations between all the variables in babies_numeric.
summary(babies_numeric)
cor(babies_numeric, use = "pairwise.complete.obs", method = "spearman")

#Q1: The correlation output shows a problem with your data. What is it?
## bwt and birthsize shouldn't be negatively correlated, which means that the 
## birthsize is not properly ordered. Age and age level should have stronger 
## correlation

#5. Examine the levels of the two categorical variables. If necessary, reorder 
#the levels
babies$birth.size <- factor(babies$birth.size, levels = c("small", "large"))
age_level <- c("teens", "early.20s", "late.20s", "early.30s", "late.30s", "forty.plus")
babies$age.level <- factor(babies$age.level, levels = age_level)

#6. After your changes to babies in step 5, rerun steps 3 and 4
babies_numeric <- babies[,c(1:7)]
babies_numeric$birth.size <- as.numeric(babies$birth.size )
babies_numeric$age.level <- as.numeric(babies$age.level )
 
#Q2: Compare your correlation results now with those in step 4. If there is a change,
#which is more believable and why?
cor(babies_numeric, use = "pairwise.complete.obs", method = "spearman")
## Now bwt and birth size are positively correlated, and age and age level are highly
## correlated

#7. Enter the following command:
fit_bsize_all <- glm(birth.size~., family=binomial(), data=babies)
#Q3: Do a quick search on the warning message, and write a sentence on what you
#think is the problem the warning relates.
## Warning messages:
## 1: glm.fit: algorithm did not converge 
## 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
## there might be too many variables in your model, leading to a perfect linear 
## separation of cases. The consequences is that model likelihood is not defined, 
## and thus you can't get the model to converge.

#8. Execute a summary of fit_bsize_all.
summary(fit_bsize_all)

#Q4: Compare the null deviance to the residual deviance.
## The null deviance shows how well the response is predicted by the model with nothing but an 
## intercept. The residual deviance shows how well the response is predicted by the model when 
## the predictors are included.
## Null deviance: 1.6181e+03  on 1167  degrees of freedom
## Residual deviance: 1.5168e-06  on 1155  degrees of freedom
## null deviance is very high, and residual deviance is very low 

#Q5: What does the displayed Std. Error tell you about this model?
## It is a measure of uncertainty about this estimate. The std. error numbers here are very big,
## which means there are a lot of uncertainties 

#Q6: Comment on the significance of the fitted coefficients, providing evidence for
#your statements
## None of them are significant
## The intercept is very small:-4.304e+03, which is -4304. It doesn't make sense if the birth.size
## is negative at all. (All the other fitted coefficients are not as strongly negative or positive) 

#9. Calculate the pseudo R2. You may use the pscl package which you installed above.
pR2(fit_bsize_all)
##          llh       llhNull            G2      McFadden          r2ML          r2CU 
##-7.583792e-07 -8.562622e+02  1.712524e+03  1.000000e+00  7.691997e-01  1.000000e+00 

#Q7: Report and comment on the McFadden and r2CU (Nagelkerke) values.
## they are both 1 

#10. Calculate the odds ratio of the fitted coefficients of your model:
exp(coef(fit_bsize_all))
##        (Intercept)                 bwt           gestation              parity                 age              height 
##0.000000e+00        2.330787e+15        8.774052e-01        1.639178e+00        1.083170e+00        1.399333e+00 
##weight               smoke  age.levelearly.20s   age.levellate.20s  age.levelearly.30s   age.l   evellate.30s 
##9.715127e-01        8.151520e-01        2.920741e+24        2.085719e+24        3.153794e+24        1.672351e+24 
##age.levelforty.plus 
##6.722182e+21 

#Q8: Report and comment on the OR. Report any OR that is strange or questionable.
## when bwt is 0, odds of baby being large is close to zero
## bwt: for every ounce increase in bwt, or of being large is on average multiplied by huge number
## age level ORs are also huge, for a mom in early 20s the OR of baby being large 

#11. Calculate the Variance Inflation Factor (VIF).
vif(fit_bsize_all)

#Q9: Report any multicollinearity that is indicated by VIF
##age and age level

#12. Fit a new model with all the predictor variables EXCEPT you should remove one
#the multicollinear variables. Execute a summary and pseudo R2.
fit_bsize_all2 <- 
  glm(birth.size ~ bwt + age + gestation + parity + height + weight + smoke, family=binomial(), data=babies)
vif(fit_bsize_all2)
summary(fit_bsize_all2)
pR2(fit_bsize_all2)

## not entirely

#13.
## bwt shouldn't be a variable for birth.size

#14. Fit a new model with all the predictor variables from your previous model in #12
#EXCEPT you should remove the predictor variable that is intertwined with the
#response variable.
fit_bsize_all3 <- 
  glm(birth.size ~  gestation + parity + height + weight + smoke + age , family=binomial(), data=babies)
vif(fit_bsize_all3)
summary(fit_bsize_all3)
pR2(fit_bsize_all3)

## bwt is the cause of the linear separation 

#15. Create a histogram of the variable you removed, colored by birth size. Briefly
#relate what this plot reveals.
ggplot(data = babies, aes(x = bwt, color = birth.size)) +
  geom_histogram(fill="white")

## bwt and birth.size are intertwined- birth.size is determined based on bwt 

#16. Execute the following command, then state which confidence intervals include an
#odds ratio of 1 (OR=1). What does this indicate and how is it related to the p-value
#of those same variables?
exp(cbind(OR=coef(fit_bsize_all3), confint(fit_bsize_all3)))

## gestation, height and weight
## p-values are extrememly close to 1 as well

#17. Perform the steps to find a final interpret retable model
fit_bsize_all4 <- 
  glm(birth.size ~  gestation + parity + height + weight + smoke, family=binomial(), data=babies)
summary(fit_bsize_all4)

fit_bsize_all5 <- 
  glm(birth.size ~  gestation + parity + height + smoke, family=binomial(), data=babies)
summary(fit_bsize_all5)
pR2(fit_bsize_all5)

## for log odds 1.04 gestation multiplier will be a 4% increase

#18. Calculate the odds ratios of the fitted coefficients of your final model
exp(coef(fit_bsize_all5))

## ln(OR) = mx + b 
## for one day increase of gestation, the odds of getting a larger baby is multiplied by 1.04 
