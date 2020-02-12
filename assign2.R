setwd("c:/USF MSHI/HS 631/Assignment 2")
# *******************************

############################################################
# Execute the commands, read the descriptions, and answer
#   the 6 questions. (Many questions have multiple parts)  
############################################################

# Read in the data file used in babies_quiz.csv
# See description of varables at the bottom of this file
babies <- read.csv(file="babies_quiz.csv", header=T)  

hist(babies$age) # attempt to create histogram of numeric variable

# Question 1: What is the problem with age?  Be sure to read error message

## the 'x' must be numeric- 'age' needs to be numeric, but it's factors here

# Check all variables in the environment: 
#  variable age is the wrong type (ie:factor) and 
#  smoke has an non-standard level due to use of "?" for NA.

# Read in the data file such that "?" is taken to be NA
babies <- read.csv(file="babies_quiz.csv", header=T, na.strings = "?")  

# Exploratory data analysis (EDA) includes visualization. 
# The EDA below reveals some problems with data in babies

############################################################
# Summary statistics
############################################################
summary(babies)

# Question 2: choose one variable with an unbelievable summary. 
# *Briefly* describe 
#  a) why it is unbelievable, and
#  b) what you believe this indicates

## the smoke section. It is not believable because smoke is either
## smoking or non-smoking. Knowing the mean does not help understand
## the dataset in any way.

# A quantile of a variable is the value below which a given % of the data lies.
# Therefore the 50% quantile is the same as the median.
quantile(babies$bwt)# default quantiles
#Alt: can specify which probabilities you want quantiles for 
quantile(babies$bwt, probs = c(0, .25, .5, .75, 1) )

# Question 3: NB: variable bwt is baby's birthweight in ounces
# NB: For any dataset the median is the 50% quantile
#  a) For any dataset, at what percent quantile is the min?
#  b) For any dataset, at what percent quantile is the max   
#  c) For the variable birthweight in babies, what birthweight is at the 75% quantile?

## a) 0%
## b) 100%
## c) 131

# Range of a variable is difference between the highest and lowest values
# However, the range function in R returns the *min and max* values
#   that the variable ranges between as a vector.
range(babies$bwt)# 

# Question 4: 
#  a) What values (min, max) does the birthweight of babies in this dataset range between?
# NB: the range of the birthweight indicated by a) is 121 ounces

## min is 55 and max is 176

# Interquartile Range (IQR) is range from upper quartile (75%) to lower quartile (25%)
# This contains 50% of the observations: those in the mid-range of the sample
IQR(babies$bwt) # for our sample, IQR is from 108.75 to 131 ounces

# Question 5: 
#  a) what is the interquartile range of the birthweight variable?

## the interquartile range is 22.25 

# Correlation is a value that describes the strength and 
#   direction of the relationship between 2 variables
# Correlation ranges between -1 (anti-correlated)
#   zero (not correlated) and 1 (perfectly correlated)

# Take slice of the babies dataset. Include all columns 
#  EXCEPT the first column (named X) and last two columns 
babies[2:7]
cor(babies[c(2:7)], use = "pairwise.complete.obs")

# Question 6: 
#  a) provide the names of two different variables that are positively correlated

## height and weight

#  b) provide the names of two different variables that are negatively correlated

## parity and age 

#  c) provide the names of the two different variables that are the least correlated
#        (ie: their correlation is closest to zero of this dataset)

## age ad gestation 

############################################################
# Code to run and understand (no questions).
# For next quiz, know what type of plot to use 
#  to present data of different types
############################################################

# Visualization of a single variable with base R graphics

# From R Graphics Essentials
# http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/
# "The type of plot depends on the type of the variable
#   For categorical variables (or grouping variables). You can visualize the count of categories using a bar plot or using a pie chart to show the proportion of each category.
#   For continuous variable, you can visualize the distribution of the variable using density plots, histograms and alternatives"

# hist for distribution of single numeric variable
#  (We will cover density plots when we cover ggplot2)
?hist #see documentation of hist 

# Default hist
hist(babies$weight)

# Examine the plot object, just this once to see the breaks and counts
# NOT on quizzes, etc
pl1 <- hist(babies$weight) # pl1 is the plot object
pl1 # there is one more value in breaks vector than in counts vector
pl2 <- hist(babies$weight, breaks=10)
pl2  # same breaks and counts in this case
pl3 <- hist(babies$weight, breaks = 20)

# Step-by-step improvement of Default hist::
# set number of breaks and title, all others (ex: xlab) are default
hist(babies$weight, breaks = 100, main="Distribution of Weight of Mother")

hist(babies$weight, breaks = 100, main="Distribution of Weight of Mother",
     xlab="Weight (lbs)", ylab="Count")

hist(babies$weight, breaks = 100, main="Distribution of Weight of Mother",
     xlab="Weight (lbs)", ylab="Count",
     border="cyan", col="dark gray")

#Default plot for our factor variable
plot(babies$age.level) 

# use of barplot for factor variable  
# Create barplots with the barplot(height) function
table(babies$age.level)# provides the heights
counts <- table(babies$age.level)
counts
barplot(counts, main="Age Level of Mother",
        xlab="Age Level", ylab="Count",
        border="cyan", col="dark gray")


# For visualization that are similar to above, using ggplot2
# ggplot2 produces publication-quality plots, but are more complex
# We will cover in a few weeks: 
#install.packages("ggplot2")
library(ggplot2)

g1 = ggplot(babies, aes(x=weight))
g1 + geom_histogram(binwidth = 10, color="cyan") +
  ggtitle("Distribution of Weight of Mother") +
  xlab("Weight (lbs)") + ylab("Count")


g2 = ggplot(babies, aes(x=age.level))
g2 + geom_bar(color="cyan") +
  ggtitle("Age Level of Mother") +
  xlab("Age Level") + ylab("Count")


g2 = ggplot(babies, aes(x=smoke))
g2 + geom_bar(color="cyan") +
  ggtitle("Smoking Status of Mother") +
  xlab("Mother smokes?") + ylab("Count")

babies2 <- read.csv(file="babies_quiz.csv", header=T, na.strings = c("?", "99", "999") )
summary(babies2)

# Description of variables:
# bwt Birth weight in ounces 
# gestation Length of pregnancy in days 
# parity 0 = first born, 1 = otherwise 
# age mother's age in years 
# height mother's height in inches 
# weight Mother's pre-pregnancy weight in pounds 
# smoke Smoking status of mother: 0 = not now, 1 = yes now

