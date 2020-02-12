setwd("c:/USF MSHI/HS 631/Assignment 4")
library(ggplot2)

#1. Read the file babies_assign4.csv into a dataframe named babies4.
babies4 <- read.csv(file= 'babies4.csv', header = T, sep= ,na.strings = "?")  

#2. Perform any necessary data cleaning on babies4.
summary(babies4)

gestation <- babies4$gestation == 999
babies4$gestation[gestation == TRUE] <- NA 

babies4$parity <- factor(babies4$parity)

babies4$age <- as.numeric(babies4$age)    #age should be numeric
age <- babies4$age < 5                    #youngest mom on earch now is 5 y/o
babies4$age[age == TRUE] <- NA
babies4$age

height <- babies4$height == 99            #tallest woman on earth is 91 in
babies4$height[height == TRUE] <- NA

weight <- babies4$weight == 999
babies4$weight[weight == TRUE] <- NA

smoke <- babies4$smoke == '?'
babies4$smoke[smoke == TRUE] <- NA #replace all ? to NA
babies4$smoke <- factor(babies4$smoke)  #remove unused level '?'

#3. Create a new column in babies4 named age_level using this code:
lev <- c("teens", "early.20s", "late.20s", "early.30s", "late.30s", "forty+")
babies4$age_level <- factor (rep (NA,nrow(babies4)), ordered=T, levels=lev)

#4. Your column in #3 contains all NA. Populate it with values using this code:
babies4$age_level         #contains all NAs now 

babies4$age_level[babies4$age<20] <- "teens"
babies4$age_level[babies4$age>=20 & babies4$age<25] <- "early.20s"
babies4$age_level[babies4$age>=25 & babies4$age<30] <- "late.20s"
babies4$age_level[babies4$age>=30 & babies4$age<35] <- "early.30s"
babies4$age_level[babies4$age>=35 & babies4$age<40] <- "late.30s"
babies4$age_level[babies4$age>=40] <- "forty+"

#5. EDA: execute command(s) to calculate summary statistics for all the variables. 
#Be sure to include min and max of all variables, including age_level.
summary(babies4$bwt)
summary(babies4$gestation)
summary(babies4$parity)
summary(babies4$age)
summary(babies4$height)
summary(babies4$weight)
summary(babies4$smoke)
summary(babies4$age_level)
min(babies4$age_level, na.rm = TRUE)    
max(babies4$age_level, na.rm = TRUE)

#6. Data Viz: execute commands to visualize the distribution of each single 
#variable in babies4.
hist(babies4$bwt, main = "Birth Weight Distribution", xlab = "Birth weight 
     (ounces)", ylab = "counts", col = "light blue")

hist(babies4$gestation, main = "Gestation Distribution", xlab = "preganancy 
     length in days", ylab = "counts", breaks = 50, col = "light green")  

plot(babies4$parity, main = "Parity", xlab = "Parity variable", ylab = "counts",
     col = "yellow")

hist(babies4$age, main = "Mother's Age Distribution", xlab = "Mother's age 
     in years", ylab = "counts", col = "pink")         

hist(babies4$height, main = "Mother's Height Distribution", xlab = "Mother's 
     height in inches", ylab = "counts", col = "orange")     

hist(babies4$weight, main = "Mother's Pre-pregnancy Weight Distribution", 
     xlab = "Mother's weight in pounds", ylab = "counts", col = "purple")      

plot(babies4$smoke, main = "Smoking Status of Mom", xlab = "Smoking variable",
     ylab = "counts", col = "brown")

table(babies4$age_level)# provides the heights
counts <- table(babies4$age_level)
barplot(counts, main="Age Level of Mother",
        xlab="Age Level", ylab="Count",
        border="red", col="dark gray")

#7. Data viz: visualize the relationship between a categorical variable and 
#a numeric variable of your choosing in babies4. Create the plot using both 
#base R AND ggplot2 graphics. Write one sentence describing what the plot 
#reveals about the relationship between the two variables.
plot(babies4$bwt ~ babies4$age_level  , main = "Birthweight and Mom's Age level",
     xlab = "Mom's age level", ylab = "baby birthweight in ounces")

g <- ggplot( data=babies4, aes(x = age_level, y = bwt))
g + geom_boxplot(varwidth=F, fill="plum") + labs(title="Birthweight and Mom's Age Level",
                 x = "Mom's age level", y="baby birthweight in ounces")
## the overall birthweights for babies between different age levels of mom don't 
## differ by too much in this dataset.

#8. Data viz: visualize the relationship between two numeric variables of your choosing
#in babies4. Create the plot using both base R AND ggplot2 graphics. Write one
#sentence describing what the plot reveals about the relationship between the two
#variables.
lm(gestation ~ bwt, data = babies4)
plot(gestation ~ bwt , data = babies4, main = "Gestation and Birthweight", xlab = 
       "length of pregnancy in days", ylab = "baby birthweight in ounces")

g1 <- ggplot(data = babies4, aes(x = bwt, y = gestation)) + geom_point(color = "#00AFBB") +
  labs(title = "Gestation and Birthweight", x = "length of pregnancy in days", y = "baby
       birthweight in ounces")
plot(g1)

##the longer the length of pregnancy, you see an increase in baby's birthweight

#9. Use the following table command to produce a contingency table relating age_level
#and smoke as categorical variables
t <- table(babies4$age_level, as.factor(babies4$smoke))
t

#10. Pass your table t to the plot command to produce bar plot relating the variables.
#Write one sentence describing what the plot reveals about the relationship between
#the two variables.
barplot(t, main = "Age Level and Smoking Status of Mom", xlab = "smoking status", 
        ylab = "age level", col=c("sky blue","pink", "green", "gray", "white", "yellow"), 
        legend = rownames(t))

barplot(t, main = "Age Level and Smoking Status of Mom", xlab = "smoking status", 
        ylab = "age level", col=babies4$age_level, 
        legend = rownames(t))

##More non-smokers than smokers in the dataset. Same for all group ages shown in this 
##bar plot 

#11. Use ggplot2 to produce a scatterplot of babies' bwt as a function of gestation,
#colored by whether the mother smokes. Write one sentence describing what the
#plot reveals about the relationship between the two variables.
g2 <- ggplot(data = babies4, aes(x = bwt, y = gestation, color = smoke)) + geom_point() +
  labs(title = "Gestation and Birthweight", x = "length of pregnancy in days", y = "baby
       birthweight in ounces", legend = "smoke")
plot(g2)

##longer the pregnancy, slight increase of baby's birth weight, and you see more 
##non-smokers leaning towards the longer pregnancy side 

#12. Calculate the pairwise correlations of the variables of babies. (You may first
#remove the factor variable.) Set argument: use="pairwise.complete.obs"
sub_babies4 = subset(babies4, select = -c(parity, smoke, age_level ))
cor(sub_babies4, use = "pairwise.complete.obs")

#. List the pair that is most positively correlated. Provide a plausible
#explanation for the correlation.
##height and weight. Taller people usually are heavier because of bone mass 
##and other factors

#List the pair that is most negatively correlated. Provide a plausible
#explanation for the correlation.
##gestation and age. Since this dataset consists a lot of teens, a plausible
##explanation is that for younger moms, their body may not be ready for pregnancy
##yet, which may prolong the pregnancy in order to stabilize the fetus.

#Are bwt and smoke positively or negatively correlated? Provide a plausible
#explanation for the correlation.
##correlation data not available because smoke is a factor 

#13. Use ggplot2 to create a plot of your own choosing using the babies4 dataset. Write
#one sentence describing what the plot reveals about the dataset.
g3 <- ggplot(data = babies4, aes(x = bwt, color = smoke)) + geom_density()
plot(g3)

##Besides from the NA data, non-smokers moms have slightly higher birthweight babies