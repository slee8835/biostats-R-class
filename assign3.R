#Working with Health Data: Data Cleaning and EDA (Exploratory Data Analysis)

#1. Read the file diabetes2.csv into a dataframe named diab.
setwd("c:/USF MSHI/HS 631/Assignment 3")
diab <- read.csv(file="diabetes2.csv", header=T)  


#2. EDA: Calculate summary statistics on diab and identify which variables seem to
#have invalid data in need of cleaning.
summary(diab)

##insulin, BMI, blood pressure, plasma shouldn't have a minimum at 0
##maximum pregnancy age at 81 seems impossible 
##Triceps skin fold thickness (mm) and BMI's maximums are very far off from the rest of
##the dataset 

#3. EDA: Plot the frequency distributions of all variables in diab and identify which
#variables seem to have invalid data in need of cleaning. Reconcile differences in #2
#and #3, if there are any.
hist(diab$preg)
hist(diab$plas)       #need cleaning
hist(diab$pres)       #need cleaning  
hist(diab$skin)       #need cleaning 
hist(diab$insu)       #need cleaning 
hist(diab$mass)       #need cleaning 
hist(diab$pedi)
hist(diab$age)     
hist(diab$class)


#4. For the variable pres (ie: blood pressure) produce a vector of logical that has the
#value TRUE at any position in the vector diab$pres where the blood pressure is
#that of a zombie rather than that of a human.
pres <- (diab$pres == 0)
pres

#5. For the variable pres use a logical expression from #4 to take a slice of the vector
#diab$pres. This slice will contain all the elements of diab$pres that need to be
#replaced with NA.
diab$pres
diab[pres == TRUE, ]

#6. For the variable pres use your slice of diab$pres from #5 to replace the values that
#should be NA with the value NA.
#Hint: one way is to use something like this, filling in with your logical expression
#from #4 and #5:
diab$pres[pres == TRUE] <- NA
diab$pres

#7. For the variable skin (ie: measure of skin fold thickness at triceps) use a logical
#expression to take a slice of the vector diab$skin. This slice will contain all the
#elements of diab$skin that need to be replaced with NA.
#Hint: the slice will contain all values of diab$skin where the skin fold thickness is
#that of a skeleton rather than that of a human. (Skeletons have zero skin thickness).
skin <- (diab$skin == 0)
skin

#8. For the variable skin use your slice of diab$skin from #7 to replace the values that
#should be NA with the value NA.
diab$skin[skin == TRUE] <- NA

#9. For all the variables other than pres and skin that need cleaning, clean them by
#replacing values that should be NA, with NA
diab$plas[diab$plas == 0] <- NA
diab$insu[diab$insu == 0] <- NA
diab$mass[diab$mass == 0] <- NA
diab

#10. Calculate the pairwise correlations of the variables of diab. Set argument:
#use="pairwise.complete.obs"
cor(diab, use = "pairwise.complete.obs")

#List all the pairs that have correlation > .5.
##preg and age, plas and insu, skin and mass

#List the pair that is least related
##pedi and pressure 

#11. Execute the following commands, each one on a different column of diab:
#summary, range, IQR, quantile, median, mean
summary(diab$preg)
range(diab$plas, na.rm = TRUE)
IQR(diab$pres, na.rm = TRUE)
quantile(diab$skin, na.rm = TRUE)
median(diab$insu, na.rm = TRUE)
mean(diab$mass, na.rm = TRUE)
quantile(diab$pres, na.rm = TRUE)
mean(diab$pres, na.rm = TRUE)
median(diab$pres, na.rm = TRUE)
#Very briefly describe what the commands reveal
##summary provides you with the range (min and max), quantiles, median and mean 
##for range, IQR, quantile, median and mean, na.rm has to be set to TRUE since there 
##are NAs in the dataset 
##IQR gives you the inner quantile range, range gives you the min and max, and quantile 
##provides you with the 0%, 25%, 50%, and 100% quantile by default

#12. Use the plot command to create the default plot type for class variable of diab.
#Include a title, x and y labels in your plot.
plot(diab$class, main = "Class in Diab dataset", xlab = "index", ylab = "class variable")

#13. As in #12, use the plot command to plot the class variable of diab, but first typecast
#(coerce) the class variable to type factor. Include title and axis labels as before.
diab$class <- factor(diab$class)
plot(diab$class, main = "Class in Diab dataset", xlab = "Class variable", ylab = "counts")

#14. Create a plot of your own choosing to show the distribution of a single variable OR
#the relation between 2 variables of the diab dataset. Include a title and axis labels in
#your plot.
table(diab$preg)
count <- table(diab$preg)
count
plot(count, main = "Pregnancy Frequency in Diab Dataset", xlab = "# of pregnancies", 
     ylab = "counts", lwd = 13, col= "sky blue")

#15. Note: this question is meant to be strange; figure out what is going on:
#Execute the following 2 lines of code:
diab$col1 <-diab$pres/(diab$pres-median(diab$pres, na.rm=T) )
diab$col2 <- sqrt(diab$pres/(diab$pres-median(diab$pres, na.rm=T) ) )

#Then execute the two which commands below:
which(is.infinite(diab$col1))
which(is.nan(diab$col2))

#After executing these, explain what happened in the creation of col1 and col2 to
#result in the output of each of the two calls to which
##the first two lines of code created two new vectors in the diab dataframe- col1 and 
##col2. 
##col1 and 2 contain numbers created based on the math equation included in the command 
##line. Next, the line which(is.infinite(diab$col1)) provides you the position in col1 
##where the number is infinite. which(is.nan(diab$col2)) provides you the position in 
##col2 where it is NAN (not a number).
