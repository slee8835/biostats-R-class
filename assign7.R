#1. Complete ex 10.1 b on p 185
vec1 <- c(2, 1, 1, 3, 2, 1, 0)
vec2 <- c(3, 8, 2, 2, 0, 0, 0)
vec_pro <- rep(0, length(vec1))

for(i in 1:length(vec1)){
  if(vec1[i] + vec2[i] > 3){
    vec_pro[i] = vec1[i]*vec2[i]
  }
  else{
    vec_pro[i] = vec1[i]+vec2[i]
  }
}
vec_pro

#2. Complete ex 10.2 b on p 191. 
##i.
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low", "High", "High", "High", "Low", "Med", "Med"), 
                    levels = c("Low", "Med", "High"))


if(any(doselevel == "High")){
  if(lowdose >= 10){ lowdose = 10 }
  else lowdose = lowdose/2 
  if(meddose >= 26){ meddose = 26 }
  if(highdose < 60){ highdose = 60 }
  else highdose = highdose * 1.5 
  dosage <- rep(lowdose, length(doselevel))
  
  for(i in 1:length(dosage)){
    if(doselevel[i] == "Med"){ dosage[i] = meddose }
    if(doselevel[i] == "High"){ dosage[i] = highdose }
  }
} else{
    doselevel <- droplevels(doselevel, "High")
    levels(doselevel) <- c("Small", "Large")
    if(lowdose < 15 && meddose <35){
      lowdose = lowdose * 2
      meddose = meddose + highdose
      dosage <- rep(lowdose, length(doselevel))
      
      for(i in 1: length(dosage)){
        if(doselevel[i] == "Large"){dosage[i] = meddose}
      }
    }}
dosage

##ii.
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low", "Low", "Low", "Med", "Low", "Med", "Med"), 
                    levels = c("Low", "Med", "High"))


if(any(doselevel == "High")){
  if(lowdose >= 10){ lowdose = 10 }
  else lowdose = lowdose/2 
  if(meddose >= 26){ meddose = 26 }
  if(highdose < 60){ highdose = 60 }
  else highdose = highdose * 1.5 
  dosage <- rep(lowdose, length(doselevel))
  
  for(i in 1:length(dosage)){
    if(doselevel[i] == "Med"){ dosage[i] = meddose }
    if(doselevel[i] == "High"){ dosage[i] = highdose }
  }
} else{
  doselevel <- droplevels(doselevel, "High")
  levels(doselevel) <- c("Small", "Large")
  if(lowdose < 15 && meddose <35){
    lowdose = lowdose * 2
    meddose = meddose + highdose
    dosage <- rep(lowdose, length(doselevel))
    
    for(i in 1: length(dosage)){
      if(doselevel[i] == "Large"){dosage[i] = meddose}
    }
  }}
dosage

##iii. 
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low", "Med", "Med"), 
                    levels = c("Low", "Med", "High"))


if(any(doselevel == "High")){
  if(lowdose >= 10){ lowdose = 10 }
  else lowdose = lowdose/2 
  if(meddose >= 26){ meddose = 26 }
  if(highdose < 60){ highdose = 60 }
  else highdose = highdose * 1.5 
  dosage <- rep(lowdose, length(doselevel))
  
  for(i in 1:length(dosage)){
    if(doselevel[i] == "Med"){ dosage[i] = meddose }
    if(doselevel[i] == "High"){ dosage[i] = highdose }
  }
} else{
  doselevel <- droplevels(doselevel, "High")
  levels(doselevel) <- c("Small", "Large")
  if(lowdose < 15 && meddose <35){
    lowdose = lowdose * 2
    meddose = meddose + highdose
    }
    dosage <- rep(lowdose, length(doselevel))
    for(i in 1: length(dosage)){
      if(doselevel[i] == "Large"){dosage[i] = meddose}
    }
  }
dosage
doselevel

##iv.
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low", "High", "High", "High", "Low", "Med", "Med"), 
                    levels = c("Low", "Med", "High"))


if(any(doselevel == "High")){
  if(lowdose >= 10){ lowdose = 10 }
  else lowdose = lowdose/2 
  if(meddose >= 26){ meddose = 26 }
  if(highdose < 60){ highdose = 60 }
  else highdose = highdose * 1.5 
  dosage <- rep(lowdose, length(doselevel))
  
  for(i in 1:length(dosage)){
    if(doselevel[i] == "Med"){ dosage[i] = meddose }
    if(doselevel[i] == "High"){ dosage[i] = highdose }
  }
} else{
  doselevel <- droplevels(doselevel, "High")
  levels(doselevel) <- c("Small", "Large")
  if(lowdose < 15 && meddose <35){
    lowdose = lowdose * 2
    meddose = meddose + highdose
  }
  dosage <- rep(lowdose, length(doselevel))
  for(i in 1: length(dosage)){
    if(doselevel[i] == "Large"){dosage[i] = meddose}
  }
}
dosage

#3. Complete ex 10.4 c on p 204

#tests 
mystring <- "R fever"
#mystring <- "beautiful"
#mystring <- "ECCENTRIC"
#mystring <- "ElAbOrAte"
#mystring <- "eeeeek!"
index <- 1
ecount <- 0
result <- mystring

while(ecount < 2 && index <= nchar(mystring)){
  temp_char <- substr(mystring, index, index)
  if(temp_char == 'e' || temp_char == 'E'){ ecount <- ecount + 1}
  if(ecount == 2){result <- substr(mystring, 1, index-1)}
  index <- index + 1
}
result

#4. Read file "babies_assign7.csv" into a dataframe named babies.
setwd("c:/USF MSHI/HS 631/Assignment 7")
babies <- read.csv(file="babies_assign7.csv", header=T) 

#Fit a linear model, named fit1, of bwt as a function of gestation and smoke.
fit1 <- lm(bwt ~ gestation + smoke, data = babies)
#Display a summary of fit1.
summary(fit1)

#Provide the equation of the fitted line for babies of non-smoking mothers
##bwt = 0.44286 * gestation days - 8.08830 * 0 (don't smoke) -0.93166
##bwt = 0.44286 * gestation days -0.93166

#Provide the equation of the fitted line for babies of smoking mothers
##bwt = 0.44286 * gestation days - 8.08830 * 1 (smoke) -0.93166
##bwt = 0.44286 * gestation days - 9.01996

#Provide a value for fit1's goodness of fit. State which quantity in the 
#summary it is
## adjusted-R squared is 0.2099, indicating the goodness of fit is bad 

#Display Residuals vs Fitted plot for fit1
plot(fit1, 1)

#Display a Residuals vs Leverage plot using the command plot(fit1,5)
plot(fit1,5)

#5. Copy babies into a new dataframe named babies6. 
babies6 <- babies

#Use an ifelse statement to modify outliers in gestation to NA. (Consider 
#an outlier to be a gestation value less than 220 days or greater than 
#330 days. 
#Do not use slice to modify outliers
babies6$gestation <-ifelse(220 > babies6$gestation|babies6$gestation > 330, NA, babies6$gestation)
summary(babies6$gestation)

#6. Fit a linear model, named fit6, of bwt as a function of gestation and smoke, using
#babies6.
fit6 <- lm(bwt ~ gestation + smoke, data = babies6)
#Display a summary of fit6.
summary(fit6)

#Provide the equation of the fitted line for babies of non-smoking mothers
##bwt = 0.49824 * gestation days -16.36153

#Provide the equation of the fitted line for babies of smoking mothers
##bwt = 0.49824 * gestation days -24.4157

#Provide a value for fit6's goodness of fit. State which quantity in the summary it is
##Adjusted R-squared- 0.2236

#Display Residuals vs Fitted plot for fit6
plot(fit6, 1)

#Display a Residuals vs Leverage plot using the command plot(fit6,5)
plot(fit6, 5)

#7. Copy babies into a new dataframe named babies7. 
babies7 <- babies

#Use a for loop to loop through babies7$gestation and modify outliers to NA. 
for(i in 1: length(babies7$gestation)){
  if(!(is.na(babies7$gestation[i])) && (babies7$gestation[i]< 220 || babies7$gestation[i] > 330)){
    babies7$gestation[i] = NA
  }
}

summary(babies7$gestation)

#8. Copy babies into a new dataframe named babies8. 
babies8 <- babies

#Use a while loop to loop through babies7$gestation and modify outliers to NA. 
index = 1
while(index <= length(babies8$gestation)){
  if(!(is.na(babies8$gestation[index])) && (babies8$gestation[index]< 220 || babies8$gestation[index] > 330)){
    babies8$gestation[index] = NA
  }
  index = index + 1  
}
summary(babies8$gestation)

#9. Use an apply function with each of these datasets to calculate the means of all of their
#columns: babies, babies6, babies7, and babies8. Be sure that your result is not NA
apply(babies, 2, mean,na.rm = TRUE)
apply(babies6, 2, mean,na.rm = TRUE)
apply(babies7, 2, mean,na.rm = TRUE)
apply(babies8, 2, mean,na.rm = TRUE)

#10. Use function all to verify that all corresponding values of babies7$gestation and
#babies6$gestation are equal.
all(babies6$gestation == babies7$gestation, na.rm = TRUE)

#11. Use function any to verify that there no corresponding values of babies7$gestation and
#babies8$gestation that are unequal.
any(babies7$gestation != babies8$gestation, na.rm = TRUE)
