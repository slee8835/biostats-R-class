##assignment 1## 

## Q1 
#Create a numeric vector named nums containing integers from 4 down
#to -3. 
nums <- 4:-3

#Create a character vector named colors containing the colors 
#"red", "blue", "green" and "yellow". 
colors <- c("red", "blue", "green", "yellow")
class(colors)
#Create a logical vector named assertions containing TRUE and FALSE. 
assertions <- TRUE:FALSE

#Print the length of each vector above. 
length(nums)
length(colors)
length(assertions)

## Q2 
#Combine the vectors nums, colors and assertions using the c( ) 
#function and store in a variable named combined.data. 
combined.data <- c(nums, colors, assertions)

#Print combined.data. Describe combined.data.
print(combined.data)

#Briefly, what do you conclude about the limitation of vectors? 
#combined.data is a vector with 14 elements. the limitation for 
#vector is that it can only store one type of data 

## Q3
#Combine the vectors nums, colors and assertions into a data frame 
#named my.data. 
my.data <- data.frame(nums, colors, assertions)

#Briefly describe what resulted when R combined the data. 
#R created a dataframe with three columns: nums, colors and assertions.
#each column contains elecments from the corresponding vector


#Briefly, what do you conclude about the limitation of data frames?
#with data frames, the row numbers have to be identical for each column.
#as a result, shorter vectors will have repeated elements to fill in

#Display the number of rows and number of columns in the data frame. 
print(nrow(my.data))
print(ncol(my.data))

#Display the names of the rows and columns. 
print(row.names(my.data))
print(colnames(my.data))

## Q4
#Create a new vector named colors.v to hold the contents of the 
#column color of my.data. Print its type. 
colors.v <- my.data$colors
class(colors.v)

#the type here is factor

#Now create a new data frame named my.data2 with the same code as 
#previously, except this time use stringsAsFactors=F. 
my.data2 <- data.frame(nums, colors,assertions, stringsAsFactors=F)

#Create a new vector named colors2.v to hold the contents of the 
#column color of my.data2. 
colors.v2 <- my.data2$colors 

#Print its type. Very briefly, what did you learn?
class(colors.v2)

#since we set stringsAsFactors = F, the string vector now is set as 
#characters instead of factor

## Q5
#Create a new data frame named colors.df to hold the contents 
#of the column color of my.data.
colors.df <- data.frame(my.data$colors)

#Print its type to verify it is a data frame.
class(colors.df)

#Create a new data frame of only those rows of my.data whose 
#color attribute is NOT "green".
no_green.df = data.frame(my.data[which(my.data$colors!= "green"),])
no_gre.df <- data.frame(my.data[my.data$colors != "green", ])
no_gre.df
#Create a new data frame named threes consisting of the rows of 
#my.data whose nums are a multiple of 3. (Hint: use %% for modulus 
#operator).
threes <-my.data[nums%%3==0, ]

#Create a data frame named colors.nums of my.data containing only 
#columns colors and nums. Print the contents of these data frames.

color.nums <- data.frame(my.data[ ,-3])
color.nums2 <- data.frame(my.data[ ,1:2])
## Q6
#Create a new vector named new.nums to hold values of nums converted 
#to type character 
new.nums <- as.character(nums)

#new vector named new.colors to hold values of colors converted to 
#type factor
new.colors <- as.factor(colors)

#new vector names new.assertions to hold the values of assertions 
#converted to type numeric.
new.assertions <- as.numeric(assertions)

#Print the contents the original vectors and these new vectors so you 
#can see the difference. 
print(nums)
print(new.nums)
print(colors)
print(new.colors)
print(assertions)
print(new.assertions)

#State whether the functions as.character(), as.factor(), 
#and as.numeric() are mutators of their arguments or whether 
#they create and modify copies instead.
## the functions convert the vector types to the stated ones, 
##therefore are mutators

## Q7
#instead of creating new vectors, modify the original vectors nums 
#and assertions as directed above. 
nums <- as.character(nums)
assertions <- as.numeric(assertions)

#Again look at the my.data. If you modify a vector from which a data 
#frame has been constructed, does the data frame itself change? 
print(my.data)
class(my.data$nums)
class(my.data$colors)
class(my.data$assertions)
#changing the vector that constructed the data frame does not change 
#the types in the data frame itself

#Also briefly explain the difference between is.factor and as.factor. 
print(is.factor(new.assertions)) 
print(is.factor(new.colors))

## Q8
#Print the nums column of my.data using the column index. 
print(my.data[ ,1])

#Print the nums column of my.data using the column name.
print(my.data$nums)

#Print the value in the nums column of the first row.
print(my.data[1,1])
my.data$nums[1]
#Is the value in the nums column of row 1 less than the value 
#in the nums column of row 2? Calculate and print this.
print(my.data[1,1])
print(my.data[2,1])
if(my.data[1,1]>my.data[2,1]){
  print("row 1 value is greater than row2")
}
#the value in nums column of row 1 is more than row 2. 4>3

#Is the value in the nums column of row 8 less than the value 
#in the nums column of row 2? Again calculate and print this
if(my.data[8,1]>my.data[2,1]){
  print("row 8 value is greater than row 2")
}else{
    print("row 8 value is less than row 2")
}
#row 8 value is less than row 2

## Q9
#Modify the nums column of my.data to be of type factor. Make sure 
#the levels are ordered correctly for display purposes, in ordinary 
#numeric order.  (Hint: use the factor function with levels=). 
 
my.data$nums <- factor(my.data$nums, levels=sort(nums))
class(my.data$nums)
#Also modify the assertions column to be of type numeric.
my.data$assertions <- as.numeric(my.data$assertions)
class(my.data$assertions)

## Q10
#Repeat all your print statements from #8 above and look over the 
#results. 
print(my.data[ ,1])
print(my.data$nums)
print(my.data[1,1])

if(my.data[1,1]>my.data[2,1]){
  print("row 1 value is greater than row2")
}

if(my.data[8,1]>my.data[2,1]){
  print("row 8 value is greater than row 2")
}else{
  print("row 8 value is less than row 2")
}

#execute the following line: 
my.data$nums <- as.ordered(my.data$nums) 
print(my.data$nums)

#the last 2 print statements that tell you: Is the value in the nums 
#column of row 1 less than the value in the nums column of row 2?  
if(my.data[1,1]>my.data[2,1]){
  print("row 1 value is greater than row2")
}

#And: Is the value in the nums column of row 8 less than the value in the nums column of row 2? 
#What do you conclude about ordinary factors as opposed to ordered factors
if(my.data[8,1]>my.data[2,1]){
  print("row 8 value is greater than row 2")
}else{
  print("row 8 value is less than row 2")
}
#when it is not ordered, > is not meaningful for factors 

## Q11
#Rename the columns of my.data to be descriptive names of your 
#choosing. 
colnames(my.data)[colnames(my.data)=="nums"] <- "numbers"
colnames(my.data)[colnames(my.data)=="colors"] <- "beautiful colors"
colnames(my.data)[colnames(my.data)=="assertions"] <- "T or F"

## Q12
#Create a list made up of a string, the vector nums, NA and FALSE (in this same order)
mylist <- list("hi", nums, "NA", FALSE)

#Create names for the list elements: "char", "numeric", "NA", "logical".
names(mylist) <- c("char", "numeric", "NA", "logical")

#Use any(is.na( )) to report if any of the list names are NA.
is.na(mylist)

#Now modify "NA" to NA and check again if any of the list names are NA.
mylist$"NA" <- NA

#Use all(is.na( )) to report if all of the list names are NA
is.na(mylist)

#Display the 2nd element of your list.
mylist[2]

## Q13 
#Create an array named my.array having dimensions of 2 rows and 3 columns, and
#containing the numbers 1 to 6.
my.array <- array(1:6, dim= c (2, 3))
my.array

#Modify my.array so the element in row 2 column 3 and in row 1, column 2 are
#both set to NA.
my.array[2,3] <- NA
my.array[1,2] <- NA
my.array

#Write code to set all NAs in my.array to zero. (Hint: use arr[is.na(arr)] <- 0)
my.array[is.na(my.array)] <- 0
my.array
