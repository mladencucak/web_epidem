
# This script cover the basic functions for the intro to R
# Refer to the HTML document (or .rmd) for more comments

### THE BASIC  ####

# Addition
2+2

# Subtraction
10-5

# Multiplication
15*2

# Division
20/10

# Exponents
3^3 # or 3**3

# Modulos (values that remains from the division)
10%%3

# Integer division (fractional part is discarded)
10%/%3




# Mean
mean(c(4,10))

# Standard deviation
sd(c(2,2,4,4,2,2))

# Square root
sqrt(9)

# Natural log
log(100)

# Using log and defining the base (here base of 10)
log(100, base = 10)

# Exp
exp(5)

# Absolute value (not actually a calculation)
abs(-7)


## --

# Using = to assign a value
a = 2 
a

# Or using <- to assign a value
b <- 10 
b

# Tip: Alt and - (Windows) or Option and - (Mac) are shortcut for "<-"

# You can use the assigned object to do calculations
c <- a+b
c

# An object can also be characters 
d <- "R is fun"
d

# Can contain multiple numbers
e <- c(1,2,3,4,5) # the c means concatenate 
e

# They can be grouped into a table (more on this later)
f <- data.frame(First = c(1,2,3,4,5),
                Second = c("A", "B", "C", "D", "E"))
f

## ---
# You cannot use a number as object name
"1" <- 2

# Some words are reserved for specifics functions and cannot be used
NA <- 200
NaN <- 150
TRUE <-100
FALSE <- 50
Inf <- 0

# Other keywords that cannot be used as object name include:
# "break", "else", "for", "if", "next", "repeat", "return", and "while"

# You also cannot use specific keyboard symbols, such as /, @, %, !, etc.
PSU/fito <- 100

PSU@fito <- 100

PSU%fito <- 100


## ---
# mean() calculate the object mean
mean(c(1, 2, 3, 4, 5))

# if you gave a name for an object of "mean", it may create awkward situations
mean <- c(1, 2, 3, 4, 5)
mean(mean)


## ---

# We will learn how to do better plots later, just for example here
library(ggplot2)

#plot data
data_plot  <- data.frame(x=c(1,2,3,4,5,6), y=c(1,2,3,4,5,6))

#plot
plot_name <- ggplot(data_plot, aes(x = x, y=y)) + geom_point() 

#print plot
plot_name


#### DATA TYPES AND STRUCTURE ####

# Numeric

# Lets create an example
numeric_example <- c(1.2, 1.5, 3.14, 2.7182)

# You can directly ask if the results are numeric with the function is.numeric()
is.numeric(numeric_example)

# Or ask what is the class of the vector
class(numeric_example)


# Integer

# Let's start by entering a vector of numbers
integer_example <- c(1,5,7,-4)
is.integer(integer_example)

# If is not integer, what is the class type?
class(integer_example) 

# When you enter the data as numbers, R will assume that is numeric
# We have to inform R that that the values are integers by using the function as.integer()
integer_exemple <- as.integer(c(1,5,7,-4))
is.integer(integer_exemple)
class(integer_exemple) 
mean(integer_exemple)


# Now, if you enter information that has decimals and force the result to be an integer,
# R will ignore the decimal information 
as.integer(c(3.1, 4.9, 5.499999, 9.99999))

# If you enter information as words (class = character) and try to force this to be an integer
# R will return an error
as.integer(c("Sunday", "Monday", "Tuesday", "Wednesday"))

# Now, what enters when you enter numerical values in character format
number_character <- c("1", "2", "3")
class(number_character)

as.integer(number_character)
class(as.integer(number_character))


# Logical

# As mentioned earlier, logical values can be only be considered as TRUE or FALSE
logical_example <- c(TRUE, FALSE, T, F)
is.logical(logical_example)

# Logical operations are very important for situations where you want to check specific conditions,
# for example, if one number is greater than then other
5 > 10        # five is greater than 10?
"Sunday" == "Monday"  # Sunday is equal to Monday?
"Sunday" != "Tuesday" # Sunday is different to Tuesday?

# Here is an example where R implies that the character element "2" 
# should be numeric and gives the correct answer

1 < "2"


# Character

# Character values are basic word elements
character_example <- c("banana", 'epidemiology', "TRUE", "11", "Character can be more than a single word")
is.character(character_example)

# As you can see, they can be defined using either " or '
character_example

# But if you forget to include the ' or ", R will give you an error message, as R thinks that it is an object
character_example <- c("banana", epidemiology)


# Factor

# Different from characters, factors are ordinal, meaning that they have a defined order or structure 
factor_example <- as.factor(c("red", 'blue', "green", "red", "red"))
class(factor_example)

# Note that now we have attributes (levels) with our values
factor_example

# A more useful function is factor(), 
# since, using this function you can define specifically the order and other attributes
factor_example2 <- factor(c("red", 'blue', "green", "red", "red"), levels = c("red", "blue", "green"))
factor_example2

# Note that the order of the levels changed given that we specified the level order
# R by default uses an alphabetical ordering

# You can also change the name of the elements by changing the level label
factor_example3 <- factor(c("red", 'blue', "green", "red", "red", "yellow"), 
                          levels = c("red", "blue", "green", "yellow"), 
                          labels = c("RED", "Blue", "green", "green"))
factor_example3

# What happened? While we added a new color, yellow, we called the label "green", which means
# we redefined what yellow means.  


# Date

# The most common date format is POSIX* 

# POSIX -- describes the date and time, to the millisecond  
# it takes the date in character (string) format
as.POSIXct("2021-04-05 11:30:45")

# You can use different time zones and date formats,
# for example, we define where the date as day, month, and year, 
# but in the New Zealand time zone (NZL)

as.POSIXct("25/04-2021 14:30:45", 
           format = "%d/%m-%Y %H:%M:%OS", 
           tz = "NZ")

# Note that we used /(slash) instead of - (dash). This is only to demonstrate 
# that R can deal with date information using different descriptions, which can be 
# common to different weather data sources.
# Also, it is important to note that although we can use different formats, 
# R still will print information using the default on ISO 8601, which means
# "year-month-day  hour(24):minutes:seconds time zone"


# There are two functions which we can use: POSIXct and POSIXlt

ct <- as.POSIXct("2021-04-05 11:30:45")
lt <- as.POSIXlt("2021-04-05 11:30:45")

# both output look the same
ct
lt

# They are from similar classes
class(ct)
class(lt)

# But, they have internally different attributes, with POSIXlt having multiple components 
unclass(ct) # the big number is the total of seconds since 1970-01-01
unclass(lt)

# It is possible to extract specific information, for example, year, month, day, etc.
weekdays(ct)
months(lt)
quarters(ct)

# The other class is date

dt <- as.Date("2021-04-05 11:30:45")
dt

# As you can see, they are similar to POSIX* functions, however, they include only the specified information
class(dt)
 

##### TYPES OF DATA STRUCTURE IN R ##### 


# Vector

# You can use the function c(), which means concatenate, to create vectors
x <- c(1, 2, 3, 4)
y <- c("a", "b", "c", "d")

x
y

# You can select an element within a vector using brackets, [ ], along with the desired position of the element
y[3] # Will extract and provide the third element in vector y


# Matrix

# Here an example with an matrix with numbers from 1 to 12 and dimensions of 4 rows and 3 columns
matrix_a <- matrix(1:12,ncol=3,nrow=4)
matrix_a

# To extract elements from a matrix, you have to specify first the row and then the column 
matrix_a[2,3]

# Names can be added to rows and columns by using the option 'dimnames'
matrix(1:12,nrow=4,ncol=3 ,
       dimnames = list(c("A", "B", "C", "D"),
                       c("X", "Y", "Z")))

# Array

# Array do not have arguments for the number of rows or columns, 
# but instead it uses the argument 'dim', where you provide the dimensions for row, column, and matrix
array_a <- array(1:36,dim=c(3,4,3)) #3 rows, 4 columns, 3 matrices
array_a # note that the array are actually multiple matrix

# You can also add row, column, and matrix names
array_a =  array(1:36,dim=c(4,3,3), 
                 dimnames = list(c("A", "B", "C", "D"),
                                 c("X", "Y", "Z"),
                                 c("First", "Second", "Third")))


# Similar to vector and matrix, you can extract elements by using [ ] (brackets)
array_a[2,1,2]

# If you do not indicate one of the values, R will collect for all of the other dimensions not specified
array_a[,1,] # extract the first column from all matrix



# Data frame

# Data frames can have vector of different modes (i.e., data types)

# Numeric/character/logical 

vec_numer <- c(1,2,3,4,5)
vec_chacr <- c("A", "B", "C", "D", "E")
vec_logic <- c(T, F, T, F, T)

# The function data.frame() creates new data frames
df <- data.frame(vec_numer, vec_chacr, vec_logic)
df

# The str function will show details of the data frame
str(df)

# To select an column in data.frame you can use $ signal
df$vec_chacr

# As well as the [ ] (brackets), similar to other data structures
df[2,2]


# List

# A list is the most complex type of data structure and
# it can hold all previous structures mentioned before in a single object

a <- 2
b <- c(1,2,3,4,5)
c <- matrix(1:20,4,5)
d <- array(1:40, c(4,5,2))
e <- data.frame(numbers = c(1:5),
                charcters = LETTERS[1:5])

first_list <- list(a, b, c, d, e)
str(first_list)
first_list

# You can add names to each data structure
second_list <- list(scalar = a, vector = b, matrix = c, array = d, data_frame = e)
second_list

# A list can include another list
third_list <- list(scalar = a, vector = b,  data_frame = e, my_list = second_list)
third_list

# Using [ ] (brackets), we can extract different components from within the list
third_list[[3]] # Extract the third component
third_list[[3]][2] # extract the second element of the third component


##### FUNCTIONS ##### 

FUN_weather <- function(x){
  how_weather = paste0("Today the weather is ", x)

  print(how_weather) 
}

FUN_weather("good")

FUN_weather("rainy")

FUN_weather("hot!!!")


# Adding more information
FUN_go_out <- function(x = 15, y = "rain", z = "busy"){
 cond <-  ifelse(x >=  22 &
         y == "sunny" &
         z == "not_busy", 
         "YES", "NO")
  
  paste0("Is it a good day to go out? ", cond)
  
}

# Use the default information
FUN_go_out()

FUN_go_out(x = 25, y = "sunny", z = "not_busy")

FUN_go_out(30, "sunny", "not_busy")

FUN_go_out(x = 30, "sunny")

FUN_go_out(z = "not_busy", x = 20, y = "sunny")


##### PACKAGES ##### 

 
## # To install the package called "psych"
install.packages("psych")
 
## # Now that package is in your computer, to make the functions available to the user, we must first load this from the memory.
library(psych) # load the package
 
# Another way to do this is to use the function require
require(psych)
 


##### WORK DIRECTORY AND PROJECTS ##### 


getwd() # See where the work directory is current located (Note: this will be different for each person)


# Manually 
setwd("D:/OneDrive_PSU/The Pennsylvania State University/Epidem class - PSU") 

# NOTE: one of the most common errors for Windows users is the slash orientation. Where R uses / (forward slash), Windows use \ (backslash). A simple copy and paste, without replacing \ to /, will cause an error.


# You can also set up with your mouse go:
#   Session > Set Working Directory > Choose Directory

# However, the best option is to work with PROJECTS


# Note that because we have defined the work directory as the folder "Epidem class - PSU", 
# which contains the subfolder "data", we do not have to type the entire path for the data
data_demo <- readr::read_csv("1_intro_to_R/data/data_demo.csv")
data_demo


# Show the object structure
str(data_demo)

# Summary stats
summary(data_demo)

# First lines
head(data_demo)

# Last lines
tail(data_demo)



#### DATA WRANGLING AND TIDYVERSE ####


# Tidyverse is a group of packages that shares the same grammar (aka rules)
# For this exercise we will use the package tidyverse (which is just a group of packages)

# If you have not install tidyverse, run the follow line
install.packages("tidyverse")

#Load the package and check the information in the console
library(tidyverse)


# Let's start with some generic data which represent values of the mycotoxin DON

DON <- c(0.1, 2.5, 7.5, 1, 0.9, 3.2, 4.5)

# If you want calculate the mean of the log-transformed values,  
# a traditional way to do this is by creating multiple objects
DON_log <- log(DON)
DON_log_mean <- mean(DON_log)
DON_log_mean

# Another way is to nest all of the functions in a single line
DON_log_nest <- mean(log(DON))
DON_log_nest
   
# Finally though, we can use a pipe which provides a logical flow to work with the data, by linking the steps of the calculation and requiring fewer objects
DON_log_pipe <- DON %>% # The pipe transfer the content of the left side to right function with DON as the data, followed by the transformation and calculation of the mean
    log(.) %>% # the pipe bring the content from one side to the other
    mean(.)
DON_log_pipe



# Filter #

# The function filter works by defining the data source and the specific condition
filter(data_demo, is.na(fdk)) # is.na() will filter only rows where there is missing observations for fdk variable

# Works with pipes as well
data_demo %>%
    filter(is.na(fdk))



data_demo %>% 
  filter(var == "R") %>% # filter only data using the column var that is defined as R (resistant in our example)
  print(n=Inf) # print all observation

data_demo %>% 
  filter(var == "R" & trt == "A") %>% # filter using the column var which contains R and from the column trt which contains A
  print(n=Inf) 

data_demo %>% 
  filter(sev >= 5) %>% # filter the column severity for all values greater than or equal to 5
  print(n=Inf) # print all observation

data_demo %>% 
  filter(fdk < 1 | don == 0) %>% # filter data from the column fdk less than 1 or from the column don where values are exactly equal to 0
  print(n=Inf) 



### Select ###

data_demo %>%
  select(trt, var, blk, sev, inc) # select multiple variables = columns

data_demo %>%
  select(-plot, -yld, -don, -fdk) # another way to achieve the same thing as the previous example, the '-' says to not select those variables


### Mutate ###

data_demo %>%
  mutate(sev_prop = sev/100, # transform yield and inc from a percentage to a proportion and add those columns to the database
         inc_prop = inc/100) 

data_demo %>%
  mutate(yld = yld*67.25,  # to convert yield in bushels per acre to kilograms per hectare. In this case, note that we replace the original values of yld with the new values
         trt_var = paste0(trt, "_", var)) # combine two variables (trt and var) into a single variable and create a new column


### Summarise ###

data_demo %>%
  group_by(trt) %>% # first group the variables by the trt values
  summarise(sev = mean(sev), # apply the functions mean, max (maximum) and sd (standard deviation) to create summaries by trt
            inv = max(inc),
            yld = sd(yld))

data_demo %>%
  group_by(var, trt) %>% # it is possible to group multiple variables, look at the differences with the first example
  summarise(sev = mean(sev), 
            inv = max(inc),
            yld = sd(yld))


### Reshape ###

# For some analyses and plots, we may want to reshape our data and 
# we can do that using the functions pivot_longer() and pivot_wider()

data_longer <- data_demo %>%
  select(-plot) %>%  # remove an extra var (= plot)
  pivot_longer(cols = -c(trt, var, blk), # columns that should not be reshaped as we change the database format
               names_to = "variables", # name the new column for the variables which gathered
               values_to = "values")   # name for the column where the data are provided 
data_longer


#### Export ####




# to export in a csv file
write.csv2(data_longer, file = "data/example_output.csv")

# export as txt file
write.table(data_longer, file = "data/example_output.txt", sep = "\t",
            row.names = FALSE)

# for excel files
 library(openxlsx)
openxlsx::write.xlsx(data_longer, # File that we want export
            file = "1_intro_to_R/data/example_output.xlsx", # location and name of our .xlsx
            sheetName="First_example", # name of sheet
            append=FALSE) # If need to add to an existing file
 

two_data_frame <- list('ex_1' = data_demo, 'ex_2' = data_longer)
openxlsx::write.xlsx(two_data_frame, file = "1_intro_to_R/data/two_sheets.xlsx")

openxlsx::write.xlsx(data_longer,
            file = "example_output.xlsx",
            sheetName="Second_example", overwrite = TRUE,
            append=FALSE) # add to existing .xlsx file




# There are other packages that also export to xlsx file, 
# but they requite JAVA, which may be a problem at PSU computer

library("xlsx")

xlsx::write.xlsx(data_demo, # File that we want export
            file = "1_intro_to_R/data/exemple_xlsx_pack.xlsx", # location and name of our .xlsx
            sheetName="Original_DF", # name of sheet
            append=FALSE) # If need to add to an existing file
 
xlsx::write.xlsx(data_longer,
            file = "1_intro_to_R/data/exemple_xlsx_pack.xlsx",
            sheetName="Long_DF",
            append=TRUE) # add to existing .xlsx file

