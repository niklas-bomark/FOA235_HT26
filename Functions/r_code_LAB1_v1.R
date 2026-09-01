

# Check the Console to see if R runs properly

# Set up a working folder (Project)

# Console – where commands run
# Script editor – where code files are written and saved
# Environment/History – shows objects and previous commands
# Files/Plots/Packages/Viewer – for browsing files, viewing plots, etc.




# Create and run a script
# Example
# install.packages("tidyverse")

library(tidyverse)
mtcars %>% head()


# Objects
# In R, everything is an object. When you write code, you are usually creating, modifying, or using objects.
# An object is simply a name that stores something: a number, text, a vector, a table, a model, etc.
# 
# You create objects with the assignment operator: <- or = 

x <- 10

# Now the object x exists and stores the value 10.



# A. Numeric objects

a <- 5
b <- 3.14


# B. Character objects

name <- "Niklas"
course <- "FOA235"


# C. Logical objects

passed <- TRUE
big <- FALSE






# 2. Vectors
# A vector is a sequence of values of the same type.

numbers <- c(1, 2, 3, 4)
numbers2 <- c(5, 5, 5, 5)
multiplier <- 4

# Character vector

fruits <- c("apple", "banana", "pear")

# Logical vector
logicals <- c(TRUE, FALSE, TRUE)




# 3. Data frames (DF)
# You mentioned “DF,” so here is the key part.

# A data frame is one of the most important object types in R.
# It is basically a table where:
# - each column is a vector
# - all columns must have the same length
# - different columns can have different types
# (numeric, text, logical, factors…)

df <- data.frame(
  age = c(20, 25, 30),
  name = c("Niklas", "Ben", "Chris"),
  passed = c(TRUE, FALSE, TRUE)
)


# View the data frame:
df


# Check the structure:
str(df)



# Functions

# Built it
square <- function(x) {
  x^2
}

# Run it
square(4)

