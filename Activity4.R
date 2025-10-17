#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

# Assign new variables 
sl <- c(iris$Sepal.Length)
sw <- c(iris$Sepal.Width)
pl <- c(iris$Petal.Length)
pw <- c(iris$Petal.Width)


# make list of new variables
one_list <- list(sl, pl, sl)
two_list <- list(sw, pw, pl)

#New list to store regression values
reg_list <- list()

#for loop for a linear regression of each relationship
for (i in 1:3){
reg_list[[i]] <- summary(lm(one_list[[i]]~two_list[[i]]))
}

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(data = NULL)

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
ggplot(data = iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point(data = NULL)

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		