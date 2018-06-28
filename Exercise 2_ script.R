#This is the Data Wrangling Exercise 2 Script file

#0)Load packages
install.packages("tidyverse")
library("tidyverse")
library(readxl)

#0)Read csv file into titanic df
titanic <- read_excel("titanic_original.csv")
View(titanic)

#1)Port of embarkation: replace missing values with S
  #1note: Passengers with this missing value are known to have embarked from Southampton

#1a)Find missing values
colSums(is.na(titanic))

#1b)Find NA indices in embarked
which(is.na(titanic$embarked))

#1c)Replace missing values with S (Southampton)
titanic$embarked[c(169, 285)] <- "S"

#2)Calculate the mean of the Age column and use that value to populate the missing values
titanic$age[c(which(is.na(titanic$age)))] <- mean(titanic$age, na.rm = TRUE)

#3)Lifeboat: Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'
titanic$boat[c(which(is.na(titanic$boat)))] <- "None"

#4)Create binary cabin variables and add new column to df
has_cabin_number <- ifelse(titanic$cabin == "NA", 0, 1) 

titanic <- add_column(titanic, has_cabin_number)

#5)Submit project on Github
write_csv(titanic, "titanic_clean.csv")

