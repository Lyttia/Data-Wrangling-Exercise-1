#This is the Data Wrangling Exercise 1 script file.

#0a)load packages
install.packages("tidyverse")
library("tidyverse")

#0b)Read in csv file and assign to refine.
refine <- read_csv("refine_original.csv")

#1)clean company names
refine$company <- gsub("^[pf].*[s]$", "philips", refine$company, TRUE)
refine$company <- gsub("^[a].*[o0]$", "akzo", refine$company, TRUE)
refine$company <- gsub("^[v].*[n]$", "van houten", refine$company, TRUE)
refine$company <- gsub("^[u].*[r]$", "unilever", refine$company, TRUE)

#2)separate product code and number
refine <- separate(refine, 2, c("product_code", "product_number"), sep = "-")

#3)add product categories
#3a)assign product code variable
product_code <- c(refine$product_code)

#3b)create function to call product category from product code
categories <- function(x) {
    if(x  == "p"){
    return("Smartphone")
  } else if (x  == "v"){
    return("TV")
  } else if (x  == "x"){
    return("Laptop")
  } else if (x  == "q"){
    return("Tablet")
  }
}

#3c)apply function over product_code to create new vector for product_categories 
product_categories <- sapply(product_code, categories)

#3d)adding product_categories column to refine df
refine <- add_column(refine, product_categories) 

#4)add full address for geocoding
#4a)assigning address, city, country variables
address <- refine$address
city <- refine$city
country <- refine$country

#4)uniting address, city, country to form full address column and add to refine df
refine <- unite(refine, "full_address", address, city, country, sep = ", ")

#5)create dummy variables for company and product category/code                     
company_philips <- ifelse(company == "philips", 1, 0) 
company_akzo <- ifelse(company == "akzo", 1, 0)
company_van_houten <- ifelse(company == "van houten", 1, 0)
company_unilever <- ifelse(company == "unilever", 1, 0)

product_smartphone <- ifelse(product_code == "p", 1, 0) 
product_tv <- ifelse(product_code == "v", 1, 0) 
product_laptop <- ifelse(product_code == "x", 1, 0)
product_tablet <- ifelse(product_code == "q", 1, 0)

#5)add dummy variable columns to refine df
refine <- add_column(refine, company_akzo, company_philips, 
                     company_unilever, company_van_houten, 
                     product_laptop, product_smartphone, product_tablet, 
                     product_tv)

#6)save new df and submit to github
write_csv(refine, "refine_clean.csv")
