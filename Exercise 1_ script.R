#This is the Data Wrangling Exercise 1 script file.

#0)load packages
install.packages("tidyverse")
library("tidyverse")
#0)Read in csv file.
refine <- read_csv("refine_original.csv")

#1)clean names
refine$company <- gsub("^[pf].*[s]$", "philips", refine$company, TRUE)
refine$company <- gsub("^[a].*[o0]$", "akzo", refine$company, TRUE)
refine$company <- gsub("^[v].*[n]$", "van houten", refine$company, TRUE)
refine$company <- gsub("^[u].*[r]$", "unilever", refine$company, TRUE)

#2)separate product code and number
refine <- separate(refine, 2, c("product_code", "product_number"), sep = "-")

#3)add product categories
product_code <- c(refine$product_code)

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

product_categories <- sapply(product_code, categories)

refine <- add_column(refine, product_categories) 

#4)add full address for geocoding
address <- refine$address
city <- refine$city
country <- refine$country

refine <- unite(refine, "full_address", address, city, country, sep = ", ")

#5)create dummy variables for company and product category
                     
company_philips <- ifelse(company == "philips", 1, 0) 
company_akzo <- ifelse(company == "akzo", 1, 0)
company_van_houten <- ifelse(company == "van houten", 1, 0)
company_unilever <- ifelse(company == "unilever", 1, 0)

product_smartphone <- ifelse(product_code == "p", 1, 0) 
product_tv <- ifelse(product_code == "v", 1, 0) 
product_laptop <- ifelse(product_code == "x", 1, 0)
product_tablet <- ifelse(product_code == "q", 1, 0)

refine <- add_column(refine, company_akzo, company_philips, 
                     company_unilever, company_van_houten, 
                     product_laptop, product_smartphone, product_tablet, 
                     product_tv)

#6)submit to github
write_csv(refine, "refine_clean.csv")
