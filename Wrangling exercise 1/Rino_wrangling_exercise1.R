# This is Data Wrangling Exercise 1: Basic Data Manipulation

# get data
refine_original <- read.csv("refine_original.csv")
refine_draft <- refine_original

# get packages
require("tidyr")
require("dplyr")

# correct spellings
refine_draft$company[agrep("philips", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 3)] <- "philips"
refine_draft$company[agrep("van houten", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 3)] <- "van houten"
refine_draft$company[agrep("akzo", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 1)] <- "akzo"
refine_draft$company[agrep("unilever", refine_draft$company, ignore.case = TRUE, value = FALSE, max.distance = 3)] <- "unilever"

# separate product code and number
refine_draft <- separate(refine_draft, 'Product.code...number', c('product_code', 'product_number'), sep = "-")

# add product category column
refine_draft <- mutate(refine_draft, 'product_category' = product_code)

refine_draft$product_category <- gsub("p", "Smartphone", refine_draft$product_category)
refine_draft$product_category <- gsub("v", "TV", refine_draft$product_category)
refine_draft$product_category <- gsub("x", "Laptop", refine_draft$product_category)
refine_draft$product_category <- gsub("q", "Tablet", refine_draft$product_category)

# add full address
refine_draft <- unite(refine_draft, "full_address", address, city, country, sep = ", ")

# create dummy variables for company category
dummy_philips <- as.numeric(refine_draft$company == "philips")
dummy_akzo <- as.numeric(refine_draft$company == "akzo")
dummy_van_houten <- as.numeric(refine_draft$company == "van houten")
dummy_unilever <- as.numeric(refine_draft$company == "unilever")

refine_draft <- cbind(refine_draft, dummy_philips, dummy_akzo, dummy_van_houten, dummy_unilever)

refine_clean <- refine_draft
