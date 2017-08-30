# This is Data Wrangling Exercise 2: Dealing with Missing Values

# Get data
titanic_original <- read.csv("titanic_original.csv")
titanic_draft <- titanic_original

# Replace missing port of embarcation values with Southampton
titanic_draft$embarked[is.na(titanic_draft$embarked)] <- "S"

# Replace missing age values with mean of age column
titanic_draft$age[is.na(titanic_draft$age)] <- 
  mean(titanic_draft$age, na.rm = TRUE)

# Another way to populate missing age values would be to
# take the mean of the age of passengers by title "Mr.", "Mrs.", "Master", "Miss"
# and use those means to fill in missing age values more precisely.

# Fill empty boat values with "none"
titanic_draft$boat[is.na(titanic_draft$boat)] <- "None"

# A missing value for cabin likely means that the passenger rode in steerage.
# It does not make sense to fill in missing cabin values.

# Create column with 1 or 0 for has_cabin_number
dummy_cabin_number <- as.numeric(!is.na(titanic_draft$cabin))
titanic_draft <- cbind(titanic_draft, dummy_cabin_number)

titanic_clean <- titanic_draft






