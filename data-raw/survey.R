## Create a randomly generated survey for illustrative purposes

# (a) initialise random number generator to ensure replicability
set.seed(10)

# (b) Declare required survey dimensions
num_P <- 1000  #number of persons in survey (must be a multiple of 10)
num_H <- 4 * (num_P / 10)   #number of households in survey

# (c) Declare initial values for household-level variables
hh <- 1:num_H
Household <- NULL
Tenure <- NULL
Household_w0<-NULL
tenure_categories <- c("Own", "Mortgage", "Shared", "Rents")

# (d) Generate values for household-level variables
#     (household size to range from 1 to 4)
hh_size <- 1
for (i in 1:num_H) {
  # Assign each person in household same household id
  Household <- c(Household, rep(hh[i], hh_size))
  # Assign each person in household same random tenure category
  Tenure <- c(Tenure, rep( sample(tenure_categories, 1), hh_size) )
  # Generate a random household weight in the range 0 to 2
  household_weight <- runif(1, 0, 2)
  # Assign weight to all persons in household
  Household_w0 <- c(Household_w0, rep(household_weight, hh_size))
  # Increment household size for next h/hold by 1
  hh_size <- hh_size + 1
  # Reset hh_size to 1 if incremented value > 4
  if (hh_size > 4) {
    hh_size <- 1
  }
}

# (e) Declare intial values for person-level variables

head_of_household <- !duplicated(Household)
Health <- NULL
WorkStatus <- NULL
age_categories <- c("0_15","16_24","25_44","45_64","65plus")
sex_categories <- c("M", "F")
health_categories <- c("Good", "Fair", "Bad")

# (f) Generate values for person-level variables

# Create set of unique person IDs
Person <- 1:num_P

# Random age category, except head of household can't be in category "0_15"
Age <- ifelse (head_of_household == TRUE,
               sample( age_categories[2:5], num_P, replace = T),
               sample( age_categories, num_P, replace = T) )

# Random sex category
Sex <- sample( sex_categories, num_P, replace = T)

# Probability of health category varies by age
for (i in 1:num_P) {
  if (Age[i] %in% c("0_15","16_24","25-44") ) {
    Health[i] <- sample( health_categories, 1,
                         prob = c(0.6,0.3,0.1))
  } else {
    Health[i] <- sample( health_categories, 1,
                         prob = c(0.4,0.3,0.3))
  }
}

# Probability of work status varids by age category
for (i in 1:num_P) {
  if (Age[i] %in% c("0_15","65plus") ) {
    WorkStatus[i] <- "NA"
  } else {
    WorkStatus[i] <- sample( c("Employed", "Unemployed"), 1,
                             prob = c(0.8,0.2))
  }
}

# Random height (mean 170, sd 10)
Height <- rnorm(num_P, 170, 10)

# Random income, but reset to NA for 0-15 year olds
Income <- rnorm(num_P, 25000, 2000)
Income <- ifelse( Age == "0_15", NA, Income)

# Random person-level weight in range 0 to 2
Person_w0<-runif(500,0,2)

# (g) Combine household and person attributes into a survey dataframe
survey <- data.frame( Household, Person, Tenure, Age, Sex,
                      Health, WorkStatus, Height, Income,
                      Household_w0, Person_w0 )

# (h) Change factor orders as required
survey$Tenure <- factor( survey$Tenure, levels = tenure_categories )
survey$Age <- factor( survey$Age, levels = age_categories )
survey$Sex <- factor( survey$Sex, levels = sex_categories )
survey$Health <- factor( survey$Health, levels = health_categories )
survey$WorkStatus <- factor( survey$WorkStatus,
                             levels = c("Employed", "Unemployed") )

# (i) Check survey content is as expected
head( survey, 20 )
str( survey )
levels( survey$Age )
levels( survey$Tenure )
levels( survey$Sex )
levels( survey$Health )
levels( survey$WorkStatus )

# Export data to package
usethis::use_data(survey, overwrite = TRUE)
