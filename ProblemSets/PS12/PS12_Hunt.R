#installing packages
library(sampleSelection)
library(tidyverse)
library(modelsummary)

#loading the data
wage_data <- read.csv("wages12.csv")

#mutating variables as factors
wage_data$college <- as.factor(wage_data$college)
wage_data$married <- as.factor(wage_data$married)
wage_data$union   <- as.factor(wage_data$union)

#creating summary table
datasummary_skim(wage_data, 
                 title = "Summary Statistics", 
                 fun_numeric = getOption("modelsummary_fun_numeric", default = list(
                   Unique = NUnique,
                   `Missing Pct.` = PercentMissing,
                   Mean = Mean,
                   SD = SD,
                   Min = Min,
                   Median = Median,
                   Max = Max
                 )), 
                 output = "latex")
#logwages are missing at a rate of 31%
#logwages are most likely to be

#estimating linear regression models with the different imputation methods
#listwise deletion
complete_cases <- wage_data %>% filter(!is.na(logwage))
model_complete <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = complete_cases)

#mean imputation
mean_logwage <- mean(wage_data$logwage, na.rm = TRUE)
wages_imputed_mean <- wage_data
wages_imputed_mean$logwage[is.na(wages_imputed_mean$logwage)] <- mean_logwage
model_mean <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = wages_imputed_mean)

#Heckman selection
wage_data_heckman <- wage_data %>%
  mutate(
    valid = ifelse(!is.na(logwage), 1, 0),
    logwage = ifelse(is.na(logwage), 0, logwage)
  )
model_heckman <- selection(
  selection = valid ~ hgc + union + college + exper + married + kids,
  outcome   = logwage ~ hgc + union + college + exper + I(exper^2),
  data      = wage_data_heckman,
  method    = "2step"
)


#Creating a model summary table for our regressions
modelsummary(
  list(
    "Listwise Deletion" = model_complete, 
    "Mean Imputation" = model_mean, 
    "Heckman Selection" = model_heckman
  ), 
  title = "Regression Outputs",
  output = "latex"
)

#Estimating a probit model
model_union_probit <- glm(
  union ~ hgc + college + exper + married + kids,
  family = binomial(link = "probit"),  # <-- PROBIT model
  data = wage_data
)
modelsummary(model_union_probit)

#Assesing impact of counterfactual
# Step 1: Compute predicted probabilities from the original probit model
wage_data$predProbit <- predict(model_union_probit, type = "response")
print(summary(wage_data$predProbit))

# Step 2: Simulate counterfactual — set coefficients on married and kids to zero
cf_model <- model_union_probit  # make a copy of the model object
cf_model$coefficients["married1"] <- 0
cf_model$coefficients["kids1"] <- 0

# Step 3: Compute counterfactual predicted probabilities
wage_data$predProbit_cf <- predict(cf_model, newdata = wage_data, type = "response")
print(summary(wage_data$predProbit_cf))

# Step 4: Compare averages
mean_original <- mean(wage_data$predProbit)
mean_counterfactual <- mean(wage_data$predProbit_cf)
diff <- mean_counterfactual - mean_original

mean_original
mean_counterfactual
diff