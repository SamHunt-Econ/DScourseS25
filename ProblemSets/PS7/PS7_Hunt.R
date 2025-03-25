#Loading packages
install.packages("mice")
install.packages("modelsummary")
library(mice)
library(modelsummary)

#Loading wages.csv as dataframe, check to see if it is a dataframe
class(wages)
print(wages)

#Dropping missing values for hgc or tenure
library(dplyr)
wages <- wages %>% filter(!is.na(hgc) & !is.na(tenure))

#Create a summary table of the data frame
datasummary_skim(wages, 
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


#The data for log wages is missing at a rate of 25%


#creating four linear regression models for each imputation method.

#Listwise Deletion
complete_cases <- wages %>% filter(!is.na(logwage))
model_complete <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = complete_cases)

#Mean Imputation
mean_logwage <- mean(wages$logwage, na.rm = TRUE)
wages_imputed_mean <- wages
wages_imputed_mean$logwage[is.na(wages_imputed_mean$logwage)] <- mean_logwage
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_imputed_mean)

#Predicted Imputation
wages_imputed_predicted <- wages
wages_imputed_predicted$logwage[is.na(wages_imputed_predicted$logwage)] <- predict(model_complete, newdata = wages_imputed_predicted[is.na(wages_imputed_predicted$logwage), ])
model_predicted_imputation <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_imputed_predicted)

#Multiple Imputation
dat_mice <- mice(wages, m = 5, printFlag = FALSE)
model_mice <- with(dat_mice, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))

#Creating the regression outputs as latex code
modelsummary(
  list(
    "Listwise Deletion" = model_complete, 
    "Mean Imputation" = model_mean, 
    "Predicted Imputation" = model_predicted_imputation, 
    "Multiple Imputation" = model_mice
  ), 
  title = "Regression Outputs", 
  output = "latex"
)





