#installing packages
install.packages("tidymodels")
install.packages("glmnet")
library(tidymodels)
library(glmnet)

#loading UCI housing data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

#setting the seed
set.seed(123456)

#splitting the data into training and testing sets
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

# Creating a recipe for preprocessing
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # Convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # Convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # Create interaction term between variables
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # Create square terms of some continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# Create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

#Check the dimensions of the training data
dim(housing_train_prepped)
dim(housing)

#Estimating a LASSO model
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_lasso <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_lasso, split = housing_split) %>%
  collect_metrics() %>% print

#show best RMSE
top_rmse %>% print(n = 1)

lasso_fit <- fit(final_lasso, data = housing_train_prepped)
#predict RMSE in sample
lasso_fit %>%
  predict(new_data = housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth = truth, estimate = .pred)


#predict RMSE out of sample
lasso_fit %>%
  predict(new_data = housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth = truth, estimate = .pred)


##Estimating a RIDGE model
tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid over which to try different values of lambda
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec) #%>%
#add_recipe(housing_recipe)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

top_rmse  <- show_best(rec_res, metric = "rmse")
best_rmse <- select_best(rec_res, metric = "rmse")

# Now train with tuned lambda
final_ridge <- finalize_workflow(rec_wf, best_rmse)

# Print out results in test set
last_fit(final_ridge, split = housing_split) %>%
  collect_metrics() %>% print

#show best RMSE
top_rmse %>% print(n = 1)

ridge_fit <- fit(final_ridge, data = housing_train_prepped)
#predict RMSE in sample
ridge_fit %>%
  predict(new_data = housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth = truth, estimate = .pred)

#predict RMSE out of sample
ridge_fit %>%
  predict(new_data = housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth = truth, estimate = .pred)