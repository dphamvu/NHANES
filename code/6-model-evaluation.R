# load libraries
library(glmnetUtils)
library(tidyverse)
library(readr)

# load test data
nhanes_test = read_csv("data/clean/nhanes_test.csv", 
                   col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

                   

nhanes_train = read_csv("data/clean/nhanes_train.csv", 
                    col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

#load elnet fit object
load("results/elnet_fit.Rda")

#evaluate OLS test RMSE
ols_predictions = predict(ols_fit, newdata = nhanes_test) %>% as.numeric()

ols_RMSE = sqrt(mean((nhanes_test$mental_score-ols_predictions)^2))


# evaluate ridge test RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = nhanes_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-nhanes_test$mental_score)^2))

# evaluate lasso test RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = nhanes_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-nhanes_test$mental_score)^2))

#evaluate elastic net regression test RMSE
elnet_predictions = predict(elnet_fit,
                            
                            alpha = elnet_fit$alpha,
                            newdata = nhanes_test,
                            s = "lambda.1se") %>% as.numeric()

elnet_RMSE=sqrt(mean((elnet_predictions - nhanes_test$mental_score)^2))


#evaluate random forest test RMSE
rf_predictions = predict(rf_fit_tuned, newdata = nhanes_test)

rf_RMSE = sqrt(mean((rf_predictions-nhanes_test$mental_score)^2))

#evaluate boosting test RMSE
gbm_predictions = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          
                          newdata = nhanes_test)

gbm_RMSE = sqrt(mean((gbm_predictions - nhanes_test$mental_score)^2))

# intercept-only prediction error
training_mean_response = mean(nhanes_train$mental_score)
constant_RMSE = sqrt(mean((training_mean_response-nhanes_test$mental_score)^2))


#evaluate OLS training RMSE  #3.97
ols_predictions_tr = predict(ols_fit, newdata = nhanes_train) %>% as.numeric()

ols_RMSE_tr = sqrt(mean((nhanes_train$mental_score-ols_predictions_tr)^2))

# evaluate ridge training RMSE
ridge_predictions_tr = predict(ridge_fit, 
                            newdata = nhanes_train, 
                            s = "lambda.1se") %>%
  as.numeric()

ridge_RMSE_tr = sqrt(mean((ridge_predictions_tr-nhanes_train$mental_score)^2))

# evaluate lasso training RMSE
lasso_predictions_tr = predict(lasso_fit, 
                            newdata = nhanes_train, 
                            s = "lambda.1se") %>%
  as.numeric()

lasso_RMSE_tr = sqrt(mean((lasso_predictions_tr-nhanes_train$mental_score)^2))

#evaluate elastic net regression training RMSE
elnet_predictions_tr = predict(elnet_fit,
                            
                            alpha = elnet_fit$alpha,
                            newdata = nhanes_train,
                            s = "lambda.1se") %>% as.numeric()

elnet_RMSE_tr=sqrt(mean((elnet_predictions_tr - nhanes_train$mental_score)^2))


#evaluate random forest training RMSE
rf_predictions_tr = predict(rf_fit_tuned, newdata = nhanes_train)

rf_RMSE_tr = sqrt(mean((rf_predictions_tr-nhanes_train$mental_score)^2))

#evaluate boosting training RMSE
gbm_predictions_tr = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          
                          newdata = nhanes_train)

gbm_RMSE_tr = sqrt(mean((gbm_predictions_tr - nhanes_train$mental_score)^2))

# print nice table for test RMSE
tibble(Method = c("Ordinary Lease Squares", "Ridge", "Lasso", 
                  "Elastic Net Regression", 
                  "Random Forest", 
                  "Boosted Model"), 
       
        `Training RMSE` = c(ols_RMSE_tr, ridge_RMSE_tr, 
                            lasso_RMSE_tr, elnet_RMSE_tr, 
                            rf_RMSE_tr, gbm_RMSE_tr),
       
       `Test RMSE` = c(ols_RMSE, ridge_RMSE, lasso_RMSE, elnet_RMSE,
                        rf_RMSE, gbm_RMSE)) %>%
  write_tsv("results/model-evaluation.tsv")