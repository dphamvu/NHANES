# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
nhanes_test = read_csv("data/clean/nhanes_test.csv", 
                    col_types = "iififfdffifddffffffffffffffffffffiifffffff")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

#evaluate OLS test RMSE
ols_predictions = predict(ols, newdata = nhanes_test) %>% as.numeric()

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

#evaluate decision tree test RMSE

dt_predictions = predict(optimal_tree, newdata = nhanes_test)
dt_RMSE = sqrt(mean((dt_predictions-nhanes_test$mental_score)^2))

#evaluate random forest test RMSE
rf_predictions = predict(rf_fit, newdata = nhanes_test)

rf_RMSE = sqrt(mean((rf_predictions-nhanes_test$mental_score)^2))

#evaluate boosting test RMSE
gbm_predictions = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          
                          newdata = nhanes_test)

gbm_RMSE = sqrt(mean((gbm_predictions - nhanes_test$mental_score)^2))


#evaluate OLS training RMSE  #3.97
ols_predictions_t = predict(ols, newdata = nhanes_train) %>% as.numeric()

ols_RMSE_t = sqrt(mean((nhanes_train$mental_score-ols_predictions_t)^2))

# evaluate ridge training RMSE
ridge_predictions_t = predict(ridge_fit, 
                            newdata = nhanes_train, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE_t = sqrt(mean((ridge_predictions_t-nhanes_train$mental_score)^2))

# evaluate lasso training RMSE
lasso_predictions_t = predict(lasso_fit, 
                            newdata = nhanes_train, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE_t = sqrt(mean((lasso_predictions_t-nhanes_train$mental_score)^2))

#evaluate elastic net regression training RMSE
elnet_predictions_t = predict(elnet_fit,
                            
                            alpha = elnet_fit$alpha,
                            newdata = nhanes_train,
                            s = "lambda.1se") %>% as.numeric()

elnet_RMSE_t=sqrt(mean((elnet_predictions_t - nhanes_train$mental_score)^2))

#evaluate decision tree training RMSE

dt_predictions_t = predict(optimal_tree, newdata = nhanes_train)
dt_RMSE_t = sqrt(mean((dt_predictions_t-nhanes_train$mental_score)^2))

#evaluate random forest training RMSE
rf_predictions_t = predict(rf_fit, newdata = nhanes_train)

rf_RMSE_t = sqrt(mean((rf_predictions_t-nhanes_train$mental_score)^2))

#evaluate boosting training RMSE
gbm_predictions_t = predict(gbm_fit_optimal, n.trees = optimal_num_trees,
                          
                          newdata = nhanes_train)

gbm_RMSE_t = sqrt(mean((gbm_predictions_t - nhanes_train$mental_score)^2))

# print nice table for test RMSE
tibble(Method = c("Ordinary Lease Squares", "Ridge", "Lasso", 
                  "Elastic Net Regression", 
                  "Decision Tree", 
                  "Random Forest", 
                  "Boosted Model"), 
       `Test RMSE` = c(ols_RMSE, ridge_RMSE, lasso_RMSE, elnet_RMSE,
                       dt_RMSE, rf_RMSE, gbm_RMSE),
        `Training RMSE` = c(ols_RMSE_t, ridge_RMSE_t, 
                            lasso_RMSE_t, elnet_RMSE_t, 
                            dt_RMSE_t, rf_RMSE_t, gbm_RMSE_t)) %>%
  write_tsv("results/model-evaluation.tsv")