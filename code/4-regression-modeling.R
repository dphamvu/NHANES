# load libraries
library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data

nhanes_train = read_csv("data/clean/nhanes_train.csv", 
                    col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

#run OLS
set.seed(1)
ols  = lm (log(mental_score) ~. - subject, data = nhanes_train)
summary(ols)


# run ridge regression
set.seed(1)
ridge_fit = cv.glmnet(mental_score ~ . - subject,   
                      alpha = 0,                 
                      nfolds = 10,               
                      data = nhanes_train)

# save the ridge fit object
save(ridge_fit, file = "results/ridge_fit.Rda")

# lambda based on one-standard-error rule (161)
ridge_fit$lambda.1se

#To get the fitted coefficients at lambda based on the one-standard-error-rule:
coef(ridge_fit, s = "lambda.1se") %>% head()

#create ridge CV plot
plot_glmnet(ridge_fit, nhanes_train)

plot_glmnet(ridge_fit, nhanes_train, features_to_plot = 7)

#dr_sleep, female, ratio_income, vigor_rec, weight_self_percept

# run lasso regression
set.seed(1)
lasso_fit = cv.glmnet(mental_score ~ . - subject,   
                      alpha = 1,                 
                      nfolds = 10,               
                      data = nhanes_train)

# save the lasso fit object
save(lasso_fit, file = "results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
lasso_plot = plot_glmnet(lasso_fit, nhanes_train, features_to_plot = 6)
ggsave(filename = "results/lasso-trace-plot.png", 
       plot = lasso_plot, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients (dr_sleep)
beta_hat_std = extract_std_coefs(lasso_fit, nhanes_train)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("results/lasso-features-table.tsv")

###ELASTIC NET REGRESSION

set.seed(1)
elnet_fit = cva.glmnet(mental_score ~ . -subject, 
                       
                       nfolds = 10, 
                       data = nhanes_train) 

elnet_fit_best = extract_best_elnet(elnet_fit)

plot(elnet_fit_best)

#create elastic net regression trace plot (sleep, female, ratio_income, vigor_rec)

plot_glmnet(elnet_fit_best, nhanes_train)

plot_glmnet(elnet_fit_best, nhanes_train, features_to_plot = 6)

