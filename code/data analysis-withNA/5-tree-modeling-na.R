library(rpart)             # install.packages("rpart")
library(rpart.plot)        # install.packages("rpart.plot")
library(tidyverse)
library(randomForest)
library(gbm)

#read in the training data
nhanes_train = read_csv("data/clean/nhanes_train.csv",
                      col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

#Fitting and plotting a regression tree using default parameters

tree_fit = rpart(mental_score ~ . - subject, data = nhanes_train)

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "results/decision-tree-plot.png")
rpart.plot(tree_fit)
dev.off()

#variable importance of the tree fit model: dr_sleep, age_first_smoke, ratio_income, sleepy, age, dr_highchol, dr_diabetes, race, told_fat, sleep_weekend, hours_worked, min_sedentary, now_smoke, told_weight, told_thyroid, sleep_weekday, edu, told_copd, told_stroke, told_liver
tree_fit$variable.importance

#optimal tree
cp_table = printcp(tree_fit) %>% as_tibble()

cp_table %>%
  ggplot(aes(x = nsplit+1, y = xerror,
             
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  
  geom_point() + geom_line() +
  geom_errorbar(width = 0.2) +
  xlab("Number of terminal nodes") + ylab("CV error") +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed") +
  theme_bw()

optimal_tree_info = cp_table %>%
  filter(xerror - xstd < min(xerror)) %>%
  arrange(nsplit) %>%
  head(1)

optimal_tree = prune(tree_fit, cp = optimal_tree_info$CP)

rpart.plot(optimal_tree)

#the output shows that the optimal tree is the tree fit model 

##Random forest

rf_fit = randomForest(mental_score ~ . - subject, mtry = 41, data = nhanes_train)
plot(rf_fit)

#tuning random forest

mvalues = seq(1,41, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(mental_score ~ . -subject, mtry = m, 
                        data = nhanes_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}

m_and_oob_errors = tibble(m = mvalues, oob_err = oob_errors) 

oob_error_m_plot = m_and_oob_errors %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  labs(x = "Value for m", y = "OOB error")
theme_bw()

ggsave(filename = "results/oob-error-vs-m.png", 
       plot = oob_error_m_plot, 
       device = "png", 
       width = 6, 
       height = 4)

#m = 5 is the best value (the minimum of the curve in the OOB error plot vs m)

# extract m corresponding to min value of OOB error
best_m = m_and_oob_errors %>% arrange(oob_errors) %>% head(1) %>% pull(m)

#tuned the random forest using optimal m

set.seed(1) # for reproducibility 

rf_fit_tuned = randomForest(mental_score ~ . - subject, 
                            mtry = best_m, ntree = 500,
                            importance = TRUE, data = nhanes_train)


# plot OOB error as a function of number of trees

oob_error_plot_before = tibble(oob_error = rf_fit$mse, trees = 1:500) %>%
  ggplot(aes(x = trees, y = oob_error)) + 
  geom_line() + 
  labs(x = "Number of trees", y = "OOB error") + 
  labs(title = "OOB error before tuning")
  theme_bw()

  oob_error_plot_after = tibble(oob_error = rf_fit_tuned$mse, trees = 1:500) %>%
    ggplot(aes(x = trees, y = oob_error)) + 
    geom_line() + 
    labs(x = "Number of trees", y = "OOB error") + 
    labs(title = "Random Forest with m = 5")
  theme_bw()
  
  
  ggsave(filename = "results/oob-error-vs-ntree-tuned-m.png", 
         plot = oob_error_plot_after, 
         device = "png", 
         width = 6, 
         height = 4)

#Produce variable importance plot based on optimal value of m

rf_varImp_plot = varImpPlot(rf_fit_tuned, n.var = 10)

png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename ="results/rf-varImp-plot.png")
rf_varImp_plot = varImpPlot(rf_fit_tuned, n.var = 10)
dev.off()

###BOOSTING

#try out with using the default parameters of 100 trees, a shrinkage factor 
#of 0.1, an interaction depth of 1, and a subsampling fraction π of 0.5.

set.seed(1)
gbm_fit = gbm(mental_score ~ . - subject,
              
              distribution = "gaussian",
              n.trees = 100,
              interaction.depth = 1,
              shrinkage = 0.1,
              cv.folds = 5,
              data = nhanes_train)

#visualize CV error
opt_num_trees = gbm.perf(gbm_fit)
opt_num_trees   #57

##Tuned the interaction depth

set.seed(1)
gbm_fit_1 = gbm(mental_score ~ . -subject,
                
                distribution = "gaussian",
                n.trees = 57,
                interaction.depth = 1,
                shrinkage = 0.1,
                cv.folds = 5,
                data = nhanes_train)

set.seed(1)
gbm_fit_2 = gbm(mental_score ~ . -subject,
                
                distribution = "gaussian",
                n.trees = 57,
                interaction.depth = 2,
                shrinkage = 0.1,
                cv.folds = 5,
                data = nhanes_train)


set.seed(1)
gbm_fit_3 = gbm(mental_score ~ . -subject,
                
                distribution = "gaussian",
                n.trees = 57,
                interaction.depth = 3,
                shrinkage = 0.1,
                cv.folds = 5,
                data = nhanes_train)


set.seed(1)
gbm_fit_4 = gbm(mental_score ~ . -subject,
                
                distribution = "gaussian",
                n.trees = 57,
                interaction.depth = 4,
                shrinkage = 0.1,
                cv.folds = 5,
                data = nhanes_train)

set.seed(1)
gbm_fit_5 = gbm(mental_score ~ . -subject,
                
                distribution = "gaussian",
                n.trees = 57,
                interaction.depth = 5,
                shrinkage = 0.1,
                cv.folds = 5,
                data = nhanes_train)

#extract cv errors

ntrees = 57
cv_errors = bind_rows(
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_1$cv.error, depth = 1),
  
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_2$cv.error, depth = 2),
  
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_3$cv.error, depth = 3),
  
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_4$cv.error, depth = 4),
  
  tibble(ntree = 1:ntrees, cv_err = gbm_fit_5$cv.error, depth = 5)

)

#plot cv errors
d = cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + 
  labs (x = "Number of trees",
        y = "CV error", 
        title = "Tuning interaction depth") +
        theme_bw()

ggsave(filename = "results/tuning-interaction-depth.png", 
       plot = d, 
       device = "png", 
       width = 6, 
       height = 4)

#optimal model and optimal number of trees

#Using min(gbm_fit_4$cv.error) and looking at the graph

gbm_fit_optimal = gbm_fit_4
optimal_num_trees = gbm.perf(gbm_fit_4, plot.it = FALSE)
optimal_num_trees   #34

#To get the variable importance measures

summary(gbm_fit_optimal, n.trees = optimal_num_trees, plotit = FALSE)  
#create partial dependence plots 

partial_dependence_plot_1 = plot(gbm_fit_optimal, 
                                 i.var = "sleepy", 
                                 n.trees = optimal_num_trees)

partial_dependence_plot_2 = plot(gbm_fit_optimal, 
                                 i.var = "dr_sleep", 
                                 n.trees = optimal_num_trees)


partial_dependence_plot_3 = plot(gbm_fit_optimal, 
                                 i.var = "age", 
                                 n.trees = optimal_num_trees)


partial_dependence_plot_4 = plot(gbm_fit_optimal, 
                                 i.var = "age_first_smoke", 
                                 n.trees = optimal_num_trees)

partial_dependence_plot_5 = plot(gbm_fit_optimal, 
                                 i.var = "ratio_income", 
                                 n.trees = optimal_num_trees)

partial_dependence_plot_6 = plot(gbm_fit_optimal, 
                                 i.var = "sleep_weekend", 
                                 n.trees = optimal_num_trees)

partial_dependence_plot_7= plot(gbm_fit_optimal, 
                                 i.var = "sleep_weekday", 
                                 n.trees = optimal_num_trees)
