library(rpart)             # install.packages("rpart")
library(rpart.plot)        # install.packages("rpart.plot")
library(tidyverse)
library(randomForest)
library(gbm)

#read in the training data
nhanes_train = read_csv("data/clean/nhanes_train.csv", 
                      col_types = "iififfdffifddfffffffffffffffffffffiifffffff")

#Fitting and plotting a regression tree

tree_fit = rpart(mental_score ~ . - subject, data = nhanes_train)

tree_plot = rpart.plot(tree_fit)

#save the tree plot

ggsave(filename = "results/decision-tree-plot.png", 
       plot = tree_plot, 
       device = "png", 
       width = 6, 
       height = 4)

#variable important
tree_fit$variable.importance

#optimal tree
cp_table = printcp(tree_fit) %>% as_tibble()

optimal_tree_info = cp_table %>%
  filter(xerror - xstd < min(xerror)) %>%
  arrange(nsplit) %>%
  head(1)

optimal_tree = prune(tree_fit, cp = optimal_tree_info$CP)

rpart.plot(optimal_tree)


##Random forest

rf_fit = randomForest(mental_score ~ . - subject, mtry = 42, data = nhanes_train)
plot(rf_fit)

#tuning random forest
mvalues = seq(1,42, by = 2)
oob_errors = numeric(length(mvalues))
ntree = 500
for(idx in 1:length(mvalues)){
  m = mvalues[idx]
  rf_fit = randomForest(mental_score ~ . -subject, mtry = m, 
                        data = nhanes_train)
  oob_errors[idx] = rf_fit$mse[ntree]
}

m_and_oob_errors = tibble(m = mvalues, oob_err = oob_errors) 
obb_error_m_plot = m_and_oob_errors %>%
  ggplot(aes(x = m, y = oob_err)) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = mvalues) +
  labs(x = "Value for m", y = "OOB error")
  theme_bw()

ggsave(filename = "results/obb_error_vs_m_plot.png", 
       plot = obb_error_m_plot, 
       device = "png", 
       width = 6, 
       height = 4)

#m = 7 is the best value (the minimum of the curve in the OOB error plot vs m)

# extract m corresponding to min value of OOB error
best_m = m_and_oob_errors %>% arrange(oob_errors) %>% head(1) %>% pull(m)

#tuned the random forest using optimal m

set.seed(1) # for reproducibility 

rf_fit_tuned = randomForest(mental_score ~ . -subject, 
                            mtry = best_m, ntree = 500,
                            importance = TRUE, data = nhanes_train)

#Produce variable importance plot based on optimal value of m

rf_varImp_plot = varImpPlot(rf_fit_tuned, n.var = 10)


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
cv_errors %>%
  ggplot(aes(x = ntree, y = cv_err, colour = factor(depth))) +
  geom_line() + theme_bw()

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

