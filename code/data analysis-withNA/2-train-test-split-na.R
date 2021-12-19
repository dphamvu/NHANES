# read in the cleaned data
nhanes_data_wt_NA = read_csv("data/clean/data_wt_NA.csv", 
                      col_types = "iififfdffifddfffffffffffffffffffffiifffffff")


# split into train and test 

set.seed(5) # seed set for reproducibility 
n = nrow(nhanes_data_wt_NA)
train_samples = sample(1:n, round(0.8*n))


# split nhanes_data into training and test sets
nhanes_train = nhanes_data_wt_NA[train_samples,]
nhanes_test = nhanes_data_wt_NA[-train_samples,]

# save the train and test data
write_csv(x = nhanes_train, file = "data/NA/nhanes_train.csv")
write_csv(x = nhanes_test, file = "data/NA/nhanes_test.csv")