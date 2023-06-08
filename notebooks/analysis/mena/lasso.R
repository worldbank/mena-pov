## Run a lasso regression to determine the covariates


# Load Data ---------------------------------------------------------------
vul_df <- read_csv(file.path(mena_file_path,
                             "csv_files_all_layers",
                             "Vul_Indicators_ADM2_std.csv"))

colnames(vul_df)


# Prepare Data ------------------------------------------------------------
vul_df_sub <- vul_df %>%
  select(ID_ADM,
         pm25_2019,
         flood_2020,
         quake_2020,
         loss_2020,
         precip_2020,
         temp_2020,
         drought_2020,
         crop_2020,
         RWI_mean,
         Vul_2016_2020,
         VulPop_2016_2020,
         VulPopSh_2016_2020)

#create a set of independent variables by removing unnecessary variables
rm_vars <- c("Vul_2016_2020",
         "VulPop_2016_2020",
         "VulPopSh_2016_2020")

long_vars  <- setdiff(colnames(vul_df_sub), rm_vars)



# omit missing values
vul_df_sub <- na.omit(vul_df_sub)


# Split the data ----------------------------------------------------------

#split the data (7:3) and create training data
set.seed(02138)
split <- rsample::initial_split(vul_df_sub, prop = 0.7)
vul_train <- training(split)
vul_test <- testing(split)

# glmnet requires users to input a numeric X matrix and a y vector directly, instead of specifying a formula as in lm. 

# create these objects 
y_c_train <- vul_train$VulPopSh_2016_2020
X_train   <- as.matrix(select(vul_train, !!!long_vars))

X_train

# run the lasso regression
fit_lasso <- cv.glmnet(x =X_train, y = y_c_train)

#predict and store it in the training dataset
vul_train <- vul_train %>%
  mutate(pred_lasso_c = as.vector(predict(fit_lasso, newx =X_train, s = "lambda.1se")))

#compute mean squared error 
vul_train %>%
  summarize(mse = (mean((VulPopSh_2016_2020 - pred_lasso_c)^2)))


##########################
#repeat for the test data##
#########################

#create a test set
y_c_test <- vul_test$VulPopSh_2016_2020
X_test   <- as.matrix(select(vul_test, !!!long_vars))


# Lasso 
vul_test <- vul_test %>%
  mutate(pred_lasso_c = as.vector(predict(fit_lasso, newx = X_test, s = 0.01)))

#compute mean squared error 
vul_test %>%
  summarize(mse = round(mean((VulPopSh_2016_2020 - pred_lasso_c)^2),2))

#create a dataset to see which variables are selected by lasso
coef_lasso <- enframe(coef(fit_lasso, s = "lambda.1se")[, 1])
coef_lasso

plot(fit_lasso)

coef(fit_lasso, s = "lambda.1se")

fit_lasso
