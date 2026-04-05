# Script Settings and Resources 
# Note: the xgboost package did not run unless the version 1.7 was specified, there seems to currently be a bug where the xgboost package is currently not capatible with the current version of caret (see here for more details: https://stackoverflow.com/questions/79849114/new-version-of-xgboost-package-is-not-working-under-caret-environment)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(GGally)
install.packages("xgboost", repos = "https://p3m.dev/cran/2025-12-01")
library(caret)
library(glmnet)
library(ranger)
library(xgboost)

# Data Import and Cleaning 
# The data was imported from an SPSS file (.sav) using the read_sav function from the haven package, as read_delim cannot be used on .sav files as the delim is not easily stored as in other file types (e.g. tabs, commas). To ensure that missing, don't know, not applicable, etc. values were coded as missing values (NAs in R), the user_na argument was set to false. Although this is the default value for this argument, you can double check using the codebook and by setting the user_na argument to true (and calling attributes on the columns to see these specific values. e.g. -98 = don't know) that any of these values was coded as values between -100 and -10. The read_csv function automatically on import converts these values into NAs. The mosthrs variable was filtered to keep only participants who did not have an NA value for this question, then this column was renamed to work hours. Finally, the hrs1 and hrs2 columns were removed and also columns where 75% or more of the responses were NA values were removed. 
gss_tbl <- read_sav("~/Desktop/psy8712-week10/data/GSS2016.sav", 
                    user_na = FALSE) %>%
  filter(!is.na(mosthrs)) %>%
  rename(`work hours` = mosthrs) %>%
  select(-c(hrs1, hrs2)) %>%
  select(where(~ mean(is.na(.)) < 0.75)) %>%
  mutate(`work hours` = as.numeric(`work hours`))

# Visualization 
# Using the ggpairs function from the GGally package, this displays a probability distribtion of the variable "work hours". The distribution peaks at around 40 hours with a slight positive skew to the data. 
gss_tbl %>%
  ggpairs(columns = "work hours")

# Analysis
# This section first creates the split in the gss_tbl to create the gss_training dataset and the gss_holdout dataset to be used to train/evaluate the 4 different models. myControl creates the same trainControl for each model. Each model is trained (using train) to predict 'work hours' from all other predictors (using ~ .). A similar strucure is used to create all 4 models, with notably changing the method = argument for each model. For all models except for the OLS model, hyperparameters are tuned using the expand.grid() function with either customized ranges or set values for each hyperparameter to have a plausible range of hyperparameters for each model. After training each model on the training data, the results of the models are stored and summarized, where the cv_r2 values are extracted for later use. Finally, the holdout data is used to create the ho_r2 values using postResample(predict) to easily calculate and extract the R2 values.
index <- createDataPartition(gss_tbl$`work hours`, p = 0.75, list = FALSE)
gss_training <- gss_tbl[index, ]
gss_holdout <- gss_tbl[-index, ]

myControl <- trainControl(method="cv", number=10, verboseIter=T)

OLS_model <- train(
  `work hours` ~ .,
  gss_training,
  method = "lm",
  na.action = na.pass,
  preProcess=c("medianImpute","center","scale"),
  trControl= myControl
)
OLS_model

glmGrid <- expand.grid(alpha = c(0, 1), 
                       lambda = seq(0.0001, 0.1, length = 10))

Glmnet_model <- train(
  `work hours` ~ .,
  gss_training,
  method = "glmnet",
  na.action = na.pass,
  preProcess=c("medianImpute","center","scale"),
  trControl= myControl,
  tuneGrid = glmGrid
)
Glmnet_model

rfGrid <- expand.grid(mtry = c(2, 5, 10),
                      splitrule = "variance", 
                      min.node.size = c(1, 5, 10))

Rf_model <- train(
  `work hours` ~ .,
  gss_training,
  method = "ranger",
  na.action = na.pass,
  preProcess=c("medianImpute","center","scale"),
  trControl= myControl,
  tuneGrid = rfGrid
)
Rf_model

xgbGrid <- expand.grid(
  nrounds = 100,             
  max_depth = c(3, 6),       
  eta = c(0.1, 0.3),       
  gamma = 0,                
  colsample_bytree = 0.8,   
  min_child_weight = 1,     
  subsample = c(0.5, 0.8, 1) 
)

xgb_model <- train(
  `work hours` ~ .,
  data = gss_training,
  method = "xgbTree",
  na.action = na.pass, 
  preProcess=c("medianImpute", "center", "scale", "nzv"),
  trControl= myControl,
  tuneGrid = xgbGrid
)
xgb_model

results <- (resamples(list(OLS = OLS_model, ENET = Glmnet_model, RF = Rf_model, XGB = xgb_model)))

summary(results)
dotplot(results)

cv_summary <- summary(results)$statistics$Rsquared
cv_r2_values <- cv_summary[, "Mean"] 

ho_r2_ols <- postResample(predict(OLS_model, gss_holdout, na.action = na.pass), gss_holdout$`work hours`)[["Rsquared"]]
ho_r2_glm <- postResample(predict(Glmnet_model, gss_holdout, na.action = na.pass), gss_holdout$`work hours`)[["Rsquared"]]
ho_r2_rf <- postResample(predict(Rf_model, gss_holdout, na.action = na.pass), gss_holdout$`work hours`)[["Rsquared"]]
ho_r2_xgb <- postResample(predict(xgb_model, gss_holdout, na.action = na.pass), gss_holdout$`work hours`)[["Rsquared"]]

# Publication
# This section creates table1_tbl, which displays the R-squared values for the 4 different models for both the 10-fold CV (cv_rsq) and holdout CV (ho_rsq). The tibble is formatted to round to 2 decimal places and not display the leading zero using mutate(). The final tibble is saved as a csv named table1.csv under the out subdirectory of the project 
table1_tbl <- tibble(
  algo = c("OLS", "Elastic Net", "Random Forest", "XGBoost"),
  cv_rsq = as.numeric(cv_r2_values),
  ho_rsq = c(ho_r2_ols, ho_r2_glm, ho_r2_rf, ho_r2_xgb)
) %>%
  mutate(across(c(cv_rsq, ho_rsq), 
                ~ str_remove(formatC(., digits = 2, format = "f"), "^0")))
write_csv(table1_tbl, "../out/table1.csv")

# 1. How did your results change between models? Why do you think this happened, specifically?
# The results changed between the models due to the different assumptions that each model has in terms of how work hours related to predictors (other variables). OLS and Glmnet models are linear models, so they are assumpting fixed linear relationships, while the random forest and gradient boosting models are non-linear tree based models, and can additionally find interactions in the data. There is also a different in the bias-variance tradeoff betweeen the models. In this case, the OLS model did significantly worse than the other models, with the gradient boosting model having the highest R2 values for both the training and holdout CVs. Specifically, this occurred due to the underlying strucures in the data and how each model handles them, and the gradient boosting model performing the best suggests that the data have non-linear and more complex relationships that the simple OLS model could not capture.
# 2. How did you results change between k-fold CV and holdout CV? Why do you think this happened, specifically?
# The results between the k-fold CV and holdout CV were fairly similar across all of the models, with the OLS, elastic net and gradient boost models having lower CVs in the holdout compared to the k-fold. This is because the holdout data used to calculate the holdout CVs was different than the training data that the models were optimized for when calculating the k-fold CVs. A lower R2 value in the holdout CVs could indicate overfitting, where the k-fold CVs is too optimized or calculated based on the training data and cannot generalize to make predictions on new data. 
# 3. Among the four models, which would you choose for a real-life prediction problem, and why? Are there tradeoffs? Write up to a paragraph.
# For a real-life prediction problem, I would use the XGBoost model out of the four because this model resulted in the highest R2 out of all of the models both in the k-fold and holdout CVs. Since the R2 was near zero for the simple OLS regression model, this means that the data more than likely have non-linear relationships, and so a model such as the XGBoost model would be the best to handle these complexities. However, there is a tradeoff with using this more complex model, as there are several hyperparameters to tune which makes running the model much more computationally expensive than a simple OLS or glmnet model. There may also be a bias-variance tradeoff, where boosting models are ideal for reducing bias, but for data that may have higher variance and lower bias, a bagging model might be more effective. 