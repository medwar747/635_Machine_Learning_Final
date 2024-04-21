# Packages
library(themis);
library(recipes);
library(rsample);
library(collapse);
library(lightgbm);
library(tidyverse);

########## Data Prep ##########
load("derived_data/train_cleaned.Rdata"); # "train"
train <- train %>%
  dplyr::select(-Id) %>%
  mutate(SalePrice=log(SalePrice));
load("derived_data/test_cleaned.Rdata"); # "test"
test <- dplyr::select(test,-Id);

X_train <- as_tibble(model.matrix(SalePrice ~ ., train)[,-1]);
X_train$SalePrice <- train$SalePrice;

X_test <- as_tibble(model.matrix(~ ., test)[,-1]);

########## Training ##########
nbags <- 3;
nfolds <- 7;
pred <- numeric(nrow(test));

set.seed(730307019);

for (split in vfold_cv(X_train, nfolds)$splits) {
  cat("Fold id:", split$id$id, "\n");
  traini <- split$in_id; 

  X_val <- X_train[-traini, ];
  y_val <- as.numeric(X_val$SalePrice);
  X_val <- dplyr::select(X_val, -SalePrice) %>% data.matrix()
  
  p <- numeric(nrow(X_test))
  
  for (seed in seq(nbags)) {
    set.seed(seed)
    
    X <- X_train[traini, ]; 
    y <- as.numeric(X$SalePrice)
    X <- dplyr::select(X, -SalePrice) %>% data.matrix()
    
    m_lgb <- lgb.train(params = list(objective = "regression",  # Regression objective
                                     metric = "rmse",  # Root Mean squared error
                                     nthread = 4), 
                       data = lgb.Dataset(X, label = y),
                       valids = list(val = lgb.Dataset(X_val, label = y_val)),
                       verbose = -1)
    
    p <- p + predict(m_lgb, as.matrix(X_test)) / nbags
  }
  
  pred <- pred + p / nfolds
}

final_pred <- exp(pred);

########## Submission ##########

submission <- read_csv("source_data/sample_submission.csv");

submission$SalePrice <- as.numeric(final_pred);

write_csv(submission,"derived_data/LightGBM_default_submission.csv");


