library(tidyverse);
library(glmnet);
library(caret);

load("derived_data/train_cleaned.Rdata"); # "train"
train <- select(train,-Id);

########## Examine Which Predictors are Most Important ########## 

X <- model.matrix(SalePrice ~ ., train)[,-1];
y <- train$SalePrice;

grid <- 10^seq(10,4,length=100);

ridge.mod <- glmnet(X, y, alpha=0, lambda=grid);

plot(log(ridge.mod$lambda), coef(ridge.mod)[2,]/sd(coef(ridge.mod)[2,]), type='l', col=2, ylim=c(-4,4), xlab='log(lambda)', ylab='Scaled coefficients')
for(i in 3:nrow(coef(ridge.mod))){
  points(log(ridge.mod$lambda), coef(ridge.mod)[i,]/sd(coef(ridge.mod)[i,]), type='l', col=i)
}



########## Cross-Validate Lambda for Optimal Performance ########## 
set.seed(730307019);

folds <- createFolds(train$SalePrice, k=5, list=FALSE);
training_error <- matrix(nrow=5,ncol=length(grid));
testing_error <- matrix(nrow=5,ncol=length(grid));

X_train <- model.matrix(SalePrice ~ ., train)[,-1];

for (i in seq(1,5)) {
  
  X_temp_train <- X_train[!(folds==i),];
  y_temp_train <- train$SalePrice[!(folds==i)];
  
  X_temp_test <- X_train[(folds==i),];
  y_temp_test <- train$SalePrice[(folds==i)];
  
  for (j in seq(1,length(grid))) {
    ridge.mod <- glmnet(X_temp_train, y_temp_train, alpha=0, lambda=grid[j]);
  
    training_predictions <- predict(ridge.mod, X_temp_train);
    training_error[i,j] <- sqrt(sum((log(training_predictions)-log(y_temp_train))^2)/dim(X_temp_train)[1]);
  
    testing_predictions <- predict(ridge.mod, X_temp_test);
    testing_error[i,j] <- sqrt(sum((log(testing_predictions)-log(y_temp_test))^2)/dim(X_temp_train)[1]);
  
  }
  
}

ggplot(, aes(x = log(grid))) +
  geom_line(aes(y = apply(training_error, 2, mean), color = "Training Error")) +
  geom_line(aes(y = apply(testing_error, 2, mean), color = "Testing Error")) +
  scale_color_manual(values = c("Training Error" = "orange", "Testing Error" = "blue")) +
  labs(title = "K=5 Cross-Validation of Lambda for Optimal Competition Score using Ridge Regression",
       x = "log(lambda)",
       y = "Error") +
  theme_minimal()

# OPTIMAL LAMBDA: 93260.33



########## Predict Test Set ##########

load("derived_data/test_cleaned.Rdata"); # "test"
submission <- read_csv("source_data/sample_submission.csv");

test <- select(test, -Id);

X_train <- model.matrix(SalePrice ~ ., train)[,-1];
y_train <- train$SalePrice;

ridge.mod <- glmnet(X_train, y_train, alpha=0, lambda=93260.33);

X_test <- model.matrix(~ ., test)[,-1];

submission$SalePrice <- as.numeric(predict(ridge.mod, X_test));
write_csv(submission,"derived_data/Ridge_submission.csv");