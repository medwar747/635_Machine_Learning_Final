library(tidyverse);
library(glmnet);
library(caret);

load("derived_data/train_cleaned.Rdata"); # "train"
train <- select(train,-Id);

########## Examine Which Predictors are Most Important ########## 

X <- model.matrix(SalePrice ~ ., train)[,-1];
y <- train$SalePrice;

grid <- 10^seq(5,3.4,length=100);

ridge.mod <- glmnet(X, y, alpha=1, lambda=grid);

my_colors <- rep("#000000", times = dim(X)[2]);
my_colors[c(93,145,146,187,202,205,212,213,239)] <- rainbow(9);

plot(log(ridge.mod$lambda), coef(ridge.mod)[2,]/sd(coef(ridge.mod)[2,]), type='l', col=2, ylim=c(-4,4),
     xlab='log(lambda)', ylab='Scaled coefficients',main="Lasso Regression Shrinkage Profile",
     cex.lab=1.4, cex.axis=1.3, cex.main=1.5)
for(i in 3:nrow(coef(ridge.mod))){
  points(log(ridge.mod$lambda), coef(ridge.mod)[i,]/sd(coef(ridge.mod)[i,]), type='l', col=my_colors[i]);
}

legend_labels <- colnames(X)[c(93,145,146,187,202,205,212,213,239)];
legend_labels <- c("Overall Condition of House (1-10)",
                   "Exterior Material Quality (unique contrast combination)",
                   "Exterior Material Quality (unique contrast combination)",
                   "Presence of Gas Forced Warm Air Furnace",
                   "Square Footage of Second Floor",
                   "Number of Full Basement Bathrooms",
                   "Kitchen Quality (unique contrast combination)",
                   "Kitchen Quality (unique contrast combination)",
                   "Square Footage of Garage"
                   );
legend_colors <- rainbow(9);
legend("bottomright", legend=legend_labels, col=legend_colors, lty=1, cex=1.25);

########## Cross-Validate Lambda for Optimal Performance ########## 
set.seed(730307019);
grid <- 10^seq(5,2.75,length=100);

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
    ridge.mod <- glmnet(X_temp_train, y_temp_train, alpha=1, lambda=grid[j]);
  
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

# OPTIMAL LAMBDA: 3162.278



########## Predict Test Set ##########

load("derived_data/test_cleaned.Rdata"); # "test"
submission <- read_csv("source_data/sample_submission.csv");

test <- select(test, -Id);

X_train <- model.matrix(SalePrice ~ ., train)[,-1];
y_train <- train$SalePrice;

ridge.mod <- glmnet(X_train, y_train, alpha=1, lambda=3162.278);

X_test <- model.matrix(~ ., test)[,-1];

submission$SalePrice <- as.numeric(predict(ridge.mod, X_test));
write_csv(submission,"derived_data/Lasso_submission.csv");