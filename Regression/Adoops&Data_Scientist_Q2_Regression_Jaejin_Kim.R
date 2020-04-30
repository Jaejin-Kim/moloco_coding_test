require(tidyverse)
require(scatterplot3d)
require(glmnet)
require(broom)
require(car)

dat <- read_csv("Adops & Data Scientist Sample Data - Q2 Regression.csv", col_names = c("A", "B", "C"))

# Descriptive statistics and assumption check
summary(dat)

## Correlation
cor(dat$A, dat$B) # independent variables seem to be not correlated

## Scatterplot
ggplot(dat, aes(A,C)) + geom_point() + geom_text(aes(label=as.numeric(rownames(dat))))
ggplot(dat, aes(B,C)) + geom_point() + geom_text(aes(label=as.numeric(rownames(dat))))
                  # We can see that there is an outlier, on row 201.

dat2 <- dat[-201,]
ggplot(dat2, aes(A,C)) + geom_point() + geom_text(aes(label=as.numeric(rownames(dat2))))
ggplot(dat2, aes(B,C)) + geom_point() + geom_text(aes(label=as.numeric(rownames(dat2))))
                  # After deleting the outlier, we can see that B somewhat has a linear relationship
                  # with C, and as the absolute value of A increases, the variance of C increases.

scatterplot3d(dat2, type = "p", highlight.3d = T, angle = 50)
                  # From the 3d plot, we can see that A seems to have a cubic relationship with C,
                  # and there maybe interaction between A^3 and B.

# Baseline OLS regression model
## Feature generation
dat2$A2 <- dat2$A^2
dat2$A3 <- dat2$A^3
dat2$A3B <- dat2$A3 * dat2$B
dat2$id <- 1:nrow(dat2)

## Train-test split
set.seed(1234) # set seed for replication
train <- dat2 %>% sample_frac(.75) # train-test split (75% train set)
test <- anti_join(dat2, train, by = 'id') # train-test split (25% test set)

## Model fitting and evaluaation
mod1 <- lm(C~ A+B+A2+A3*B, data = train)

lr_pred <- predict(mod1, test[-3])
lm_mse <- mean((test$C- lr_pred)^2)


summary(mod1)
lm_mse
                  # The model has a very high adjusted R-squared in 0.9343.
                  # All the coefficients are statistically significant.
                  # Testing MSE is 322.2111.
                  # There might be the problem of overfitting,
                  # therefore I proceed to fit a LASSO regularized model.

# Lasso regression model building
preds = model.matrix(C ~ A + B + A2 + A3 + A3B, dat2)[,-1] # predictors
resp = dat2 %>% select(C) %>% unlist() %>% as.numeric() # response variable

## Test-train split
lasso_train <- dat2 %>% sample_frac(.75) # train-test split (75% train set)
lasso_test <- anti_join(dat2, lasso_train, by = 'id') # train-test split (25% test set)

preds_train <- model.matrix(C ~ A + B + A2 + A3 + A3B, lasso_train)[,-1]
preds_test <- model.matrix(C ~ A + B + A2 + A3 + A3B, lasso_test)[,-1]

resp_train <- lasso_train %>% select(C) %>% unlist() %>% as.numeric()
resp_test <- lasso_test %>% select(C) %>% unlist() %>% as.numeric()

## Finding optimal lambda value
grid = 10^seq(10, -2, length = 100)
lasso_mod <- glmnet(preds_train, resp_train, alpha = 1, lambda = grid)
plot(lasso_mod) # plot coefficients
          # We can see that some of the coefficients are forced to zero.

cv.out <- cv.glmnet(preds_train, resp_train, alpha = 1)
plot(cv.out) # plot training MSE as a function of lambda
bestlam <- cv.out$lambda.min # select lambda that minimizes training MSE

## Prediction and evaluation using the best lambda
lasso_pred <- predict(lasso_mod, s = bestlam, newx = preds_test)
lasso_coef <- predict(lasso_mod, type = "coefficients", s = bestlam)
lasso_coef
lasso_mse <- mean((lasso_pred - resp_test)^2) # MSE
lasso_mse
rsq_lasso_cv <- cor(resp_test, lasso_pred)^2
rsq_lasso_cv # R-squared
                  # The LASSO regularization forced zero coefficients on A^2.
                  # MSE is larger compared to the OlS model.
                  # R-Squared is also lower compared to the OLS model.

# Conclusion
## It seemed that the baseline OLS model overfitted the data, with an unusually high R-squared.
## LASSO regularized model on the other hand, forced zero coefficients on A^2
## and showed slightly lower performance, but still high enough R-squared in 0.9296883.
## It's MSE is also only slightly higher than that of the OLSs model.
## Therefore, for the sake of not-overfitting, I would choose the LASSO regularized model,