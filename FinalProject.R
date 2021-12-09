library("here")
library("tidyverse")
library("forcats")
library("rsample")
library("glmnet")
library("glmnetUtils")
library("ggplot2")
library("dplyr")
library('sjPlot')

wineQuality <- read.csv(here::here("datasets", "winequality.csv"))

summary(wineQuality)

# lasso regression
lasso_mod <- cv.glmnet(quality ~ .,
                       data = wineQuality,
                       alpha = 1)

print(lasso_mod$lambda.min)
print(lasso_mod$lambda.1se)
sqrt(lasso_mod$cvm[lasso_mod$lambda == lasso_mod$lambda.1se])

# summary linear regression
mod1 <- lm(formula = quality ~., data = wineQuality)
summary(mod1)
tidy(mod1)
tab_model(mod1)
linear_model <- plot_model(mod1)

wine_split <- initial_split(wineQuality, prop = 0.75)
wine_train <- training(wineQuality)
wine_test <- testing(wineQuality)
get_rmse <- function(true, predictions){
  sqrt(mean((true - predictions)^2))
}
get_rmse(wine_train$quality, preds_train)
get_rmse(wine_test$quality, preds_test)

get_mae(wine_train$quality, preds_train)
get_mae(wine_test$quality, preds_test)

plot(wine_train)

# lasso model
lasso_coefs <- data.frame(
  `lasso_min` = coef(lasso_mod, s = lasso_mod$lambda.min) %>%
    round(3) %>% as.matrix() %>% as.data.frame(),
  `lasso_lse` = coef(lasso_mod, s = lasso_mod$lambda.1se) %>%
    round(3) %>% as.matrix() %>% as.data.frame()
) %>% rename(`lasso_min` = 1, `lasso_1se` =2)
print(lasso_coefs)

plot(lasso_mod)

#best
lambda_best <- lasso_mod$lambda.min
lambda_best

# training model on best value
lambdas <- 10^seq(2,-3, by = -.1)
predictions_train <- predict(lasso_mod, s = lambda_best, newx = wine_train)


lambda1se(lasso_mod)

# elasticNet
wine_split <- initial_split(wineQuality, prop = 0.7)
wine_train <- training(wine_split)
wine_test <- testing(wine_split)


enet_mod <- cva.glmnet(quality ~.,
                       data = wine_train,
                       alpha = seq(0,1,by = 0.05))


plot(enet_mod)
minlossplot(enet_mod,
            cv.type = "min")


get_alpha <- function(fit) {
  alpha <- fit $alpha
  error <- sapply(fit$modlist,
                  function(mod) {min(mod$cvm)})
  alpha[which.min(error)]
}

get_model_params <- function(fit){ 
  alpha <- fit$alpha
  lambdaMin <- sapply(fit$modlist, `[[`, "lambda.min")
  lambdaSE <- sapply(fit$modlist, `[[`, "lambda.1se")
  error <- sapply(fit$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(error)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambdaSE = lambdaSE[best], error = error[best])
}


get_model_params(enet_mod)

# extract the best model object
best_mod <- enet_mod$modlist[[which(enet_mod$alpha == best_alpha)]]
best_alpha <- get_alpha(enet_mod)
print(best_alpha)


optimal_lambda <- best_mod$lambda.min
print(optimal_lambda)


preds_train <- predict(enet_mod, newdata = wine_train, s - 'lambda.min', which = 3)
preds_test <- predict(enet_mod, newdata = wine_test, s - 'lambda.min', which = 3)

get_rmse(wine_train$quality, preds_train)
get_rmse(wine_test$quality, preds_test)

get_mae(wine_train$quality, preds_train)
get_mae(wine_test$quality, preds_test)






