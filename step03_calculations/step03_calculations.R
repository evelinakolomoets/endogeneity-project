setwd("~/Documents/GitHub/endogeneity-project/")

df <- read.csv("step01_data/workinghours_df.csv")

# удаляем переменную, которая принимает единственное значение
df <- df[, -which(names(df) %in% c("aaj11"))]

cat_names <- c(
  "aaj4.1", "aaj21.3", "aaj125.2", "aah5",
  "aaj6", "aaj11.1",
  "aaj23", "aaj26", "aaj72.171", "aaj31",
  "aaj66.1", "aaj62", "aaj66", "aaj61", "aaj14", "aaj81"
)

df[cat_names] <- lapply(df[cat_names], as.factor)
df[, 
   -which(names(df) %in% cat_names)] <- lapply(df[-which(names(df) 
                                                         %in% cat_names)], 
                                               as.numeric)

X <- df[, -which(names(df) %in% c("aaj6.2"))]
y <- df$aaj6.2

### МОДЕЛИ ###
library(lmtest)
library(sandwich)

# МНК
ols <- lm(y ~ ., data = X)
ols_summary <- summary(ols)
bptest(ols)$p.value > 0.05
ols_test <- coeftest(ols, vcov = vcovHC(ols, type = 'HC0'))

# МНК БЕЗ НЕКОТОРЫХ ПРИЗНАКОВ - ОТОБРАНЫ ПО КРИТЕРИЮ АКАИКЕ
step_ols <- step(lm(y ~ ., data = X))
step_ols_summary <- summary(step_ols)
bptest(step_ols)$p.value > 0.05

step_ols_test <- coeftest(step_ols, vcov = vcovHC(step_ols, type = 'HC0'))

# ПРЕОБРАЗОВАНИЕ В МАТРИЧНЫЙ ВИД
X_with_dummies <- X[, -which(names(X) %in% cat_names)]
X_with_dummies[] <- lapply(X_with_dummies, as.numeric)

# преобразование factor колонок в OHE
for (cat_col in cat_names) {
  level_names <- levels(X[[cat_col]])
  dummy_matrix <- model.matrix(~ X[[cat_col]])[,2:length(level_names)]
  
  initial_ncol <- ncol(X_with_dummies)
  X_with_dummies <- cbind(X_with_dummies, dummy_matrix)
  
  colnames(X_with_dummies)[
    (initial_ncol + 1):ncol(X_with_dummies)
  ] <- paste(cat_col, 
             level_names[2:length(level_names)], 
             sep = "")
  
}

rm(cat_col, level_names, initial_ncol, dummy_matrix)

X_with_dummies <- cbind(intercept = 1, X_with_dummies)
X_with_dummies <- as.matrix(X_with_dummies)

# МНК БЕЗ НЕКОТОРЫХ ПРИЗНАКОВ - ОТОБРАНЫ С ПОМОЩЬЮ LASSO
library(glmnet)

lasso_model <- glmnet(X_with_dummies, y, alpha = 1)

cv_model <- cv.glmnet(X_with_dummies, y, alpha = 1)
best_lambda <- cv_model$lambda.min

coefficients <- coef(lasso_model, s = best_lambda)
selected_variables <- na.omit(
  colnames(X_with_dummies)[which(coefficients != 0)]
  )

X_selected <- X_with_dummies[, which(colnames(X_with_dummies) %in% selected_variables)]
X_selected <- X_selected[, -which(colnames(X_selected) %in% c("intercept"))]
ols_lasso <- lm(y ~ ., data = data.frame(X_selected))
summary(ols_lasso)
bptest(ols_lasso)$p.value > 0.05
ols_lasso_test <- coeftest(ols_lasso, vcov = vcovHC(ols_lasso, type = 'HC0'))

# ММП
library(maxLik)
y <- as.matrix(y)

likelihood <- function(param, x) {
  beta <- param[-1]
  sigma <- param[1]
  mu <- x %*% beta
  sum(dnorm(y, mu, sigma, log = TRUE))
}

# init_vars <- setNames(rep(0, ncol(X_with_dummies)), colnames(X_with_dummies))

ml <- maxLik(likelihood,
              start = c(sigma = sd(y),
                        ols$coefficients
              ),
              x = X_with_dummies,
              method = "CG"
)
ml_summary <- summary(ml)
ml_summary

# ЭКСПОРТ РЕЗУЛЬТАТОВ
library(stargazer)
stargazer(ols_test, step_ols_test, ols_lasso_test, 
          type = "latex",
          out = "models_summary.tex",
          digits = 3,
          title = "Models Summary",
          notes.align = 'r')

library(texreg)

htmlreg(ml, stars=c(0.01, 0.05, 0.1), file='ml.html')


stargazer(ols_test, step_ols_test, ols_lasso_test, 
          type = "html",
          out = "models_summary.html",
          digits = 3,
          title = "Models Summary",
          notes.align = 'r')