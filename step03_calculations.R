df <- read.csv('workinghours_df.csv')

# удаляем переменную, которая принимает единственное значение
df <- df[ , -which(names(df) %in% c('aaj11'))]

cat_names <- c('aaj4.1', 'aaj21.3', 'aaj125.2', 'aah5', 
              'aa_educ', 'aaj6', 'aaj11.1', 
              'aaj23', 'aaj26', 'aaj72.171', 'aaj31', 
              'aaj66.1', 'aaj62', 'aaj66', 'aaj61', 'aaj14', 'aaj81')

df[cat_names] <- lapply(df[cat_names], as.factor)
df[, -which(names(df) %in% cat_names)] <- lapply(df[-which(names(df) %in% cat_names)], as.numeric)

X <- df[ , -which(names(df) %in% c('aaj6.2'))]
y <- df$aaj6.2

### МОДЕЛИ ###

# МНК
ols <- lm(y ~ ., data=X)
ols_summary <- summary(ols)

# ММП
library(maxLik)

X_with_dummies <- X[, -which(names(df) %in% cat_names)]
X_with_dummies[] <- lapply(X_with_dummies, as.numeric)

# преобразование factor колонок в OHE
for (cat_col in cat_names){
  level_names <- levels(X[[cat_col]])
  dummy_matrix <- model.matrix(~ X[[cat_col]] - 1)
  
  initial_ncol <- ncol(X_with_dummies)
  X_with_dummies <- cbind(X_with_dummies, dummy_matrix)
  
  # Automatically assign names to dummy variables
  needed_colname <- paste(cat_col, '_', sep = "")
  colnames(X_with_dummies)[
    (initial_ncol + 1):ncol(X_with_dummies)
    ] <- paste(needed_colname, level_names, sep = "")
}


X_with_dummies <- cbind(intercept=1, X_with_dummies)
X_with_dummies <- as.matrix(X_with_dummies)

likelihood <- function(param) {
  beta <- param[-1]
  sigma <- param[1]
  mu <- X_with_dummies %*% beta
  sum(dnorm(y, mu, sigma, log = TRUE))
}   

ml <- maxLik(likelihood, 
             start = c(sigma=sd(y), 
                       rep(0, ncol(X_with_dummies))
                       # intercept=1,
                       # aaj4.1=0,
                       # aaj13.2=0,
                       # aaj21.3=0,
                       # aaj125.2=0,
                       # aah5=0,
                       # aa_age=0,
                       # aa_educ=0,
                       # aaj161.3y=0,
                       # aaj6=0,
                       # aaj6.0=0,
                       # aaj11.1=0,
                       # aaj13=0,
                       # aaj23=0,
                       # aaj26=0,
                       # aaj72.171=0,
                       # aaj31=0,
                       # aaj66.1=0,
                       # aaj62=0,
                       # aaj66=0,
                       # aaj61=0,
                       # aaj14=0,
                       # aaj15=0,
                       # aaj16=0,
                       # aaj81=0
                       ))

ml_summary <- summary(ml)
