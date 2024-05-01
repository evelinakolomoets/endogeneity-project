df <- read.csv('workinghours_df.csv')

# удаляем переменную, которая принимает единственное значение
df <- df[ , -which(names(df) %in% c('aaj11'))]

X <- df[ , -which(names(df) %in% c('aaj6.2'))]
y <- df$aaj6.2

### МОДЕЛИ ###

# МНК
ols <- lm(y ~ ., data=X)
ols_summary <- summary(ols)

# ММП
library(maxLik)

X_matrix <- as.matrix(cbind(intercept=1, X))

likelihood <- function(param) {
  beta <- param[-1] #Regression Coefficients
  sigma <- param[1] #Standard Deviation
  mu <- X_matrix %*% beta #multiply matrices
  sum(dnorm(y, mu, sigma, log = TRUE))
}   

ml <- maxLik(likelihood, 
             start = c(sigma=sd(y), 
                       #rep(0, ncol(X_matrix))
                       intercept=1,
                       aaj4.1=0,
                       aaj13.2=0,
                       aaj21.3=0,
                       aaj125.2=0,
                       aah5=0,
                       aa_age=0,
                       aa_educ=0,
                       aaj161.3y=0,
                       aaj6=0,
                       aaj6.0=0,
                       aaj11.1=0,
                       aaj13=0,
                       aaj23=0,
                       aaj26=0,
                       aaj72.171=0,
                       aaj31=0,
                       aaj66.1=0,
                       aaj62=0,
                       aaj66=0,
                       aaj61=0,
                       aaj14=0,
                       aaj15=0,
                       aaj16=0,
                       aaj81=0
                       ))

ml_summary <- summary(ml)
