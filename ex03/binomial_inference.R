# successes
y <- 7
# trials
n <- 20
# probability vector
dp <- 1 / 2000
p <- seq(0, 1, by = dp)

stepf <- function(x) {
   ifelse(x <= 0.2, x,
          ifelse(x <= 0.3, 0.2,
                 ifelse(x <= 0.5, 0.5 - x, 0)))
}

post_step_num <- function(x) {
    stepf(x) * dbinom(y, n, x)
}

post_step <- post_step_num(p) / integrate(post_step_num, 0, 1)$value
post_unif <- dbeta(p, 1 + y, 1 + n - y)
post_jeff <- dbeta(p, 0.5 + y, 0.5 + n - y)
