y <- 7  # number of successes
n <- 20 # number of trials

stepf <- function(x) {
   ifelse(x <= 0.2, x,
          ifelse(x <= 0.3, 0.2,
                 ifelse(x <= 0.5, 0.5 - x, 0)))
}

alph_unif <- 1 + y
beta_unif <- 1 + n - y
alph_jeff <- 0.5 + y
beta_jeff <- 0.5 + n - y

post_unif <- function(p) dbeta(p, alph_unif, beta_unif)
post_jeff <- function(p) dbeta(p, alph_jeff, beta_jeff)
post_step <- function(p) {
    post_step_num <- function(x) stepf(x) * dbinom(y, n, x)
    post_step_num(p) / integrate(post_step_num, 0, 1)$value
}

my_pal <- c("#a4c5ee", "#395262", "#dec54a", "#ff7373")

# probability vector
dp <- 1 / 2000
p <- seq(0, 1, by = dp)

par(family = "Lato", cex = 1.2, bty = "l")
plot(p, post_unif(p), type = "l",
     xlab = "Probability", ylab = "Posterior distribution")
