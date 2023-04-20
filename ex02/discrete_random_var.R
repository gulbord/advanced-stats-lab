library(ggplot2)
library(wesanderson)
font <- "Roboto Condensed"
theme_set(theme_minimal(base_size = 14, base_family = font))
my_pal <- wes_palette("FantasticFox1")

# probability density function
# p(k) = k / 15 with k = 1, ..., 5
dunif5 <- function(x) {
    ifelse(x %in% 1:5, x / 15, 0)
}

# cumulative distribution function
punif5 <- function(x) {
    ifelse(x > 4, 1, ifelse(x < 1, 0, x * (x + 1) / 30))
}

# plotting the pdf
x <- 0:6
ggplot() +
    geom_col(aes(x, dunif5(x)), fill = my_pal[1]) +
    scale_x_continuous(breaks = x) +
    labs(x = "x", y = "f(x)", title = "Probability density function")

# plotting the cdf
x <- 0:8
ggplot() +
    geom_col(aes(x, punif5(x)), fill = my_pal[1]) +
    scale_x_continuous(breaks = x) +
    labs(x = "x", y = "F(x)", title = "Cumulative distribution function")

# mean value and variance
x <- 1:5
munif5 <- sum(x * sapply(x, dunif5))
vunif5 <- sum((x - munif5)^2 * sapply(x, dunif5))

# exp val of k(6 - k)
expval <- sum(x * (6 - x) * sapply(x, dunif5))

# sample from the distribution
runif5 <- function(n) {
    # sample from uniform first
    us <- runif(n)
}