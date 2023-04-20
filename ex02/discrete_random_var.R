library(ggplot2)
font <- "Roboto Condensed"
theme_set(theme_minimal(base_size = 14, base_family = font))
my_pal <- unname(palette.colors(4, "Okabe-Ito"))

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
    geom_col(aes(x = x, y = dunif5(x)), fill = my_pal[2]) +
    scale_x_continuous(breaks = x) +
    labs(x = "x", y = "f(x)", title = "Probability density function")

# plotting the cdf
x <- 0:8
ggplot() +
    geom_col(aes(x = x, y = punif5(x)), fill = my_pal[2]) +
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
    x <- 1:5
    sapply(runif(n), function(r) x[which(sapply(x, punif5) > r)[1]])
}

# sample 1e5 numbers and plot them alongside dunif5
sample <- runif5(1e5)
x <- 1:5
ggplot() +
    geom_histogram(aes(x = sample,
                       y = after_stat(count / sum(count)),
                       fill = "hist"),
                   binwidth = 1) +
    geom_point(aes(x = x,
                   y = sapply(x, dunif5),
                   colour = "pdf"),
              size = 4) +
    geom_line(aes(x = x,
                  y = sapply(x, dunif5),
                  colour = "pdf"),
              linewidth = 1) +
    scale_fill_manual(values = my_pal[2], labels = "Simulated data") +
    scale_colour_manual(values = my_pal[1], labels = "PDF") +
    labs(x = "x", y = "f(x)", colour = NULL, fill = NULL)
