library(tidyverse)

# read data and import to tibble
names <- c("month", "year", "full_time", "part_time", "total")
american <- read_table("data/american_airline_empl.txt",
                       skip = 1, col_names = names)
delta <- read_table("data/delta_airline_empl.txt",
                    skip = 1, col_names = names)
federal <- read_table("data/federal_express_empl.txt",
                      skip = 1, col_names = names)
united <- read_table("data/united_airline_empl.txt",
                      skip = 1, col_names = names)

# merge the four tibbles [https://stackoverflow.com/a/41620524]
employees <- list(american = american, delta = delta,
                  federal = federal, united = united) |>
    bind_rows(.id = "company")

# colours from each company’s palette
comp_cols <- list(american = c("#0078d2", "#c30019"),
                  delta = c("#e3132c", "#9b1631"),
                  federal = c("#4d148c", "#ff6600"),
                  united = c("#0033ab", "#00244d"))
comp_names <- names(comp_cols)

empl_plots <- vector(length = 4)
for (i in 1:4) {
    empl_plots <- employees |>
        filter(company == comp_names[i])
        gather("type", "number", c("full_time", "part_time"))
}