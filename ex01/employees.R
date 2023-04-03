library(tidyverse)
font <- "Fira Sans Condensed"
theme_set(theme_bw(base_size = 20, base_family = font))

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
    bind_rows(.id = "company") |>
    # combine month and year to a single date column
    mutate(month = make_date(year = year, month = month)) |>
    select(-year)

# assign long names for plot labels
full_names <- list(american = "American Airlines",
                   delta = "Delta Airlines",
                   federal = "Federal Express",
                   united = "United Airlines")
job_type <- list(full_time = "Full-Time",
                 part_time = "Part-Time",
                 total = "Total")

# plot full-time, part-time and total employees as a function of time
empl_plots <- lapply(names(job_type), function(col) {
    employees |>
    ggplot(aes(x = month, y = .data[[col]], color = company)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = unname(palette.colors(4, "Okabe-Ito")),
                           labels = full_names) +
        labs(x = "Year", y = "Number of employees", color = "Company",
             title = paste(job_type[[col]], "Employees"))
})

# plot the fraction of part-time workers over the total employees
# as a function of time
ptime_frac <- employees |>
    mutate(part_time = part_time / total) |>
    ggplot(aes(x = month, y = part_time, color = company)) +
        geom_line(linewidth = 1) +
        scale_color_manual(values = unname(palette.colors(4, "Okabe-Ito")),
                           labels = full_names) +
        labs(x = "Year", y = "Fraction of part-time employees",
             color = "Company",
             title = "Fraction of Part-Time Employees Through the Years")
