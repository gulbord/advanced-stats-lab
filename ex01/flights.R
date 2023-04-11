library(tidyverse)
library(nycflights13)

font <- "Lato"
theme_set(theme_minimal(base_size = 14, base_family = font))
my_palette <- unname(palette.colors(4, "Okabe-Ito"))

flights |>
    filter(origin %in% c("JFK", "LGA", "EWR")) |>
    mutate(date = make_date(year, month, day)) |>
    count(date, origin) |>
    ggplot(aes(x = date, y = n, colour = origin)) +
        geom_point(size = 0.8) +
        geom_smooth(aes(fill = origin), linewidth = 0.8) +
        scale_colour_manual(values = my_palette) +
        scale_fill_manual(values = my_palette, guide = "none") +
        labs(x = "Date", y = "Number of flights", colour = "Airport") +
        guides(colour = guide_legend(override.aes = list(fill = NA))
