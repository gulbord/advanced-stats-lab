library(rvest)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nord)
library(countrycode)

men100m_html <- read_html("http://www.alltime-athletics.com/m_100ok.htm")
men100m_list <- men100m_html |>
    html_nodes(xpath = "//pre") |>
    html_text()
# read_fwf: read fixed-width file
men100m_tbl <- read_fwf(men100m_list, show_col_types = FALSE) |>
    select(-1) |>
    setNames(c("timing", "wind_speed", "athlete", "country",
               "birth_date", "position", "city", "date")) |>
    na.exclude()

# timings with A are affected by altitude, add another column to keep info
men100m_tbl <- men100m_tbl |>
    mutate(altitude = grepl(".*A$", timing)) |>
    mutate(timing = as.numeric(gsub("A", "", timing)))

# tidy up wind speeds
men100m_tbl <- men100m_tbl |>
    mutate(wind_speed = ifelse(wind_speed == "±0.0", 0, wind_speed)) |>
    mutate_at(c("wind_speed"), as.numeric)

# convert birth date to proper format
men100m_tbl$birth_date <- as.Date(men100m_tbl$birth_date,
                                  format = "%d.%m.%y")
# also date of race
men100m_tbl$date <- as.Date(men100m_tbl$date, format = "%d.%m.%Y")

# get fastest runner each year + name and country
timings <- men100m_tbl |>
    mutate(year = as.numeric(format(date, "%Y"))) |>
    group_by(year) |>
    summarise(fastest = min(timing),
              athlete = athlete[which.min(timing)],
              country = country[which.min(timing)])

# add flags (.svg files, circular flags)
github <- "https://raw.githubusercontent.com/"
flag_repo <- paste0(github, "HatScripts/circle-flags/gh-pages/flags/")
timings <- timings |>
    mutate(country = countrycode(country, "ioc", "iso2c"),
           flag = paste0(flag_repo, tolower(country), ".svg"))

font <- "Fira Sans Condensed"
theme_set(theme_light(base_size = 20, base_family = font))
# plotting
p1 <- ggplot(data = timings, aes(x = year, y = fastest)) +
    # running average line (loess smoothing)
    stat_smooth(colour = "sienna4", fill = "sienna3", alpha = 0.3) +
    # data
    geom_point(colour = "gray35", size = 2.5) +
    # labels for some outliers
    geom_text_repel(aes(label = ifelse(fastest < 9.71, athlete, "")),
                    size = 5.5, box.padding = 0.5, nudge_y = -0.01,
                    family = font, colour = "gray35") +
    # flags over data points
    geom_image(aes(image = flag), size = 0.02, asp = 1.6) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
    labs(x = "Year", y = "Fastest time (s)",
         title = "Fastest man in the 100 m through the years",
         caption = "Data: www.alltime-athletics.com") +
    theme(plot.title = element_text(size = 28, face = "bold"))
