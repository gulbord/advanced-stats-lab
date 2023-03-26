library(rvest)
library(tidyverse)

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