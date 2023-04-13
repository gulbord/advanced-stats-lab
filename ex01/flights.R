library(kableExtra)
library(tidyverse)
library(nycflights13)

font <- "Roboto Condensed"
theme_set(theme_minimal(base_size = 14, base_family = font))
my_palette <- rcartocolor::carto_pal(10, "Safe")[1:3]

flights$date <- as_date(flights$time_hour, tz = "EST")

flights |>
    count(date, origin) |>
    ggplot(aes(x = date, y = n, colour = origin)) +
        geom_point(size = 0.8) +
        geom_smooth(aes(fill = origin), lwd = 0.8) +
        scale_colour_manual(values = my_palette) +
        scale_fill_manual(values = my_palette, guide = "none") +
        labs(x = "Date", y = "Number of flights", colour = "Airport") +
        guides(colour = guide_legend(override.aes = list(fill = NA)))

flights |>
    mutate(day_type = ifelse(wday(date) %in% c(1, 7),
                             "weekend", "workday")) |>
    # count by day, keeping info on week and day_type
    count(week = week(date), day, day_type) |>
    # regroup by week and day_type to get the averages we want
    group_by(week, day_type) |>
    summarize(avg = mean(n)) |>
    ggplot(aes(x = week, y = avg, colour = day_type)) +
        geom_line(lwd = 0.8) +
        scale_colour_manual(values = my_palette,
                            labels = c("Weekends", "Workdays")) +
        labs(x = "Week", y = "Average flights per day",
             colour = "Day type")

by_week <- flights |>
    dplyr::filter(wday(date) %in% 2:6) |>
    count(week = week(date))

ind_day <- week("2013-07-04")
thnxgiv <- week("2013-11-28")

by_week[(ind_day - 2):(ind_day + 2), ] |>
    kbl(caption = "Flights in the workweeks around July 4th",
        table.attr = "style='width:50%;'") |>
    kable_styling(full_width = TRUE, position = "center")
by_week[(thnxgiv - 2):(thnxgiv + 2), ] |>
    kbl(caption = "Flights in the workweeks around Thanksgiving",
        table.attr = "style='width:50%;'") |>
    kable_styling(full_width = TRUE, position = "center")

dep_delays <- flights |>
    # some flights don’t have a recorded departure and/or arrival time
    drop_na(dep_delay) |>
    group_by(date, origin) |>
    summarize(avg = mean(dep_delay),
              min = min(dep_delay),
              max = max(dep_delay))

plot_delays <- function(key) {
    names <- c(
        "Maximum" = "max",
        "Minimum" = "min",
        "Average" = "avg"
    )

    ggplot(dep_delays, aes(x = date, y = .data[[names[key]]],
                           colour = origin)) +
        geom_point(size = 0.8) +
        geom_smooth(aes(fill = origin), lwd = 0.8) +
        scale_colour_manual(values = my_palette) +
        scale_fill_manual(values = my_palette, guide = "none") +
        labs(x = "Date", y = paste(key, "departure delay (min)"),
             colour = "Airport") +
        guides(colour = guide_legend(override.aes = list(fill = NA)))
}

plot_delays("Average")
plot_delays("Maximum")
plot_delays("Minimum")

# average speed
flights |>
    drop_na(air_time) |>
    # distances are in miles, air_time is in minutes
    mutate(speed = distance * 1.60934 * 60 / air_time) |>
    group_by(date) |>
    summarize(avg = mean(speed), sd = sd(speed)) |>
    ggplot(aes(x = date, y = avg)) +
        geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd),
                    fill = "#627bb4", alpha = 0.5,
                    colour = "transparent") +
        geom_line(colour = "#4a417b", lwd = 0.8) +
        labs(x = "Date", y = "Average speed during the day (km/h)")

flights |>
    count(date, carrier) |>
    slice_max(n = 2, by = date, order_by = n)

flights |>
    count(week = week(date), carrier) |>
    slice_max(n = 2, by = week, order_by = n)

flights |>
    count(month, carrier) |>
    slice_min(by = month, order_by = n)

flights |>
    group_by(month) |>
    summarize(carrier = carrier[which.max(distance)],
              max_dist = max(distance))
