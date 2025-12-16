library(plotly)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)

sleep_ola <- read.csv("dane/sleep_ola.csv", header = FALSE)

sleep_ola[sleep_ola == ""] <- NA

names(sleep_ola) <- as.character(unlist(sleep_ola[1, ]))
sleep_ola <- sleep_ola[-1, ]
sleep_ola <- sleep_ola[, colSums(is.na(sleep_ola)) < nrow(sleep_ola)]


sleep_ola <- sleep_ola %>%
  rename(start_time = com.samsung.health.sleep.start_time,
         end_time = com.samsung.health.sleep.end_time) %>%
  select(start_time, end_time) %>%
  mutate(
    start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
    date = as.Date(start_time),
    went_to_bed = format(start_time, "%H:%M:%OS"),
    woke_up = format(end_time, "%H:%M:%OS"),
    year = format(start_time, "%Y"),
    month = format(start_time, "%m"),
    duration = as.numeric(end_time - start_time)) %>%
  filter(year == 2025) %>% 
  distinct(date, .keep_all = TRUE) %>% 
  complete(date = seq.Date(min(date), max(date), by = "day")) %>% 
  mutate(
    duration = na.approx(duration, na.rm = FALSE,),
    day = weekdays(date))

plot_ly(
  data = sleep_ola, 
  x = ~duration,
  frame = ~day,
  type = "violin") %>%
  animation_slider(
    currentvalue = list(prefix = "Wybrany dzień: ")
  ) %>%
  animation_opts(
    redraw = FALSE
  )

