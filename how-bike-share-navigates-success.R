# Cyclistic Bike-Share Case Study Data Prep for Tableau
# ======================================================
# Goal: Compare member and casual rider behavior to support marketing strategy

# Load required libraries
library(tidyverse)
library(lubridate)
library(readr)
library(janitor)
library(ggplot2)
library(leaflet)

# Load and combine monthly files
files <- list.files("C:/Users/nkc06/OneDrive/Desktop/Documents/", pattern = "*divvy-tripdata.csv", full.names = TRUE)

combined_data_df_cleaned <- files %>%
  map(read_csv) %>%
  keep(~"started_at" %in% names(.)) %>%
  map_dfr(~ .x %>%
            mutate(
              started_at = parse_date_time(started_at, orders = c("ymd HMS", "mdy HMS", "mdY HMS", "Ymd HMS")),
              ended_at = parse_date_time(ended_at, orders = c("ymd HMS", "mdy HMS", "mdY HMS", "Ymd HMS")),
              ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
              day_of_week = weekdays(as.Date(started_at)),
              hour_of_day = hour(started_at),
              season = case_when(
                month(started_at) %in% c(12, 1, 2) ~ "Winter",
                month(started_at) %in% c(3, 4, 5) ~ "Spring",
                month(started_at) %in% c(6, 7, 8) ~ "Summer",
                TRUE ~ "Fall"
              )
            )
  ) %>%
  filter(!is.na(ride_length) & ride_length >= 3 & ride_length <= 1440) %>%
  mutate(
    member_casual = as.factor(member_casual),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    ride_date = as.Date(started_at),
    week_start = floor_date(ride_date, unit = "week")
  )

saveRDS(combined_data_df_cleaned, file = "C:/Users/nkc06/OneDrive/Desktop/learningR/combined_data_df_cleaned.rds")

# Summary function for time-based analysis
create_time_summary <- function(df, time_unit = "week", export_name = NULL) {
  summary_df <- df %>%
    mutate(period = floor_date(started_at, unit = time_unit)) %>%
    group_by(period, member_casual) %>%
    summarise(
      total_rides = n(),
      avg_ride_length = round(mean(ride_length, na.rm = TRUE), 2),
      .groups = "drop"
    )
  if (!is.null(export_name)) write_csv(summary_df, paste0(export_name, ".csv"))
  return(summary_df)
}

# Seasonal summary
create_seasonal_summary <- function(df, export_name = NULL) {
  df %>%
    group_by(season, member_casual) %>%
    summarise(
      total_rides = n(),
      avg_ride_length = round(mean(ride_length, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    { if (!is.null(export_name)) write_csv(., paste0(export_name, ".csv")); . }
}

# Hourly breakdown by date
create_hourly_by_date_summary <- function(df, export_name = NULL) {
  summary_df <- df %>%
    filter(!is.na(ride_date)) %>%
    group_by(ride_date, season, hour_of_day, member_casual) %>%
    summarise(
      total_rides = n(),
      avg_ride_length = round(mean(ride_length, na.rm = TRUE), 2),
      .groups = "drop"
    )
  if (!is.null(export_name)) write_csv(summary_df, paste0(export_name, ".csv"))
  return(summary_df)
}

# Weekly day-of-week summary
create_dayofweek_summary <- function(df, export_name = NULL) {
  df %>%
    group_by(week_start, day_of_week, member_casual) %>%
    summarise(total_rides = n(), avg_ride_length = round(mean(ride_length, na.rm = TRUE), 2), .groups = "drop") %>%
    { if (!is.null(export_name)) write_csv(., paste0(export_name, ".csv")); . }
}
# Generate and export summary files for Tableau
create_seasonal_summary(combined_data_df_cleaned, "tableau_seasonal_summary")
create_hourly_by_date_summary(combined_data_df_cleaned, "tableau_hourly_by_date")
create_dayofweek_summary(combined_data_df_cleaned, "tableau_dayofweek_summary")
create_time_summary(combined_data_df_cleaned, "week", "tableau_weekly_summary")
create_time_summary(combined_data_df_cleaned, "month", "tableau_monthly_summary")

# Bike type preference for donut chart
bike_pref_summary <- combined_data_df_cleaned %>%
  group_by(member_casual, rideable_type) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(member_casual) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))
write_csv(bike_pref_summary, "tableau_bike_preference_summary.csv")

# Ride length boxplot data
ride_length_box_data <- combined_data_df_cleaned %>%
  filter(ride_length <= 90) %>%
  select(ride_id, member_casual, season, ride_date, ride_length)
write_csv(ride_length_box_data, "tableau_ride_length_boxplot.csv")

# Ride length box summary
ride_length_box_summary <- combined_data_df_cleaned %>%
  filter(ride_length <= 90) %>%
  group_by(season, member_casual) %>%
  summarise(count = n(), min = min(ride_length), q1 = quantile(ride_length, 0.25),
            median = median(ride_length), q3 = quantile(ride_length, 0.75),
            max = max(ride_length), .groups = "drop")
write_csv(ride_length_box_summary, "tableau_ride_length_box_summary.csv")

# Ride length histogram
ride_length_histogram <- combined_data_df_cleaned %>%
  filter(ride_length <= 90) %>%
  mutate(
    ride_length_bin = cut(ride_length, breaks = seq(0, 90, by = 5), include.lowest = TRUE, right = FALSE)
  ) %>%
  group_by(ride_length_bin, member_casual) %>%
  summarise(count = n(), .groups = "drop")
write_csv(ride_length_histogram, "tableau_ride_length_histogram.csv")

# Combine start and end station names into a unified long format (shared prep step)
station_usage_base <- combined_data_df_cleaned %>%
  filter(!is.na(start_station_name) & !is.na(start_lat) & !is.na(start_lng) &
           !is.na(end_station_name) & !is.na(end_lat) & !is.na(end_lng)) %>%
  select(member_casual, start_station_name, start_lat, start_lng,
         end_station_name, end_lat, end_lng) %>%
  pivot_longer(
    cols = c(start_station_name, end_station_name),
    names_to = "station_role",
    values_to = "station_name"
  ) %>%
  mutate(
    lat = ifelse(station_role == "start_station_name", start_lat, end_lat),
    lng = ifelse(station_role == "start_station_name", start_lng, end_lng)
  ) %>%
  select(member_casual, station_name, lat, lng) %>%
  group_by(member_casual, station_name, lat, lng) %>%
  summarise(ride_count = n(), .groups = "drop") %>%
  arrange(member_casual, desc(ride_count))

# 🔟 Top 10 for bar chart
top_station_usage <- station_usage_base %>%
  group_by(member_casual) %>%
  slice_max(ride_count, n = 10)

# Optional: save for Tableau
write_csv(top_station_usage, "tableau_top10_station_combined.csv")

# 🗺️ Top 20 for mapping
top_station_usage_map <- station_usage_base %>%
  group_by(member_casual) %>%
  slice_max(ride_count, n = 20)

# Optional: save for Tableau map
write_csv(top_station_usage_map, "tableau_top20_station_map.csv")

# 🔍 Static map using ggplot2
ggplot(top_station_usage_map, aes(x = lng, y = lat, color = member_casual)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Top 20 Most Frequently Used Stations (Mapped)",
    x = "Longitude", y = "Latitude", color = "Rider Type"
  ) +
  theme_minimal()

# 🌍 Interactive map using leaflet
leaflet(top_station_usage_map) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng, lat = ~lat,
    popup = ~paste0("<b>", station_name, "</b><br/>", ride_count, " rides"),
    color = ~ifelse(member_casual == "member", "blue", "red"),
    fillOpacity = 0.8, radius = 6
  ) %>%
  addLegend(
    "bottomright",
    colors = c("blue", "red"),
    labels = c("Member", "Casual"),
    title = "Rider Type"
  )

cat("✅ All station summary files exported successfully on", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
