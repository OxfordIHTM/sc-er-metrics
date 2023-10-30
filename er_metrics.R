# Seychelles A&E metricds

## Load libraries ----
library(openxlsx)
library(sf)
library(dplyr)
library(tibble)
library(lubridate)
library(ggplot2)

## Set SC colour palette ----
sc_blue   <- "#002F6C"
sc_yellow <- "#FED141"
sc_red    <- "#D22730"
sc_green  <- "#007A33"


## Read data ----

### Read A&E data ----
sc_er_data <- read.xlsx(
  xlsxFile = "data/Copy of Audit of patient journey in A&E 09.03.2023-09.04.2023 .xlsx",
  detectDates = TRUE,
  sheet = 1
) |>
  tibble::tibble()

### Read SC administrative boundaries data (district) ----
sc_districts <- st_read(
  dsn = "data",
  layer = "syc_admbnda_adm3_nbs2010"
)


## Clean and process data ---
sc_er_data <- sc_er_data |>
  mutate(
    Date = stringr::str_replace_all(Date, "\\.", "-") |> as.Date(format = "%d-%m-%Y"),
    Time.Arrive = stringr::str_pad(Time.Arrive, width = 5, side = "left", pad = "0") |>
      stringr::str_replace_all("\\.", ":") |>
      paste0(":00"),
    Date_Time = paste(Date, Time.Arrive) |>
      strptime(format = "%Y-%m-%d %H:%M:%S", tz = "SCT"),
    Week = lubridate::isoweek(Date_Time),
    Hour = lubridate::hour(Date_Time),
    Age = as.integer(Age),
    age_group = cut(
      x = Age,
      breaks = seq(from = 0, to = 100, by = 5),
      labels = c(
        "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-40", 
        "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
        "80-84", "85-89", "90-94", "95+"
      )
    )
  )
  

## Visualisation

### Hourly arrivals per day over 1 month period

sc_er_data |>
  summarise(
    n = n(),
    .by = c(Date, Hour)
  ) |>
  ggplot(mapping = aes(x = Hour, y = n, group = Date)) +
  geom_col(fill = "lightblue", colour = "lightblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 2)) +
  scale_y_continuous(n.breaks = 8) +
  labs(
    title = "Hourly visits to A&E Department per day over a 1 month period",
    subtitle = "9 March to 9 April 2023",
    x = "Hour of day", y = "No. of visits"
  ) +
  facet_wrap(. ~ Date, ncol = 7) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18)
  )

ggsave(
  filename = "hourly_vists_per_day.png",
  width = 16,
  height = 8,
  units = "in"
)

### Hourly arrivals per week over 1 month period

sc_er_data |>
  filter(!is.na(Week)) |>
  mutate(
    Week = factor(
      x = Week, 
      labels = c("Week 10", "Week 11", "Week 12", "Week 13", "Week 14")
    )
  )|>
  summarise(
    n = n(),
    .by = c(Week, Hour)
  ) |>
  ggplot(mapping = aes(x = Hour, y = n, group = Week)) +
  geom_col(fill = "lightblue", colour = "lightblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1)) +
  scale_y_continuous(n.breaks = 8) +
  labs(
    title = "Hourly visits to A&E Department per week over a 1 month period",
    subtitle = "9 March to 9 April 2023",
    x = "Hour of day", y = "No. of visits"
  ) +
  facet_wrap(. ~ Week, ncol = 5) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18)
  )

ggsave(
  filename = "hourly_vists_per_week.png",
  width = 18,
  height = 8,
  units = "in"
)

### Hourly arrivals for the whole 1 month period

sc_er_data |>
  summarise(n = n(), .by = Hour) |>
  ggplot(mapping = aes(x = Hour, y = n)) +
  geom_col(fill = "lightblue", colour = "lightblue", alpha = 0.5) +
  annotate(
    geom = "rect", xmin = 7.5, xmax = 11.5, ymin = 0, ymax = Inf, 
    fill = "red", alpha = 0.2
  ) +
  annotate(geom = "text", x = 9.5, y = 10, label = "8 to 12 am", size = 8) +
  annotate(
    geom = "rect", xmin = 18.5, xmax = 21.5, ymin = 0, ymax = Inf, 
    fill = "red", alpha = 0.2
  ) +
  annotate(geom = "text", x = 20, y = 10, label = "6 to 10 pm", size = 8) +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 1)) +
  scale_y_continuous(n.breaks = 8) +
  labs(
    title = "Hourly visits to A&E Department over a 1 month period",
    subtitle = "9 March to 9 April 2023",
    x = "Hour of day", y = "No. of visits"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18)
  )

ggsave(
  filename = "hourly_vists_month.png",
  width = 16,
  height = 8,
  units = "in"
)


### Hourly arrivals for the whole 1 month period by category type

sc_er_data |>
  filter(!is.na(CAT) & CAT != "n/a") |>
  mutate(
    CAT = factor(
      x = CAT, 
      labels = c("Category 1", "Cateogry 2", "Catogery 3", "Category 4", "Category 5")
    )
  ) |>
  summarise(n = n(), .by = c(CAT, Hour)) |>
  ggplot(mapping = aes(x = Hour, y = n)) +
  geom_col(fill = "lightblue", colour = "lightblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 2)) +
  scale_y_continuous(n.breaks = 8) +
  labs(
    title = "Hourly visits to A&E Department over a 1 month period by category type",
    subtitle = "9 March to 9 April 2023",
    x = "Hour of day", y = "No. of visits"
  ) +
  facet_wrap(. ~ CAT, ncol = 5) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18)
  )

ggsave(
  filename = "hourly_vists_month_category.png",
  width = 16,
  height = 8,
  units = "in"
)


### Hourly arrivals for the whole 1 month period by five year age group

sc_er_data |>
  filter(!is.na(Age)) |>
  summarise(n = n(), .by = c(age_group, Hour)) |>
  ggplot(mapping = aes(x = Hour, y = n)) +
  geom_col(fill = "lightblue", colour = "lightblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 23, by = 2)) +
  scale_y_continuous(n.breaks = 8) +
  labs(
    title = "Hourly visits to A&E Department over a 1 month period by 5-year age group",
    subtitle = "9 March to 9 April 2023",
    x = "Hour of day", y = "No. of visits"
  ) +
  facet_wrap(. ~ age_group, ncol = 5) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 18)
  )

ggsave(
  filename = "hourly_vists_month_age_group.png",
  width = 16,
  height = 8,
  units = "in"
)