library(tidyquant)
library(tidyverse)
# 
# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# 
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw = read_rds("data/unemp_raw.rds")

unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"    ~ "Oregon",
      symbol == "CAUR"    ~ "California",
      symbol == "TXUR"    ~ "Texas",
      symbol == "NYUR"    ~ "New York"
    )
  )





unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

unemp <- unemp |>
  arrange(region,date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )




unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5)


unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5)


#Section 5:

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +        # or geom_col(), geom_point(), etc.
  labs(title = "Unemployment Rates by Region", x = "Date", y = "Unemployment Rate") +
  theme_minimal()

ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Monthly Unemployment Rate by Region",
       x = "Date",
       y = "Unemployment Rate (%)") +
  theme_minimal()


ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate, fill = region)) +
  geom_col() +
  labs(title = "Average Unemployment Rate by Region",
       x = "Region",
       y = "Average Unemployment Rate (%)") +
  theme_minimal()

#This graph shows unemployment rates for each region against the following month's change.
#I think this is interesting because it shows that when a given month has a relatively high unemployment rate, the following month's rate tends to fall.
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(next_month_change = lead(unemployment_rate_change)) |>
  ungroup()

ggplot(unemp, aes(x = unemployment_rate, y = next_month_change, color = region)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Unemployment Rate vs. Next Month's Change",
       x = "Unemployment Rate (%)",
       y = "Change in Following Month (percentage points)") +
  theme_minimal()


