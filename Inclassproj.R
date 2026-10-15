library(tidyquant)
library(tidyverse)
library(fpp3)

emp_raw <- tq_get(
  c("PAYNSA"),
  get = "economic.data",
  from = "1976-01-01"
)

emp <- emp_raw |>
  mutate(Month = yearmonth(date), employment = price) |>
  select(Month, employment)

emp <- emp |>
  as_tsibble(index = Month)

head(emp)

emp <- emp |>
  mutate(log_employment = log(employment))

emp <- emp |>
  mutate(employment_growth = 100 * difference(log_employment))

emp_fc <- emp |>
  filter(!is.na(employment_growth)) |>
  stretch_tsibble(.init = 240) |>
  model(mean = MEAN(employment_growth),
        naive = NAIVE(employment_growth),
        snaive = SNAIVE(employment_growth)) |>
  forecast(h = 1)
emp_fc |> accuracy(emp, measures = list(MAE = MAE))

errs <- emp_fc |>
  as_tibble() |>
  left_join(as_tibble(emp) |> select(Month, actual = employment_growth),
            by = "Month") |>
  mutate(e = actual - .mean) |>
  select(Month, .model, e) |>
  pivot_wider(names_from = .model, values_from = e) |>
  na.omit()
forecast::dm.test(errs$snaive, errs$mean, h = 1, power = 1)

