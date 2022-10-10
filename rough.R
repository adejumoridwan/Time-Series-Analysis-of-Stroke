
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(fable)
library(tidymodels)


incidence |> 
  plot_time_series(Date, Count)

splits <- initial_time_split(incidence, prop = 0.9)

# Model 1: auto_arima ----
model_fit_arima_no_boost <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Count ~ Date, data = training(splits))
#> frequency = 12 observations per 1 year

models_tbl <- modeltime_table(
  model_fit_arima_no_boost
)

models_tbl

calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = incidence
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = T
  )

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = incidence)

refit_tbl %>%
  modeltime_forecast(h = "10 years", actual_data = incidence) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = T
  )
