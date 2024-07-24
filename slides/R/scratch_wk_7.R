# created November 16, 2023

require(magrittr)

# Data
m750 <- timetk::m4_monthly %>% filter(id == "M750")

m750 %>%
  timetk::plot_time_series(date, value)

# Split Data 80/20
splits <-
  timetk::time_series_split(
    m750
    , initial = "20 years"
    , assess = "5 years"
  )

splits %>%
  timetk::tk_time_series_cv_plan() %>%
  timetk::plot_time_series_cv_plan(.date_var = date, .value = value)

# model :ets
model_fit_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets") %>%
  parsnip::fit(value ~ date, data = rsample::training(splits))

model_fit_lm <- linear_reg() %>%
  set_engine("lm") %>%
  fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
      data = training(splits))

model_fit_arima <- modeltime::arima_reg() %>%
  parsnip::set_engine("auto_arima") %>%
  parsnip::fit(value ~ date, data = rsample::training(splits))

models_tbl <- modeltime::modeltime_table(
  model_fit_arima,
  # model_fit_arima_boosted,
  model_fit_ets,
  #model_fit_prophet,
  model_fit_lm
  #wflw_fit_mars
)

calibration_tbl <- models_tbl %>%
  modeltime::modeltime_calibrate(new_data = rsample::testing(splits))

calibration_tbl %>%
  modeltime::modeltime_forecast(
    new_data    = rsample::testing(splits),
    actual_data = m750
  ) %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )

calibration_tbl %>%
  modeltime::modeltime_accuracy() %>%
  modeltime::table_modeltime_accuracy(
    .interactive = FALSE
  )

refit_tbl <- calibration_tbl %>%
  modeltime::modeltime_refit(data = m750)

refit_tbl %>%
  modeltime::modeltime_forecast(h = "3 years", actual_data = m750) %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = TRUE
  )
