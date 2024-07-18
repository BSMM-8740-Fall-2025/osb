# created Nov 17, 2023


# Taylor, J.W. (2003) Short-term electricity demand forecasting using double seasonal exponential smoothing. Journal of the Operational Research Society, 54, 799-805.

timetk::taylor_30_min

timetk::taylor_30_min %>%
  timetk::plot_time_series(date, value)

# Q1 ----
# - plot the data
timetk::taylor_30_min %>%
  timetk::plot_time_series(date, value)

# - plot the acf, pacf
timetk::taylor_30_min %>%
  timetk::plot_acf_diagnostics(date, value, .lags = 100)

# - plot the acf, pacf
timetk::taylor_30_min %>%
  timetk::plot_seasonal_diagnostics(date, value)

# Q2 ----
# - downscale to hourly
taylor_60_min <-
  timetk::taylor_30_min %>%
  timetk::summarise_by_time(
    .date_var = date
    , .by = "hour"
    , value = sum(value)
  )

taylor_60_min %>%
  timetk::plot_time_series(date, value)

taylor_60_min %>%
  dplyr::summarize(min = min(date), max = max(date)) %>%
  dplyr::mutate(as = max - min)


# Split Data 80/20
splits <-
  taylor_60_min %>%
  timetk::time_series_split(
    initial = "2 months"
    , assess = "1 weeks"
  )

splits %>%
  timetk::tk_time_series_cv_plan() %>%
  timetk::plot_time_series_cv_plan(.date_var = date, .value = value)

train <- rsample::training(splits) #splits %>% dplyr::filter(.key == "training")
test  <- rsample::testing(splits) #plits %>% dplyr::filter(.key == "testing")

# Q3 ----
# remove some features
# - recipes (i) add time features, (ii) mutate

regression_rec <- taylor_60_min %>%
  recipes::recipe(value ~ .) %>%
  timetk::step_timeseries_signature(date) %>%
  recipes::step_rm( -c(date_index.num, date_month.lbl, date_wday.lbl, date_hour) ) %>%
  step_normalize(date_index.num) %>%
  recipes::step_mutate(date_hour = date_hour %>% as.factor()) %>%
  recipes::step_dummy(all_nominal(), one_hot = TRUE)

regression_rec %>% recipes::prep(training = train) %>% summary()
regression_rec %>% recipes::prep(training = train) %>%
  recipes::bake(new_data = NULL) %>%
  dplyr::glimpse()

fourier_rec <- taylor_60_min %>%
  recipes::recipe(value ~ date + .) %>%
  timetk::step_fourier(date, period = c(12,24,48,168,336), K=2)

fourier_rec %>% recipes::prep(training = train) %>% summary()


# Q4 ----
# multiple models
library(modeltime)

model_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets")

model_lm <- parsnip::linear_reg(penalty = 0.02, mixture = 0.5) %>%
  set_engine("glmnet")

model_arima <- modeltime::arima_reg() %>%
  parsnip::set_engine("auto_arima")

wfsets <- workflowsets::workflow_set(
  preproc = list(regression = regression_rec,  fourier = fourier_rec),
  models  = list(
    ets = model_ets
    , lm = model_lm
    , arima = model_arima
  ),
  cross   = TRUE
)

wfsets %>% modeltime::modeltime_fit_workflowset(train)

data_tbl <- tibble::tibble(
  date = seq.Date(from = as.Date("2000-01-01"), by = 1, length.out = 5),
  x    = rnorm(5) * 10,
  y    = 5:1
)
timetk::tk_index(data_tbl)

# ets -------
require(tidymodels)

model_ets <- modeltime::exp_smoothing() %>%
  parsnip::set_engine(engine = "ets")

base_rec <- train %>%
  recipes::recipe(value ~ date)

workflow_fit_ets <- workflows::workflow() %>%
  workflows::add_recipe(ets_rec) %>%
  workflows::add_model(model_ets) %>%
  parsnip::fit(rsample::training(splits))

# arima ----

model_arima <- modeltime::arima_reg() %>%
  parsnip::set_engine("auto_arima")

workflow_fit_arima <- workflows::workflow() %>%
  workflows::add_recipe(lm_rec_x) %>%
  workflows::add_model(model_arima) %>%
  parsnip::fit(rsample::training(splits))

# lm ----
model_lm <- parsnip::linear_reg(penalty = 0.02, mixture = 0.5) %>%
  parsnip::set_engine("glmnet")

# model_lm <- parsnip::linear_reg() %>%
#   set_engine("lm")

lm_rec <- train %>%
  recipes::recipe(value ~ .) %>%
  timetk::step_timeseries_signature(date) %>%
  recipes::step_select( value, date_index.num, date_month.lbl, date_wday.lbl, date_hour ) %>%
  step_normalize(date_index.num) %>%
  recipes::step_mutate(date_hour = date_hour %>% as.factor()) %>%
  recipes::step_dummy(all_nominal(), one_hot = TRUE)

workflow_fit_lm <- workflows::workflow() %>%
  workflows::add_recipe(lm_rec) %>%
  workflows::add_model(model_lm) %>%
  parsnip::fit(rsample::training(splits))


model_tbl <- modeltime::modeltime_table(
  workflow_fit_ets
  , workflow_fit_arima
  , workflow_fit_lm
)

model_tbl

calibration_tbl <- model_tbl %>%
  modeltime::modeltime_calibrate(testing(splits))

calibration_tbl


calibration_tbl %>% modeltime::modeltime_accuracy()

calibration_tbl %>%
  modeltime::modeltime_forecast(
    new_data    = testing(splits),
    actual_data = taylor_60_min
  ) %>%
  modeltime::plot_modeltime_forecast()


refit_tbl <- calibration_tbl %>%
  modeltime::modeltime_refit(data = taylor_60_min)

refit_tbl %>%
  modeltime::modeltime_forecast(h = "2 weeks", actual_data = taylor_60_min) %>%
  modeltime::plot_modeltime_forecast(
    .legend_max_width = 12, # For mobile screens
    .interactive      = TRUE
  )


# @@@@@@@@@@@@@@@@@
lm_rec <- train %>%

  recipes::recipe(formula = value ~ .) %>%
  timetk::step_timeseries_signature(date) %>%
  recipes::step_select(
    value
    , 'date_index' = `date_index.num`
    , date_hour) %>%
  # recipes::step_select( `date_index.num`, `date_month.lbl`, `date_wday.lbl`, `date_hour`) %>%
  # step_normalize(date_index.num) %>%
  recipes::step_mutate(date_hour = date_hour %>% as.factor()) %>%
  recipes::step_dummy(all_nominal(), one_hot = TRUE)

regression_rec <- train %>%
  recipes::recipe(value ~ .) %>%
  timetk::step_timeseries_signature(date) %>%
  recipes::step_select( value, date_index.num, date_month.lbl, date_wday.lbl, date_hour ) %>%
  step_normalize(date_index.num) %>%
  recipes::step_mutate(date_hour = date_hour %>% as.factor()) %>%
  recipes::step_dummy(all_nominal(), one_hot = TRUE)

workflow_fit_lm <- workflows::workflow() %>%
  workflows::add_recipe(regression_rec) %>%
  workflows::add_model(model_lm) %>%
  parsnip::fit(rsample::training(splits))
