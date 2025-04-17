# created August 02, 2024

require(ggplot2)
require(magrittr)
require(rlang)

source(here::here("slides/R/ggdag-mask.R"))
source(here::here("slides/R/setup.R"))

#s
causalworkshop::net_data |>
  ggplot(aes(malaria_risk, fill = net)) +
  geom_density(color = NA, alpha = .8)

# causalworkshop::net_data |>
#   ggplot(aes(malaria_risk, fill = net)) +
#   geom_count(color = NA, alpha = .8)

#s
means <- causalworkshop::net_data |>
  dplyr::group_by(net) |>
  dplyr::summarize(malaria_risk = mean(malaria_risk)) |>
  dplyr::pull(malaria_risk)

means

causalworkshop::net_data |>
  dplyr::group_by(net) |>
  dplyr::summarize(malaria_risk = mean(malaria_risk))

causalworkshop::net_data |>
  lm(malaria_risk ~ net, data = _) |>
  broom::tidy()

# A tibble: 2 × 5
# term        estimate std.error statistic  p.value
#   <chr>          <dbl>     <dbl>     <dbl> <dbl>
# 1 (Intercept)     43.9     0.377     116.  0
# 2 netTRUE        -16.4     0.741     -22.1 1.10e-95

mosquito_dag <- ggdag::dagify(
  malaria_risk ~ net + income + health + temperature + resistance,
  net ~ income + health + temperature + eligible + household,
  eligible ~ income + household,
  health ~ income,
  exposure = "net",
  outcome = "malaria_risk",
  coords = list(
    x = c(
      malaria_risk = 7,
      net = 3,
      income = 4,
      health = 5,
      temperature = 6,
      resistance = 8.5,
      eligible = 2,
      household = 1
    ),
    y = c(
      malaria_risk = 2,
      net = 2,
      income = 3,
      health = 1,
      temperature = 3,
      resistance = 2,
      eligible = 3,
      household = 2
    )
  ),
  labels = c(
    malaria_risk = "Risk of malaria",
    net = "Mosquito net",
    income = "Income",
    health = "Health",
    temperature = "Nighttime temperatures",
    resistance = "Insecticide resistance",
    eligible = "Eligible for program",
    household = "Number in the household"
  )
)

mosquito_dag |>
  ggdag::tidy_dagitty() |>
  ggdag::node_status() |>
  ggplot(
    aes(x, y, xend = xend, yend = yend, color = status)
  ) +
  ggdag::geom_dag_edges() +
  ggdag::geom_dag_point() +
  ggdag::geom_dag_label_repel() +
  ggokabeito::scale_color_okabe_ito(na.value = "grey90") +
  ggdag::theme_dag() +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")

#s
propensity_model <- glm(
  net ~ income + health + temperature,
  data = causalworkshop::net_data,
  family = binomial()
)

# the first six propensity scores
head(predict(propensity_model, type = "response"))

#s
net_data_wts <- propensity_model |>
  broom::augment(newdata = causalworkshop::net_data, type.predict = "response") |>
  # .fitted is the value predicted by the model
  # for a given observation
  dplyr::mutate(wts = propensity::wt_ate(.fitted, net))

net_data_wts |>
  dplyr::select(net, .fitted, wts) |>
  dplyr::slice_head(n=16)

#s
net_data_wts |>
  lm(malaria_risk ~ net, data = _, weights = wts) |>
  broom::tidy(conf.int = TRUE)

estimates <- net_data_wts |>
  lm(malaria_risk ~ net, data = _, weights = wts) |>
  broom::tidy(conf.int = TRUE) |>
  dplyr::filter(term == "netTRUE") |>
  dplyr::select(estimate, starts_with("conf")) |>
  dplyr::mutate(
    dplyr::across( everything(), \(x)round(x, digits = 1) )
  )
estimates

fit_ipw <- function(split, ...) {
  # get bootstrapped data sample with `rsample::analysis()`
  if("rsplit" %in% class(split)){
    .df <- rsample::analysis(split)
  }else if("data.frame" %in% class(split)){
    .df <- split
  }

  # fit propensity score model
  propensity_model <- glm(
    net ~ income + health + temperature,
    data = .df,
    family = binomial()
  )

  # calculate inverse probability weights
  .df <- propensity_model |>
    broom::augment(type.predict = "response", data = .df) |>
    dplyr::mutate(wts = propensity::wt_ate(.fitted, net))

  # fit correctly bootstrapped ipw model
  lm(malaria_risk ~ net, data = .df, weights = wts) |>
    broom::tidy()
}

# create bootstrap samples
bootstrapped_net_data <- rsample::bootstraps(
  causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net)),
  times = 1000,
  # required to calculate CIs later
  apparent = TRUE
)

# create ipw and fit each bootstrap sample
ipw_results <- bootstrapped_net_data |>
  dplyr::mutate(
    boot_fits = purrr::map(splits, fit_ipw))

ipw_results |>
  dplyr::mutate(
    estimate = purrr::map_dbl(
      boot_fits,
      # pull the `estimate` for `netTRUE` for each fit
      \(.fit) .fit |>
        dplyr::filter(term == "net") |>
        dplyr::pull(estimate)
    )
  ) |>
  ggplot(aes(estimate)) +
  geom_histogram(fill = "#D55E00FF", color = "white", alpha = 0.8)

# ========================
outcome_model <- glm(
  malaria_risk ~ net + income + health + temperature + insecticide_resistance +
    I(health^2) + I(temperature^2) + I(income^2) + I(insecticide_resistance^2),
  data = causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))
)

outcome_model |> broom::tidy()

fit_reg <- function(split, ...) {
  # print(class(split))
  # get bootstrapped data sample with `rsample::analysis()`
  if("rsplit" %in% class(split)){
    .df <- rsample::analysis(split)
  }else if("data.frame" %in% class(split)){
    .df <- split
  }
  # print(.df |> dplyr::slice_head(n=5))
  # print(.df |> dim())
  # fit outcome model
  glm(malaria_risk ~ net + income + health + temperature + insecticide_resistance +
      , data = .df
    )|>
    broom::tidy() # %T>% print()
}

both_results <- ipw_results |>
  dplyr::mutate(
    reg_fits = purrr::map(splits, fit_reg))

both_results_dat <- both_results |>
  dplyr::mutate(
    reg_estimate = purrr::map_dbl(
      reg_fits,
      # pull the `estimate` for `net` for each fit
      \(.fit) .fit |>
        dplyr::filter(term == "net") |>
        dplyr::pull(estimate)
    )
    , ipw_estimate = purrr::map_dbl(
      boot_fits,
      # pull the `estimate` for `net` for each fit
      \(.fit) .fit |>
        dplyr::filter(term == "net") |>
        dplyr::pull(estimate)
    )
  )

# both_results_dat |>
#   dplyr::summarize(
#     ipw = mean(ipw_estimate), ipw_sd = sd(ipw_estimate), ipw_med = median(ipw_estimate)
#     , reg = mean(reg_estimate), reg_sd = sd(reg_estimate), reg_med = median(reg_estimate)
#   )

dat <- both_results_dat |>
  dplyr::select(reg_estimate, ipw_estimate) |>
  tidyr::pivot_longer(cols=everything(), names_to = "method", values_to = "effect estimate")

# dat |>
#   ggplot(aes(value)) +
#   geom_histogram(data=subset(dat, name == 'reg_estimate'),color = "#DFFE00FF", fill = "white", alpha = 0.5) +
#   geom_histogram(data=subset(dat, name == 'ipw_estimate'),color = "#D55E00FF", fill = "white", alpha = 0.5)

dat |>
  dplyr::mutate(method=factor(method)) |>
  ggplot(aes(`effect estimate`,colour = method,), bins = 50, alpha = .5) +
  geom_histogram(fill = "white", alpha = 0.2)

dat |>
  dplyr::mutate(method=factor(method)) |>
  ggplot(aes(`effect estimate`, after_stat(density), colour = method, fill = method), bins = 50, alpha = .5) +
  geom_histogram(alpha = 0.2, position = 'identity') +
  geom_density(alpha = 0.2)

# dat |>
#   ggplot(aes(value)) +
#   halfmoon::geom_mirror_histogram(data=subset(dat, name == 'reg_estimate'),fill = "red", alpha = 0.2) +
#   halfmoon::geom_mirror_histogram(data=subset(dat, name == 'reg_estimate'),fill = "blue", alpha = 0.2)
#
# |>
#   ggplot(aes(estimate)) +
#   geom_histogram(fill = "#DFFE00FF", color = "white", alpha = 0.8)

dat |>
  dplyr::group_by(method) |>
  dplyr::summarise(
    mean.est = mean(`effect estimate`, na.rm = TRUE)
    , sd.est = sd(`effect estimate`, na.rm = TRUE)
    , n.est = 2 #dplyr::n()
  ) |>
  dplyr::mutate(se.est = sd.est / sqrt(n.est),
          lower.ci.est = mean.est - qt(1 - (0.05 / 2), n.est - 1) * se.est,
          upper.ci.est = mean.est + qt(1 - (0.05 / 2), n.est - 1) * se.est)


dat |>
  dplyr::group_by(method) |>
  dplyr::summarise(
  ate_est = mean(`effect estimate`)
  , lower_ci = quantile(`effect estimate`, 0.025)
  , upper_ci = quantile(`effect estimate`, 0.975)
)


both_results |>
  dplyr::mutate(
    reg_estimate = purrr::map_dbl(
      reg_fits,
      # pull the `estimate` for `netTRUE` for each fit
      \(.fit){
        # print(.fit)
        .fit |>
          dplyr::filter(term == "netTRUE") |>
          dplyr::pull(estimate)
      }

    )
  ) |> dplyr::glimpse()



both_results

# using the boot package
# from https://www.r-bloggers.com/2019/09/understanding-bootstrap-confidence-interval-output-from-the-r-boot-package/
reg_fn <-
  (\(x){
  fit_reg(x) |>
    dplyr::filter(term == "netTRUE") |>
    dplyr::pull(estimate)
})

# boot_out_reg
boot_out_reg <- boot::boot(
  data = causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))
  , R = 1000
  , sim = "ordinary"
  , statistic =
    (\(x,y){
      fit_reg(x[y,]) |>
        dplyr::filter(term == "net") |>
        dplyr::pull(estimate)
    })
)

boot_out_reg |>
  boot::boot.ci(L = boot::empinf(boot_out_reg, index=1L, type="jack"))

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates
#
# CALL :
#   boot::boot.ci(boot.out = boot_out_reg, L = boot::empinf(boot_out_reg,
#                                                           index = 1, type = "jack"))
#
# Intervals :
#   Level      Normal              Basic
# 95%   (-12.74, -11.56 )   (-12.73, -11.55 )
#
# Level     Percentile            BCa
# 95%   (-12.75, -11.56 )   (-12.75, -11.56 )
# Calculations and Intervals on Original Scale

boot_out_ipw <- boot::boot(
  data = causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))
  , R = 1000
  , sim = "ordinary"
  , statistic =
    (\(x,y){
      fit_ipw(x[y,]) |>
        dplyr::filter(term == "net") |>
        dplyr::pull(estimate)
    })
)

boot_out_ipw |>
  boot::boot.ci(L = boot::empinf(boot_out_ipw, index=1L, type="jack")) -> edc

# BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
# Based on 1000 bootstrap replicates
#
# CALL :
#   boot::boot.ci(boot.out = boot_out_ipw, L = boot::empinf(boot_out_ipw,
#                                                           index = 1, type = "jack"))
#
# Intervals :
#   Level      Normal              Basic
# 95%   (-13.40, -11.72 )   (-13.43, -11.75 )
#
# Level     Percentile            BCa
# 95%   (-13.34, -11.66 )   (-13.34, -11.67 )

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
table(dat_$net)
# 0    1
# 1298  454

dat_ <- causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))

risk_model <- glm(malaria_risk ~ income + health + temperature, data = dat_)

rmr <- risk_model$residuals

net_model <- glm(net ~ income + health + temperature , data = dat_, family=binomial)

# see https://stats.stackexchange.com/questions/1432/what-do-the-residuals-in-a-logistic-regression-mean
lp = predict(net_model)
mu = exp(lp)/(1+exp(lp))

# manually calculating the 1st response residual
nmr <- resid(net_model, "response")
# dat_$net[1] - mu[1]


nmr <- net_model$residuals

cov(rmr,nmr) / var(nmr)

# manually calculating the 1st pearson residual
resid(net_model, type="pearson")[1]
(dat_$net[1]-mu[1]) / sqrt(mu[1]*(1-mu[1]))
var(resid(net_model, type="pearson"))

# manually calculating the 1st deviance residual
resid(net_model, type="deviance")[1]
sqrt(-2*log(1-mu[1]))*sign(dat_$net[1]-mu[1])  # shortcut, since y_1=0
var(resid(net_model, type="deviance"))

# manually calculating the 1st working residual
resid(net_model, type="working")[1]
(dat_$net[1]-mu[1]) / (mu[1]*(1-mu[1]))
var(resid(net_model, type="working"))







# +++++++++++++++++++DOUBLE ROBUST++++++++++++++++++++++++++++++++++


#| label: doubly robust estimation
D <- 'intervention'
Y <- 'achievement_score'
X <- setdiff(names(tmp), c('schoolid',D,Y))

doubly_robust <- function(df, X, D, Y){
  ps <- # propensity score
    as.formula(paste(D, " ~ ", paste(X, collapse= "+"))) |>
    stats::glm( data = df, family = binomial() ) |>
    broom::augment(type.predict = "response", data = df) |>
    dplyr::pull(.fitted)

  lin_frml <- formula(paste(Y, " ~ ", paste(X, collapse= "+")))

  idx <- df[,D] %>% dplyr::pull(1) == 0
  mu0 <- # mean response D == 0
    lm(lin_frml, data = df[idx,]) %>%
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  idx <- df[,D] %>% dplyr::pull(1) == 1
  mu1 <- # mean response D == 1
    lm(lin_frml, data = df[idx,]) |>
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  # convert treatment factor to integer | recast as vectors
  d <- df[,D] %>% dplyr::pull(1) |> as.character() |> as.numeric()
  y <- df[,Y] %>% dplyr::pull(1)

  mean( d*(y - mu1)/ps + mu1 ) -
    mean(( 1-d)*(y - mu0)/(1-ps) + mu0 )
}

doubly_robust(
  causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))
  , c('income', 'health', 'temperature'), "net", "malaria_risk")

# -12.71


doubly_robust_bad_ipw <- function(df, X, D, Y){
  ps <- ps <- runif(dim(df)[1], 0.1, 0.9) # wrong propensity score

  lin_frml <- formula(paste(Y, " ~ ", paste(X, collapse= "+")))

  idx <- df[,D] %>% dplyr::pull(1) == 0
  mu0 <- # mean response D == 0
    lm(lin_frml, data = df[idx,]) %>%
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  idx <- df[,D] %>% dplyr::pull(1) == 1
  mu1 <- # mean response D == 1
    lm(lin_frml, data = df[idx,]) |>
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  # convert treatment factor to integer | recast as vectors
  d <- df[,D] %>% dplyr::pull(1) |> as.character() |> as.numeric()
  y <- df[,Y] %>% dplyr::pull(1)

  mean( d*(y - mu1)/ps + mu1 ) -
    mean(( 1-d)*(y - mu0)/(1-ps) + mu0 )
}

doubly_robust_bad_ipw(
  causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))
  , c('income', 'health', 'temperature'), "net", "malaria_risk")


# +++++++++++++++++++++++++ STACKING - g-computation +++++++++++++++++++++++++++++++++++++++
dat_ <- causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))

# model fitting the outcome
risk_model_net_fit <- glm(
  malaria_risk ~ net + income + health + temperature + insecticide_resistance, data = dat_)

risk_model_net_fit_1 <- glm(
  malaria_risk ~ (net + income + health + temperature + insecticide_resistance)^2, data = dat_)

dat_stacked <-
  dplyr::bind_rows(
    dat_ |> dplyr::mutate(net=1)
    , dat_ |> dplyr::mutate(net=0)
  )

predictions <-
  risk_model_net_fit_1 |>
  broom::augment(newdata = dat_stacked, type.predict = "response")

predictions |>
  dplyr::group_by(net) |>
  dplyr::summarize(mean_response = mean(.fitted)) |>
  dplyr::mutate(diff = mean_response - dplyr::lag(mean_response))


# +++++++++++++++++++++++ DML ++++++++++++++++++++++++
library(hdm)

utils::data("pension", package = "hdm")
help(pension)

data <- pension

skm <- skimr::skim(data)
skm$skim_variable
skm$skim_type

ggplot(data, aes(x = e401, fill = factor(e401))) +
  geom_bar()

dens_net_tfa <- ggplot(data, aes(x = net_tfa, color = factor(e401), fill = factor(e401))) +
  geom_density() +
  xlim(c(-20000, 150000)) +
  facet_wrap(. ~ e401)

data$e401 |> table()
# 0    1
# 6233 3682
data |> dplyr::group_by(e401) |>
  dplyr::summarize(mean = mean(net_tfa)) |>
  dplyr::mutate(diff = mean - dplyr::lag(mean))
# e401   mean   diff
# <int>  <dbl>  <dbl>
#   1     0 10788.    NA
# 2     1 30347. 19559.

data$p401 |> table()
# 0    1
# 7321 2594
data |> dplyr::group_by(p401) |>
  dplyr::summarize(mean = mean(net_tfa)) |>
  dplyr::mutate(diff = mean - dplyr::lag(mean))
# p401   mean   diff
# <int>  <dbl>  <dbl>
#   1     0 10890.    NA
# 2     1 38262. 27372.

rec_y <- data |> recipes::recipe(net_tfa ~ .) |>
  recipes::step_select(net_tfa, age, inc, educ, fsize, male, marr, twoearn, db, pira, hown)

rec_d <- data |> recipes::recipe(~ .) |>
#  recipes::step_select(net_tfa, e401, age, inc, educ, fsize, male, marr, twoearn, db, pira, hown) |>
  recipes::step_select(net_tfa, e401, age, inc, educ, fsize,        marr, twoearn, db, pira, hown) |>
  recipes::step_normalize(-c(net_tfa, e401)) |>
  recipes::step_mutate( e401F = factor(e401, levels = c(0, 1)) ) |>
#  recipes::step_bin2factor(p401, levels = c("0", "1") ) |>
  recipes::step_poly(age, degree = 6, options =list(raw=TRUE)) |>
  recipes::step_poly(inc, degree = 8, options =list(raw=TRUE)) |>
  recipes::step_poly(educ, degree = 4, options =list(raw=TRUE)) |>
  recipes::step_poly(fsize, degree = 4, options =list(raw=TRUE))

dat_y <- rec_y |> recipes::prep() |> recipes::bake(new_data = data)
dat_d <- rec_d |> recipes::prep() |> recipes::bake(new_data = data)


# **********
treatment <- c("e401")
response <- c("net_tfa")
xActual <- c("age", "inc", "fsize", "educ", "pira", "hown", "marr", "db", "twoearn")

pension %>%
  select(net_tfa,
         e401,
         age, inc, fsize, educ,
         pira, hown, marr, db, twoearn) -> modelData

modelData %>% mutate(e401F = factor(e401, levels = c(0, 1))) -> modelData

inds <- sample.int(nrow(dat_d ), nrow(dat_d )/2, replace=F)

dataList <- list(dat_d [inds, ],
                 dat_d [-inds, ])

train_control <- caret::trainControl(method="adaptive_cv",
                              number=10,
                              search = "random",
                              verboseIter = TRUE)

rfResponseModel <- lapply(dataList,
                          function(x) caret::train(net_tfa ~ . - e401 - e401F,
                                            method = "ranger",
                                            tuneLength = 10,
                                            data = x,
                                            verbose = T,
                                            trControl = train_control))

rfTreatmentModel <- lapply(dataList,
                           function(x) caret::train(e401F ~ . - net_tfa - e401,
                                             method="ranger",
                                             tuneLength = 10,
                                             data = x,
                                             verbose = T,
                                             trControl = train_control))

calc_theta <- function(dataList, responseModel, treatmentModel){

  # Predict the response in dataset 1 (2) using model 2 (1).
  responsePredictions <- lapply(list(c(1,2), c(2,1)),
                                function(i) predict(responseModel[[i[1]]],
                                                    dataList[[i[2]]]))
  # Do the same for the treatment model
  treatmentPredictions <- lapply(list(c(1,2), c(2,1)),
                                 function(i) as.numeric(predict(treatmentModel[[i[1]]],
                                                                dataList[[i[2]]])) - 1)
  # Calculate the treatment residuals
  treatmentResiduals <- list(dataList[[2]]$e401 - treatmentPredictions[[1]],
                             dataList[[1]]$e401 - treatmentPredictions[[2]])

  # Calculate the response residuals
  responseResiduals <- list(dataList[[2]]$net_tfa - responsePredictions[[1]],
                            dataList[[1]]$net_tfa - responsePredictions[[2]])

  # Regress the residuals across both datasets
  theta1 <- mean(treatmentResiduals[[1]] %*% responseResiduals[[1]]) / mean(treatmentResiduals[[1]] %*% dataList[[2]]$e401)
  theta2 <- mean(treatmentResiduals[[2]] %*% responseResiduals[[2]]) / mean(treatmentResiduals[[2]] %*% dataList[[1]]$e401)

  # Take the average as our treatment effect estimator
  mean(c(theta1, theta2))
}

calc_theta(dataList, rfResponseModel, rfTreatmentModel)

rfResponseModel_lm <- lapply(dataList,
                          function(x) lm(net_tfa ~ . - e401 - e401F -1, data = x) )

rfTreatmentModel_lm <- lapply(dataList,
                           function(x) glm(e401F ~ . - net_tfa - e401 -1, data = x, family=binomial) )

calc_theta(dataList, rfResponseModel_lm, rfTreatmentModel_lm)

rfResponseModel_gbm <- lapply(dataList,
                          function(x) caret::train(net_tfa ~ . - e401 - e401F,
                                                   method = "gbm",
                                                   tuneLength = 10,
                                                   data = x,
                                                   verbose = T,
                                                   trControl = train_control))

rfTreatmentModel_gbm <- lapply(dataList,
                           function(x) caret::train(e401F ~ . - net_tfa - e401,
                                                    method="glmnet",
                                                    tuneLength = 10,
                                                    data = x,
                                                    verbose = T,
                                                    trControl = train_control))

calc_theta(dataList, rfResponseModel_gbm, rfTreatmentModel_gbm)

# **********


boot_fn <- function(dat, idx, y, t){
  #print(idx)
  # print(dim(dat))
  # print(y)
  # print(t)
  # print(class(dat))

  wf <- workflows::workflow() |>
    # workflows::add_model( linear_reg() |> set_engine("lm") )
    workflows::add_model( parsnip::rand_forest(trees = 200, min_n = 5) |> set_engine("ranger") |> parsnip::set_mode("regression") )

  res <- tibble::tibble(
    wf_y = wf |>
      workflows::add_formula( formula(paste(y, "~.", collapse = " ")) ) |>
      parsnip::fit(data = dat[idx,] |> dplyr::select(-dplyr::all_of(c(t))) ) |>
      broom::augment(dat[idx,]) |> dplyr::select(-dplyr::all_of(c(t, ".pred"))) |>
      dplyr::mutate(.resid = 1-2) |>
      dplyr::pull(.resid)
      # broom::augment(dat[idx,]) |>
      # dplyr::pull(.resid)

    ,wf_t = wf |>
      workflows::add_formula( formula(paste(t, "~.", collapse = " ")) ) |>
      parsnip::fit(data = dat[idx,] |> dplyr::select(-dplyr::all_of(c(y))) ) |>
      broom::augment(dat[idx,]) |> dplyr::select(-dplyr::all_of(c(t, ".pred"))) |>
      dplyr::mutate(.resid = 1-2) |>
      dplyr::pull(.resid)

  )

  print(res)
  res |>
    lm(wf_y ~ wf_t, data = _) |>
    broom::tidy() |>
    dplyr::filter(term == "wf_t") |>
    dplyr::pull(estimate)


  # lm(wf_y ~ wf_y)
  #
  print(res)
  res
}

bt <- boot::boot(dat_d, statistic = boot_fn, R = 500, y = "net_tfa", t = "e401")

bt <- boot::boot(dat_d, statistic = boot_fn, R = 500, y = "net_tfa", t = "e401")
bt$t |> mean()
bt_p <- boot::boot(dat_d, statistic = boot_fn, R = 500, y = "net_tfa", t = "p401")
bt_p$t |> mean()
bt_p2 <- boot::boot(dat_d, statistic = boot_fn, R = 1000, y = "net_tfa", t = "p401")
bt_p2$t |> mean()
f1 <- \(x){ sd(x)/sqrt(length(x))}
bt_p2$t |> f1()

  \(x){ sd(x)/sqrt(length(x))}()


bt_ranger <- boot::boot(dat_d, statistic = boot_fn, R = 500, y = "net_tfa", t = "p401")



set.seed(27)
boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)

lm(net_tfa ~., data = dat_d) |> broom::tidy()

workflows::workflow() |>
  # workflows::add_model( linear_reg() |> set_engine("lm") ) |>
  workflows::add_model( parsnip::rand_forest(trees = 200, min_n = 5) |> set_engine("ranger") |> parsnip::set_mode("regression") ) |>
  workflows::add_formula( formula(paste("net_tfa", "~.")) ) |>
  parsnip::fit(data = dat_d |> dplyr::select(-dplyr::all_of(c("p401"))) ) |>
  broom::augment(dat_d) |> dplyr::glimpse()
  dplyr::pull(.resid)

# !!!!!!!!!!!!!! original
# outcome variable
y <- data[, "net_tfa"]
# treatment variable
D <- data[, "e401"]
D2 <- data[, "p401"]
D3 <- data[, "a401"]

columns_to_drop <- c(
  "e401", "p401", "a401", "tw", "tfa", "net_tfa", "tfa_he",
  "hval", "hmort", "hequity",
  "nifa", "net_nifa", "net_n401", "ira",
  "dum91", "icat", "ecat", "zhat",
  "i1", "i2", "i3", "i4", "i5", "i6", "i7",
  "a1", "a2", "a3", "a4", "a5"
)

# covariates
X <- data[, !(names(data) %in% columns_to_drop)]

# Constructing the controls
x_formula <- paste("~ 0 + poly(age, 6, raw=TRUE) + poly(inc, 8, raw=TRUE) + poly(educ, 4, raw=TRUE) ",
                   "+ poly(fsize, 2, raw=TRUE) + male + marr + twoearn + db + pira + hown")
X <- data.table::as.data.table(model.frame(x_formula, X))
head(X)

dml2_for_plm <- function(x, d, y, dreg, yreg, nfold = 3, method = "regression") {
  nobs <- nrow(x) # number of observations
  foldid <- rep.int(1:nfold, times = ceiling(nobs / nfold))[sample.int(nobs)] # define folds indices
  I <- split(1:nobs, foldid) # split observation indices into folds
  ytil <- dtil <- rep(NA, nobs)
  cat("fold: ")
  for (b in seq_along(I)) {
    if (method == "regression") {
      dfit <- dreg(x[-I[[b]], ], d[-I[[b]]]) # take a fold out
      yfit <- yreg(x[-I[[b]], ], y[-I[[b]]]) # take a foldt out
      dhat <- predict(dfit, x[I[[b]], ], type = "response") # predict the left-out fold
      yhat <- predict(yfit, x[I[[b]], ], type = "response") # predict the left-out fold
      dtil[I[[b]]] <- (d[I[[b]]] - dhat) # record residual for the left-out fold
      ytil[I[[b]]] <- (y[I[[b]]] - yhat) # record residial for the left-out fold
    } else if (method == "randomforest") {
      dfit <- dreg(x[-I[[b]], ], as.factor(d)[-I[[b]]]) # take a fold out
      yfit <- yreg(x[-I[[b]], ], y[-I[[b]]]) # take a fold out
      dhat <- predict(dfit, x[I[[b]], ], type = "prob")[, 2] # predict the left-out fold
      yhat <- predict(yfit, x[I[[b]], ], type = "response") # predict the left-out fold
      dtil[I[[b]]] <- (d[I[[b]]] - dhat) # record residual for the left-out fold
      ytil[I[[b]]] <- (y[I[[b]]] - yhat) # record residial for the left-out fold
    } else if (method == "decisiontrees") {
      dfit <- dreg(x[-I[[b]], ], as.factor(d)[-I[[b]]]) # take a fold out
      yfit <- yreg(x[-I[[b]], ], y[-I[[b]]]) # take a fold out
      dhat <- predict(dfit, x[I[[b]], ])[, 2] # predict the left-out fold
      yhat <- predict(yfit, x[I[[b]], ]) # predict the left-out fold
      dtil[I[[b]]] <- (d[I[[b]]] - dhat) # record residual for the left-out fold
      ytil[I[[b]]] <- (y[I[[b]]] - yhat) # record residial for the left-out fold
    } else if (method == "boostedtrees") {
      dfit <- dreg(x[-I[[b]], ], d[-I[[b]]]) # take a fold out
      yfit <- yreg(x[-I[[b]], ], y[-I[[b]]]) # take a fold out
      dhat <- predict(dfit, x[I[[b]], ], type = "response") # predict the left-out fold
      yhat <- predict(yfit, x[I[[b]], ], type = "response") # predict the left-out fold
      dtil[I[[b]]] <- (d[I[[b]]] - dhat) # record residual for the left-out fold
      ytil[I[[b]]] <- (y[I[[b]]] - yhat) # record residial for the left-out fold
    }
    cat(b, " ")
  }
  rfit <- lm(ytil ~ dtil) # estimate the main parameter by regressing one residual on the other
  coef_est <- coef(rfit)[2] # extract coefficient
  se <- sqrt(sandwich::vcovHC(rfit)[2, 2]) # record robust standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef_est, se)) # printing output
  return(list(coef_est = coef_est, se = se, dtil = dtil, ytil = ytil)) # save output and residuals
}


summaryPLR <- function(point, stderr, resD, resy, name) {
  data <- data.frame(
    estimate = point, # point estimate
    stderr = stderr, # standard error
    lower = point - 1.96 * stderr, # lower end of 95% confidence interval
    upper = point + 1.96 * stderr, # upper end of 95% confidence interval
    `rmse y` = sqrt(mean(resy^2)), # RMSE of model that predicts outcome y
    `rmse D` = sqrt(mean(resD^2)), # RMSE of model that predicts treatment D
    `accuracy D` = mean(abs(resD) < 0.5) # binary classification accuracy of model for D
  )
  rownames(data) <- name
  return(data)
}

set.seed(123)
cat(sprintf("\nDML with Lasso CV \n"))

dreg_lasso_cv <- function(x, d) {
  glmnet::cv.glmnet(x, d, family = "gaussian", alpha = 1, nfolds = 5)
}
yreg_lasso_cv <- function(x, y) {
  glmnet::cv.glmnet(x, y, family = "gaussian", alpha = 1, nfolds = 5)
}

dml2_results <- dml2_for_plm(as.matrix(X), D, y, dreg_lasso_cv, yreg_lasso_cv, nfold = 5)

sum_lasso_cv <- summaryPLR(dml2_results$coef_est, dml2_results$se, dml2_results$dtil,
                           dml2_results$ytil, name = "LassoCV")
tableplr <- data.frame()
tableplr <- rbind(sum_lasso_cv)
tableplr

# Because residuals are output, reconstruct fitted values for use in ensemble
dhat_lasso <- D - dml2_results$dtil
yhat_lasso <- y - dml2_results$ytil

# DML with Random Forest
set.seed(123)
cat(sprintf("\nDML with Random Forest \n"))

dreg_rf <- function(x, d) {
  randomForest::randomForest(x, d, ntree = 1000, nodesize = 10)
} # ML method=Forest
yreg_rf <- function(x, y) {
  randomForest::randomForest(x, y, ntree = 1000, nodesize = 10)
} # ML method=Forest

dml2_results <- dml2_for_plm(as.matrix(X), D, y, dreg_rf, yreg_rf, nfold = 5, method = "randomforest")
sum_rf <- summaryPLR(dml2_results$coef_est, dml2_results$se, dml2_results$dtil,
                     dml2_results$ytil, name = "Random Forest")
tableplr <- rbind(tableplr, sum_rf)
tableplr

dhat_rf <- D - dml2_results$dtil
dhat_rf <- y - dml2_results$ytil


# DML with Decision Trees
set.seed(123)
cat(sprintf("\nDML with Decision Trees \n"))

dreg_tr <- function(x, d) {
  rpart::rpart(as.formula("D~."), cbind(data.frame(D = d), x), method = "class", minbucket = 10, cp = 0.001)
}
dreg_tr <- function(x, y) {
  rpart::rpart(as.formula("y~."), cbind(data.frame(y = y), x), minbucket = 10, cp = 0.001)
}

# decision tree takes in X as dataframe, not matrix/array
dml2_results <- dml2_for_plm(X, D, y, dreg_tr, dreg_tr, nfold = 5, method = "decisiontrees")
sum_tr <- summaryPLR(dml2_results$coef_est, dml2_results$se, dml2_results$dtil,
                     dml2_results$ytil, name = "Decision Trees")
tableplr <- rbind(tableplr, sum_tr)
tableplr

dhat_tr <- D - dml2_results$dtil
yhat_tr <- y - dml2_results$ytil

# @@@@@@@@

set.seed(27)


boots <- bootstraps(mtcars, times = 2000, apparent = TRUE)


# replication question ----

propensity_model <- glm(
  net ~ income + health + temperature,
  data = causalworkshop::net_data,
  family = binomial()
)

net_data_wts <- propensity_model |>
  broom::augment(newdata = causalworkshop::net_data, type.predict = "response") |>
  dplyr::mutate(
    wts =
      dplyr::case_when(
        net ~ 1/.fitted
        , TRUE ~ 1/(1-.fitted)
      )
  )

# randomization question ----
smpl_dat <- causalworkshop::net_data |>
  tibble::rowid_to_column()

split_0 <- smpl_dat |>
  dplyr::slice_sample(prop=0.5) |>
  dplyr::mutate(smpl = 0) |>
  dplyr::bind_rows(

  )

split_data <- split_0 |>
  dplyr::bind_rows(
    smpl_dat[-smpl_dat0$rowid,] |>
      dplyr::mutate(smpl = 1)
  ) |>
  dplyr::group_by(smpl)


tidysmd::tidy_smd(
  split_data,
  c(income, health, temperature),
  .group = smpl,
)

tidysmd::tidy_smd(
  causalworkshop::net_data,
  c(income, health, temperature),
  .group = net,
)

# FWL question ----

risk_data = readr::read_csv("slides/data/risk_data.csv", show_col_types = FALSE)

risk_data |>
  dplyr::group_by(credit_limit) |>
  dplyr::mutate(size = n(), default = mean(default), ) |>
  dplyr::distinct(default,credit_limit, size) |>
  ggplot(aes(x = credit_limit, y = default, size = size)) +
  geom_point() +
  labs(title = "Default Rate by Credit Limit", x = "credit_limit", y = "default") +
  theme_minimal()

glm(default ~ credit_limit, family=binomial, data = risk_data) |>

  lm(default ~ credit_limit, data = risk_data) |>
  broom::augment() |>
  dplyr::group_by(credit_limit) |>
  dplyr::mutate(size = n(), default = mean(default), ) |>
  dplyr::distinct(default,credit_limit, size) |>
  ggplot(aes(x = credit_limit, y = default, size = size)) +
  geom_point() +
  labs(title = "Default Rate by Credit Limit", x = "credit_limit", y = "default") +
  theme_minimal()

  lm(default ~ credit_limit + wage +  credit_score1 + credit_score2, data = risk_data)

  # !!!!!!!!!!

  debiasing_model <- lm(credit_limit ~ wage +  credit_score1 + credit_score2, data = risk_data)

  risk_data_deb <- risk_data |>
    dplyr::mutate(credit_limit_res = mean(credit_limit) + debiasing_model$residuals)

  lm(default ~ credit_limit_res, data = risk_data_deb) |>
    broom::tidy(conf.int = TRUE)

  risk_data_deb |>
    dplyr::mutate(credit_limit_res = round(credit_limit_res,digits=-2)) |>
    dplyr::group_by(credit_limit_res) |>
    dplyr::mutate(size = n(), default = mean(default), ) |>
    dplyr::filter(size>30) |>
    dplyr::distinct(default,credit_limit_res, size) |>
    ggplot(aes(x = credit_limit_res, y = default, size = size)) +
    geom_point() +
    labs(title = "Default Rate by Debiased Credit Limit", x = "credit_limit", y = "default")+
    theme_minimal()

  denoising_model <-
    lm(default ~ wage + credit_score1  + credit_score2, data = risk_data_deb)

  denoising_model |> broom::tidy(conf.int = TRUE)

  risk_data_denoise <-  risk_data_deb |>
    dplyr::mutate(default_res = denoising_model$residuals + mean(default))

  lm(default_res ~ credit_limit_res, data = risk_data_denoise) |>
    broom::tidy(conf.int = TRUE)

