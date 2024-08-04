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
  malaria_risk ~ net + income + health + temperature + insecticide_resistance,
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
  glm(malaria_risk ~ net + income + health + temperature + insecticide_resistance
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
  tidyr::pivot_longer(cols=everything(), names_to = "method")

# dat |>
#   ggplot(aes(value)) +
#   geom_histogram(data=subset(dat, name == 'reg_estimate'),color = "#DFFE00FF", fill = "white", alpha = 0.5) +
#   geom_histogram(data=subset(dat, name == 'ipw_estimate'),color = "#D55E00FF", fill = "white", alpha = 0.5)

dat |>
  dplyr::mutate(method=factor(method)) |>
  ggplot(aes(value,colour = method), bins = 50, alpha = .5) +
  geom_histogram(fill = "white", alpha = 0.2)

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


mtcars %>%
  group_by(vs) %>%
  summarise(mean.mpg = mean(mpg, na.rm = TRUE),
            sd.mpg = sd(mpg, na.rm = TRUE),
            n.mpg = n()) %>%
  mutate(se.mpg = sd.mpg / sqrt(n.mpg),
         lower.ci.mpg = mean.mpg - qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg,
         upper.ci.mpg = mean.mpg + qt(1 - (0.05 / 2), n.mpg - 1) * se.mpg)



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
  boot::boot.ci(L = boot::empinf(boot_out_ipw, index=1L, type="jack"))

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







# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


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


# +++++++++++++++++++++++++ STACKING +++++++++++++++++++++++++++++++++++++++
dat_ <- causalworkshop::net_data |> dplyr::mutate(net = as.numeric(net))

# model fitting the outcome
risk_model_net_fit <- glm(malaria_risk ~ net + income + health + temperature + insecticide_resistance, data = dat_)

dat_stacked <-
  dplyr::bind_rows(
    dat_ |> dplyr::mutate(net=1)
    , dat_ |> dplyr::mutate(net=0)
  )

predictions <-
  risk_model_net_fit |>
  broom::augment(newdata = dat_stacked, type.predict = "response")

predictions |>
  dplyr::group_by(net) |>
  dplyr::summarize(mean_response = mean(.fitted)) |>
  dplyr::mutate(diff = mean_response - dplyr::lag(mean_response))

