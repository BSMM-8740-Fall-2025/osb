# created August 21, 2024


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

fq_dag <- ggdag::dagify(
  mq ~ ff + m + c + ci + ct,
  ff ~ c + ct + ci + fsa + fc,
  m ~ ff,
  es ~ mq + ff,
  #fsa ~ mq,
  exposure = "ff",
  outcome = "mq",
  labels = c(
    mq = "management_quality",
    ff = "founder_family",
    m = "managers",
    fsa = "firm_size_age",
    c = "competition",
    ci = "culture_institutions",
    ct = "complex_technology",
    fc = "family_circumstances",
    es = "export share"
  )
)

tidy_dagitty_obj <- fq_dag |>
  ggdag::tidy_dagitty(layout = "time_ordered")

ggdag::update_dag_data(tidy_dagitty_obj) <-
  tidy_dagitty_obj |>
    ggdag::pull_dag_data() |>
    dplyr::left_join(
      fq_dag |> ggdag::tidy_dagitty() |>
        ggdag::pull_dag_data() |>
        dplyr::select(name,to,label)
      , by = dplyr::join_by("name"=="name", "to"=="to")
    )


tidy_dagitty_obj |>

  # fq_dag |>
  # ggdag::tidy_dagitty() |>
  ggdag::node_status() |>
  ggplot(
    aes(x, y, xend = xend, yend = yend, color = status)
  ) +
  ggdag::geom_dag_edges() +
  ggdag::geom_dag_point() +
  ggdag::geom_dag_label_repel(aes(label = label)) +
  ggokabeito::scale_color_okabe_ito(na.value = "darkgrey") +
  ggdag::theme_dag() +
  theme(legend.position = "none") +
  coord_cartesian(clip = "off")

# "grey90"

#########################################################################################
# Prepared for Gabor's Data Analysis
#
# Data Analysis for Business, Economics, and Policy
# by Gabor Bekes and  Gabor Kezdi
# Cambridge University Press 2021
#
# gabors-data-analysis.com
#
# License: Free to share, modify and use for educational purposes.
# 	Not to be used for commercial purposes.

# CHAPTER 21
# CH20A Founder/family ownership and quality of management
# using the wms-management dataset
# version 0.92 2020-03-08
#########################################################################################

# Clear memory
rm(list=ls())

library(tidyverse)
library(purrr)
library(haven)
library(MatchIt)
library(Matching) # masks dplyr select!!! #
library(gmodels)
library(fixest)

# check if 'librarian' is installed and if not, install it
if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(fixest, gmodels, Hmisc, Matching, MatchIt)

getwd()
# set working directory
# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# data used
source("set-data-directory.R") #data_dir must be first defined #

use_case_dir <- file.path("ch21-ownership-management-quality/")

data_in <- use_case_dir
data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)

# This is the second part
# you must run ch21-wms-01-dataprep.R first.



# Read in data ------------------------------------------------------------

here::here("slides/R/da_data_repo", "wms-management-survey", "clean")

# tidy
data <-
  readr::read_csv(
    here::here("slides/R/da_data_repo", "wms-management-survey", "clean","wms_da_textbook-work.csv")
    , show_col_types = FALSE
  )

data |> dplyr::slice_head(n=5) |> dplyr::glimpse()

data <- read_csv(paste0(data_out, "wms_da_textbook-work.csv"))

data %>%
  group_by(foundfam_owned) %>%
  summarise (mean(management))

# Set variables to use -------------------------------------------------------

y_var <- "management"
x_var <- "foundfam_owned"

control_vars <- c("degree_nm", "degree_nm_sq", "compet_moder", "compet_strong",
                  "lnemp", "age_young", "age_old", "age_unknown")
control_vars_to_interact <- c("industry", "countrycode")


data %>%
  dplyr::select(all_of(c(control_vars, control_vars_to_interact))) %>%
  summary()

# *************************************************************
# * REGRESSIONS
# *************************************************************

# OLS with no control vars. -------------------------------------------------------
formula1 <- as.formula(paste0(y_var, " ~ ",x_var))
ols1 <- fixest::feols(formula1, data=data)

# OLS with all control vars -------------------------------------------------------

formula2 <- as.formula(paste0(y_var, " ~ ",x_var," + ",
                              paste(c(control_vars, control_vars_to_interact), collapse = " + ")))

ols2 <- fixest::feols(formula2, data=data)


# OLS with all controls + interactions -------------------------------------------------------

formula3 <- as.formula(paste(y_var, " ~ ",x_var," + ",
                             paste(control_vars_to_interact, collapse = ":"),
                             " + (", paste(control_vars, collapse = "+"),")*(",
                             paste(control_vars_to_interact, collapse = "+"),")",sep=""))

ols3 <- fixest::feols(formula3, data=data)

result <- fixest::etable( ols1,ols2,ols3,
        fitstat = c('n','r2'),
        keep = c('Constant',x_var),
        headers = c("'no confounders'", "'with confounders'", "'with confounders interacted'"),
        depvar = F )

result |> tibble::as_tibble(.name_repair = "unique") |>
  # take the rows with data
  dplyr::slice(c(1,3,4,6:8)) |>
  # use the first colun for row names
  gt::gt("...1") |>
  # divide the coefficient estimates from statistics estimates
  gt::tab_style(
    style = gt::cell_borders(sides = c("bottom"),  weight = px(0.5))
    ,locations = gt::cells_body(rows = c(3))) |>
  gtExtras::gt_theme_espn()


#stargazer_r(
#	list_of_models = list(ols1, ols2, ols3),
#	keep.stat=c("n", "rsq"), keep = c(x_var, "Constant"), dep.var.labels.include = FALSE, dep.var.caption = "",
#	column.labels = c("'no confounders'", "'with confounders'", "'with confounders interacted'")) #%>%
#cat(.,file= paste0(output, "ch21-foundfam-reg1.tex"))


# *************************************************************
# * EXACT MATCHING
# *****************************************************************
Hmisc::describe(data$management)
data <- data %>%
  mutate(
    # turn employee number into categories
    empbin5 = cut(emp_firm, quantile(emp_firm, seq(0,1,1/5)), include.lowest = TRUE, right = FALSE)
    # turn age into categories
    , agecat = (age_young == TRUE) + 2*(age_mid == TRUE) + 3*(age_old == TRUE) + 4*(age_unknown == TRUE)
  )

data |>
  dplyr::select(emp_firm,age_young,age_mid,age_old,age_unknown,agecat)

data_agg <- data %>%
  group_by(degree_nm_bins, agecat, competition, empbin5, industry, countrycode) %>%
  dplyr::summarise(
    n = n(), n0 = sum(1-foundfam_owned), n1 = sum(foundfam_owned),
    y0 = sum(management*(foundfam_owned == 0))/sum(1-foundfam_owned),
    y1 = sum(management*(foundfam_owned == 1))/sum(foundfam_owned)
  ) %>%
  ungroup()

# firms with/without exact match
data_agg %>%
  group_by(n0 == 0, n1 == 0) %>%
  summarise(n())

# random order just for the examples
set.seed(12345)
data_sample <- data_agg %>%
  sample_n(size = 340) %>%
  dplyr::select(industry, countrycode, degree_nm_bins, competition, agecat, empbin5, n1, n0, n)

# tidy
set.seed(12345)
data_sample <- data_agg %>%
  dplyr::slice_sample(n = 340) %>%
  dplyr::select(industry, countrycode, degree_nm_bins, competition, agecat, empbin5, n1, n0, n)


# examples with founder/family only
data_sample %>%
  .[1:19,] %>%
  filter(n1==1 & n0==0)

# examples with other only:
data_sample %>%
  .[1:19,] %>%
  filter(n1==0 & n0==1)

# examples of similar firms unmatched
data_sample %>%
  .[1:339,] %>%
  filter(countrycode == "us" & industry == "food" & n == 1) %>%
  arrange(countrycode, industry, degree_nm_bins, competition, agecat, empbin5, n)

# ATE/ATET
data_agg %>%
  filter(n0>0 & n1>0) %>%
  summarise(ATE = weighted.mean(y1-y0, n), ATET = weighted.mean(y1-y0, n1))

# tidy
set.seed(12345)
data_sample <- data_agg %>%
  dplyr::slice_sample(n = 340) %>%
  dplyr::select(industry, countrycode, degree_nm_bins, competition, agecat, empbin5, n1, n0, n)

# examples with founder/family only
data_sample %>%
  dplyr::slice(1:19) |>
  dplyr::filter(n1==1 & n0==0)

# examples with other only:
data_sample |>
  dplyr::slice(1:19) |>
  dplyr::filter(n1==0 & n0==1)

# examples of similar firms unmatched
data_sample %>%
  dplyr::slice(1:339) |>
  dplyr::filter(countrycode == "us" & industry == "food" & n == 1) %>%
  dplyr::arrange(countrycode, industry, degree_nm_bins, competition, agecat, empbin5, n)

# ATE/ATET
data_agg %>%
  dplyr::filter(n0>0 & n1>0) %>%
  dplyr::summarise(ATE = weighted.mean(y1-y0, n), ATET = weighted.mean(y1-y0, n1))

# *****************************************************************
# * Matching on the propensity score
# *****************************************************************

# NOTE: the R code calculates ATET with the estimand=="ATT" option

# Function only works with non-missing values and factor variables
data_pscore <- data %>%
  dplyr::select(all_of(c(y_var, x_var, control_vars, control_vars_to_interact))) %>%
  na.omit() %>% mutate( industry = factor( industry ),
                        countrycode = factor( countrycode ) )

# tidy
data_pscore <- data %>%
  dplyr::select(all_of(c(y_var, x_var, control_vars, control_vars_to_interact))) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    industry = factor( industry )
    , countrycode = factor( countrycode )
  )

# with all control vars -------------------------------------------------------

# Step 1 - Matching
formula_pscore1 <- as.formula(paste0(x_var, " ~ ",
                                     paste(c(control_vars, control_vars_to_interact), collapse = " + ")))

mod_match <- MatchIt::matchit(formula_pscore1,
                     data = data_pscore,
                     method = 'nearest', distance = 'logit', replace=TRUE, estimand="ATT")

summary(mod_match)

# Step 2 - restrict data to matched
data_match <- MatchIt::match.data(mod_match)

# Please note that nhe "number of matched observations" calculated by
# this code varies marginally from the one on p607 in the textbook.
dim(data_match)

# Step 3 - Estimate treatment effects
# NOTE: We use weights here,to account for control observations that were matchet to multiple treated osb
#       This is different from weights used to estimate ATE!
reg_match <- fixest::feols(management ~ foundfam_owned,
                   data = data_match,
                   weights = data_match$weights
)

out1 <- summary(reg_match)

ATET_PSME1 <- out1$coefficients[2]
ATET_PSME1_SE <- out1$se[2]


# with all controls + interactions -------------------------------------------------------

# Step 1 - Matching
formula_pscore2 <- as.formula(paste(x_var, " ~ " ,
                                    paste(control_vars_to_interact, collapse = ":"),
                                    " + (", paste(control_vars, collapse = "+"),")*(",
                                    paste(control_vars_to_interact, collapse = "+"),")",sep=""))

mod_match2 <- MatchIt::matchit(formula_pscore2,
                      data = data_pscore,
                      method = 'nearest', distance = 'logit', replace=TRUE, estimand="ATT")

summary(mod_match2)

# Step 2 - restrict data to matched
data_match2 <- MatchIt::match.data(mod_match2)

# Please note that nhe "number of matched observations" calculated by
# this code varies marginally from the one on p607 in the textbook.
dim(data_match2)

# Step 3 - Estimate treatment effects
# NOTE: We use weights here,to account for control observations that were matchet to multiple treated osb
#       This is different from weights used to estimate ATE!
reg_match2 <- fixest::feols(management ~ foundfam_owned,
                    data = data_match2, weights = data_match2$weights)

out2 <- summary(reg_match2)

ATET_PSME2 <- out2$coefficients[2]
ATET_PSME2_SE <- out2$se[2]


# *****************************************************************
# * CHECK common support
# *****************************************************************

# Country, cometition, industry
c1 <- gmodels::CrossTable(data$foundfam_owned, data$compet_moder, na.rm=T )
c2 <- gmodels::CrossTable(data$foundfam_owned, data$compet_strong, na.rm=T)

i <- gmodels::CrossTable(data$foundfam_owned, data$industry, na.rm=T)
c <- gmodels::CrossTable(data$foundfam_owned, data$countrycode, na.rm=T)


cbind(c1$prop.row, c2$prop.row, i$prop.row, c$prop.row)

# College Degree
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(degree_nm , na.rm=T),
            max = max(degree_nm , na.rm=T),
            p1 = quantile(degree_nm , probs = 0.01, na.rm=T),
            p5 = quantile(degree_nm , probs = 0.05, na.rm=T),
            p95 = quantile(degree_nm , probs = 0.95, na.rm=T),
            q99 = quantile(degree_nm, probs = 0.99, na.rm=T),
            n = n())

# Employment
data %>%
  group_by(foundfam_owned) %>%
  summarise(min = min(emp_firm , na.rm=T),
            max = max(emp_firm , na.rm=T),
            p1 = quantile(emp_firm , probs = 0.01, na.rm=T),
            p5 = quantile(emp_firm, probs = 0.05, na.rm=T),
            p95 = quantile(emp_firm, probs = 0.95, na.rm=T),
            q99 = quantile(emp_firm, probs = 0.99, na.rm=T),
            n = n())

# * common support check passed


# Q4 ----
# $$$$$$$$$
# qini

library(grf)
library(maq)

doubly_robust <- function(df, X, D, Y){
  ps <- # propensity score
    as.formula(paste(D, " ~ ", paste(X, collapse= "+"))) |>
    stats::glm( data = df, family = binomial() ) |>
    broom::augment(type.predict = "response", data = df) |>
    dplyr::pull(.fitted)

  lin_frml <- formula(paste(Y, " ~ ", paste(X, collapse= "+")))

  idx <- df[,D] |> dplyr::pull(1) == 0
  mu0 <- # mean response D == 0
    lm(lin_frml, data = df[idx,]) |>
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  idx <- df[,D] |> dplyr::pull(1) == 1
  mu1 <- # mean response D == 1
    lm(lin_frml, data = df[idx,]) |>
    broom::augment(type.predict = "response", newdata = df[,X]) |>
    dplyr::pull(.fitted)

  # convert treatment factor to integer | recast as vectors
  d <- df[,D] |> dplyr::pull(1) |> as.character() |> as.numeric()
  y <- df[,Y] |> dplyr::pull(1)

  mean( d*(y - mu1)/ps + mu1 ) -
    mean(( 1-d)*(y - mu0)/(1-ps) + mu0 )
}

doubly_robust_models <- function(df, X, D, Y){
  ps <- # propensity score
    as.formula(paste(D, " ~ ", paste(X, collapse= "+"))) |>
    stats::glm( data = df, family = binomial() )

  lin_frml <- formula(paste(Y, " ~ ", paste(X, collapse= "+")))

  idx <- df[,D] |> dplyr::pull(1) == 0
  mu0 <- # mean response D == 0
    lm(lin_frml, data = df[idx,])

  idx <- df[,D] |> dplyr::pull(1) == 1
  mu1 <- # mean response D == 1
    lm(lin_frml, data = df[idx,])

  # # convert treatment factor to integer | recast as vectors
  # d <- df[,D] |> dplyr::pull(1) |> as.character() |> as.numeric()
  # y <- df[,Y] |> dplyr::pull(1)
  #
  # mean( d*(y - mu1)/ps + mu1 ) -
  #   mean( (1-d)*(y - mu0)/(1-ps) + mu0 )

  return(
    list(p_score = ps, m0 = mu0, m1 = mu1)
  )
}

doubly_robust_predict <- function(df, X, mdls, cost = 1){
  ps <- # propensity score
    mdls$p_score |> broom::augment(type.predict = "response", data = df) |>
    dplyr::pull(.fitted)

  # mean response D == 0
  df$D = 0
  mu0 <- mdls$m0 |>
    broom::augment(type.predict = "response", newdata = df) |>
    dplyr::pull(.fitted)

  # mean response D == 1
  df$D = 1
  mu1 <- mdls$m1 |>
    broom::augment(type.predict = "response", newdata = df) |>
    dplyr::pull(.fitted)

  tibble::tibble(
    m0 = mu0, m1 = mu1, ps = ps, res = mu1/ps - mu0/ps
  ) |>
    dplyr::mutate(net = res - cost) |>
    dplyr::filter(net > 0)
}




n <- 2000
p <- 5
X <- matrix(rnorm(n * p), n, p)
W <- rbinom(n, 1, 0.5)
Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)

set.seed(8740)
dat <- matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("X",1:p)) ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    W = rbinom(n, 1, 0.5)
    , Y = pmax(X1, 0) * W + X2 + pmin(X3, 0) + rnorm(n)
    )

require(ggplot2)
dat |> ggplot(aes(x=W, y=Y)) + geom_point()


doubly_robust(dat, paste0("X",1:p), "W", "Y")
# 0.3242907
doubly_robust_predict(dat, paste0("X",1:p), "W", "Y")

splits     <- rsample::initial_split(dat, prop = 0.5)
train_data <- rsample::training(splits)
test_data  <- rsample::testing(splits)

doubly_robust(train_data, paste0("X",1:p), "W", "Y")
# 0.3242907
mdls <- doubly_robust_models(train_data, paste0("X",1:p), "W", "Y")

wsx <- doubly_robust_predict(test_data, X, mdls, cost = 0.5)
mean(wsx$res)

#!@!@!@!
set.seed(8740)
dat <- matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("X",1:p)) ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    W = rbinom(n, 1, 0.5)
    , Y = pmax(X1, 0) * W + X2 + pmin(X3, 0) + rnorm(n)
  )

dat <- matrix(rnorm(n * p), n, p, dimnames = list(NULL, paste0("X",1:p)) ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    W = rbinom(n, 1, 0.5)
    , Y = pmax(X1, 0) * W + X2 + pmin(X3, 0) + rnorm(n)
  )

base_mod <- lm(Y~., data = train_data)
base_mod <- lm(Y~W*X1 + X2 + X3, data = train_data) # 0.26409
base_mod <- lm(Y~W*X1 + X2 + X3 + X4 + X5, data = train_data) # 0.26471
doubly_robust(train_data, paste0("X",1:p), "W", "Y") # 0.2622673

stacked_data <- train_data |> dplyr::mutate(W=1) |>
  dplyr::bind_rows(
    train_data |> dplyr::mutate(W=0)
  )

base_mod |>
  broom::augment(newdata = stacked_data)|>
  dplyr::group_by(W) |>
  dplyr::summarize(mean_effect = mean(.fitted)) |>
  dplyr::mutate(contrast = mean_effect - dplyr::lag(mean_effect)) # 0.262

predictions |>
  dplyr::group_by(net) |>
  dplyr::summarize(mean_malaria_risk = mean(.fitted)) |>
  dplyr::mutate(contrast = mean_malaria_risk - dplyr::lag(mean_malaria_risk))



base_mod |>
  broom::augment(newdata = test_data |> dplyr::mutate(W = 1)) |>
  dplyr::select(r1 = .fitted) |>
  dplyr::bind_cols(
    base_mod |>
      broom::augment(newdata = test_data |> dplyr::mutate(W = 0)) |>
      dplyr::select(r0 = .fitted)
  ) |>
  dplyr::summarize(effect = mean(r1-r0))

wsx <- base_mod |>
  broom::augment(newdata = test_data |> dplyr::mutate(W = 1)) |>
  dplyr::select(r1 = .fitted) |>
  dplyr::bind_cols(
    base_mod |>
      broom::augment(newdata = test_data |> dplyr::mutate(W = 0)) |>
      dplyr::select(r0 = .fitted)
  ) |>
  dplyr::mutate(
    lift = r1 - r0 - 0.1
    , baseline_lift = base_mod$coefficients["W"] - 0.1
  ) |>
  dplyr::arrange(desc(lift)) |>
  tibble::rowid_to_column("ID") |>
  dplyr::mutate(
    cumulative_pct = ID / n,
    treated_cumsum = cumsum(r1),
    control_cumsum = cumsum(r0),
    baseline_cumsum = cumsum(baseline_lift),
    cumulative_uplift = cumsum(lift) #treated_cumsum - control_cumsum
  )

wsx |>
  dplyr::summarize(max_ordered = max(cumulative_uplift), max_random = max(baseline_cumsum))

wsx |>
  tidyr::pivot_longer(c(baseline_cumsum, cumulative_uplift)) |>
  ggplot(aes(x=cumulative_pct, y=value, color = name)) + geom_line()



wsx |> ggplot(aes(x=cumulative_pct, y=value, group = name)) + geom_point()

data <- data %>%
  mutate(
    cumulative_n = row_number(),
    cumulative_pct = cumulative_n / n,
    treated_cumsum = cumsum(treatment * outcome),
    control_cumsum = cumsum((1 - treatment) * outcome),
    cumulative_uplift = treated_cumsum - control_cumsum
  )

# Normalize cumulative uplift for Qini curve (optional)
data <- data %>%
  mutate(
    normalized_uplift = cumulative_uplift / max(cumulative_uplift)
  )

base_mod |>
  broom::augment(newdata = dat |> dplyr::mutate(W = 0))


# *&*&*&*&*&*&*& FROM CLAUDE

 Load required libraries
library(dplyr)
library(ggplot2)

# Set random seed for reproducibility
set.seed(123)

# Generate synthetic data
n <- 1000
data <- data.frame(
  customer_id = 1:n,
  propensity_score = runif(n),  # Random propensity scores
  treatment = rbinom(n, 1, 0.5),  # Random treatment assignment
  baseline_response = rbinom(n, 1, 0.3)  # Baseline response rate
)

# Simulate treatment effect (higher effect for customers with higher propensity)
data$outcome <- with(data,
  ifelse(treatment == 1,
    baseline_response + 0.2 * (propensity_score > 0.7),
    baseline_response
  )
)

# Function to calculate Qini curve points
calculate_qini <- function(data, n_points = 10) {
  # Sort by propensity score
  data <- data[order(-data$propensity_score), ]

  # Calculate incremental gains at different percentiles
  percentiles <- seq(0, 1, length.out = n_points)
  qini_points <- data.frame(
    percentile = percentiles,
    incremental_response = NA,
    random_incremental_response = NA
  )

  for(i in 1:length(percentiles)) {
    if(percentiles[i] == 0) {
      qini_points$incremental_response[i] <- 0
      qini_points$random_incremental_response[i] <- 0
      next
    }

    # Select top k% of observations
    k <- floor(nrow(data) * percentiles[i])
    subset <- data[1:k, ]

    # Calculate actual incremental response
    treated <- subset[subset$treatment == 1, ]
    control <- subset[subset$treatment == 0, ]

    # Calculate treatment effect
    treat_effect <- sum(treated$outcome) / nrow(treated) -
                   sum(control$outcome) / nrow(control)

    qini_points$incremental_response[i] <- treat_effect * k
    qini_points$random_incremental_response[i] <- treat_effect * k * percentiles[i]
  }

  return(qini_points)
}

# Calculate Qini curve points
qini_data <- calculate_qini(data)

# Calculate Qini coefficient
qini_coefficient <- with(qini_data, {
  actual_area <- sum(diff(percentile) * (incremental_response[-1] + incremental_response[-length(incremental_response)]) / 2)
  random_area <- sum(diff(percentile) * (random_incremental_response[-1] + random_incremental_response[-length(random_incremental_response)]) / 2)
  actual_area - random_area
})

# Plot Qini curve
ggplot(qini_data, aes(x = percentile)) +
  geom_line(aes(y = incremental_response, color = "Actual"), size = 1) +
  geom_line(aes(y = random_incremental_response, color = "Random"),
            linetype = "dashed", size = 1) +
  labs(title = "Qini Curve",
       x = "Percentage of population targeted",
       y = "Cumulative incremental response",
       color = "Model") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "blue", "Random" = "red"))

# Print Qini coefficient
print(paste("Qini Coefficient:", round(qini_coefficient, 4)))
Last edited just now

# Q5 ----

# Load required packages
library(causaldata)
library(MatchIt)
library(dplyr)
library(ggplot2)
library(cobalt)
library(sandwich)
library(lmtest)

# Load the NHEFS data
data(nhefs)


# Create binary treatment variable for smoking cessation
# 1 = quit smoking, 0 = continued smoking
nhefs <- nhefs %>%
  mutate(quit_smoking = ifelse(smkintensity82_71 == 0, 1, 0)) |>   #ifelse(smk82_71 == 0, 1, 0)) %>%
  # Remove missing values in key variables
  filter(!is.na(quit_smoking) & !is.na(wt82_71) & !is.na(age) &
           !is.na(wt71) & !is.na(smokeintensity) & !is.na(exercise))

# Perform matching using nearest neighbor matching
# We'll match on baseline characteristics
match_obj <- matchit(
  quit_smoking ~ age + wt71 + smokeintensity + exercise +
    education + sex + race,
  data = nhefs,
  method = "nearest",
  ratio = 1,
  caliper = 0.25
)

# Get summary of matching
print("Matching Summary:")
summary(match_obj)

# Extract matched data
matched_data <- match.data(match_obj)

# Create balance plot
love.plot(match_obj, binary = "std",
          abs = TRUE,
          var.order = "unadjusted",
          line = TRUE,
          limits = c(0, 0.8),
          title = "Covariate Balance Before and After Matching")

# Calculate treatment effect (weight change) in matched sample
model <- lm(wt82_71 ~ quit_smoking, data = matched_data, weights = weights)

# Get robust standard errors
robust_se <- sqrt(diag(vcovHC(model, type = "HC1")))
est <- coef(model)["quit_smoking"]
se <- robust_se["quit_smoking"]
t_stat <- est/se
p_val <- 2 * pt(-abs(t_stat), df = nrow(matched_data) - 2)

# Create results data frame
results <- data.frame(
  Estimate = est,
  SE = se,
  t_value = t_stat,
  p_value = p_val,
  CI_lower = est - 1.96 * se,
  CI_upper = est + 1.96 * se
)

# Print results
print("\nAverage Treatment Effect on the Treated (ATT) - Weight Change:")
print(results)

# Visualize treatment effect
p1 <- ggplot(matched_data, aes(x = factor(quit_smoking),
                               y = wt82_71,
                               fill = factor(quit_smoking))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Weight Change Distribution by Smoking Cessation Status",
       x = "Quit Smoking",
       y = "Weight Change (kg)",
       fill = "Quit Smoking") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                    labels = c("Continued Smoking", "Quit Smoking")) +
  theme_minimal()

# Additional balance checks
balance_stats <- matched_data %>%
  group_by(quit_smoking) %>%
  summarise(
    n = n(),
    age_mean = mean(age),
    baseline_weight = mean(wt71),
    smoking_intensity = mean(smokeintensity),
    exercise_mean = mean(exercise),
    education_mean = mean(education),
    prop_male = mean(sex == 1)
  )

print("\nBalance Statistics After Matching:")
print(balance_stats)

# Create density plots for key covariates
p2 <- matched_data %>%
  select(age, wt71, smokeintensity, quit_smoking) %>%
  tidyr::pivot_longer(cols = c(age, wt71, smokeintensity)) %>%
  ggplot(aes(x = value, fill = factor(quit_smoking))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distribution of Key Covariates After Matching",
       fill = "Quit Smoking") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                    labels = c("Continued Smoking", "Quit Smoking")) +
  theme_minimal()

# Calculate standardized differences for key covariates
std_diff <- matched_data %>%
  group_by(quit_smoking) %>%
  summarise(across(c(age, wt71, smokeintensity, exercise),
                   list(mean = mean, sd = sd))) %>%
  tidyr::pivot_wider(names_from = quit_smoking,
                     values_from = ends_with(c("mean", "sd"))) %>%
  mutate(across(everything(), ~replace_na(., 0)))

print("\nStandardized Differences for Key Covariates:")
print(std_diff)

# Print both plots
print(p1)
print(p2)

# Q5 ----

nhefs |>
  dplyr::group_by(quit_smoking) |>
  dplyr::summarize(mean_effect = mean(wt82_71)) |>
  dplyr::mutate(ATE = mean_effect - dplyr::lag( mean_effect) ) |> readr::write_csv('')

nhefs <-
  readr::read_csv('labs/solutions/data/nhefs_data.csv', show_col_types = FALSE) |>
  dplyr::select(wt82_71, quit_smoking, age, wt71, smokeintensity, exercise, education, sex, race)

nhefs_data <- nhefs |> dplyr::select(wt82_71, quit_smoking, age, wt71, smokeintensity, exercise, education, sex, race)

nhefs_data <-
  wt82_71 ~ quit_smoking

nhefs_data <- nhefs_data |> recipes::recipe(data=_,formula = wt82_71 ~ .) |>
  recipes::update_role(quit_smoking, new_role = 'treatment') |>
  recipes::step_normalize(age, wt71, smokeintensity) |>
  recipes::prep() |>
  recipes::bake(new_data=NULL)

treated   <- nhefs_data |> dplyr::filter(quit_smoking==1)
untreated <- nhefs_data |> dplyr::filter(quit_smoking==0)

mt0 <- # untreated knn model predicting recovery
  caret::knnreg(x = untreated |> dplyr::select(age, wt71, smokeintensity, exercise, education, sex, race), y = untreated$wt82_71, k=1)
mt1 <- # treated knn model predicting recovery
  caret::knnreg(x = treated |> dplyr::select(age, wt71, smokeintensity, exercise, education, sex, race), y = treated$wt82_71, k=1)

predicted <-
  # combine the treated and untreated matches
  c(
    # find matches for the treated looking at the untreated knn model
    treated |>
      tibble::rowid_to_column("ID") |>
      {\(y)split(y,y$ID)}() |> # hack for native pipe
      # split(.$ID) |>         # this vesion works with magrittr
      purrr::map(
        (\(x){
          x |>
            dplyr::mutate(
              match = predict( mt0, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
            )
        })
      )
    # find matches for the untreated looking at the treated knn model
    , untreated |>
      tibble::rowid_to_column("ID") |>
      {\(y)split(y,y$ID)}() |>
      # split(.$ID) |>
      purrr::map(
        (\(x){
          x |>
            dplyr::mutate(
              match = predict( mt1, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
            )
        })
      )
  ) |>
  # bind the treated and untreated data
  dplyr::bind_rows()

predicted |>
  dplyr::summarize("ATE (est)" = mean( (2*quit_smoking - 1) * (wt82_71 - match) ))

predicted |>
  dplyr::slice_head(n=5) |>
  gt::gt() |>
  gt::fmt_number(columns = c('sex','age','severity'), decimals = 6) |>
  gtExtras::gt_theme_espn() |>
  gt::as_raw_html()


ols0 <- lm(wt82_71 ~ age + wt71 + smokeintensity + exercise + education + sex + race, data = untreated)
ols1 <- lm(wt82_71 ~ age + wt71 + smokeintensity + exercise + education + sex + race, data = treated)

# find the units that match to the treated
treated_match_index <- # RANN::nn2 does Nearest Neighbour Search
  (RANN::nn2(mt0$learn$X, treated |> dplyr::select(age, wt71, smokeintensity, exercise, education, sex, race), k=1))$nn.idx |>
  as.vector()

# find the units that match to the untreated
untreated_match_index <- # RANN::nn2 does Nearest Neighbour Search
  (RANN::nn2(mt1$learn$X, untreated |> dplyr::select(age, wt71, smokeintensity, exercise, education, sex, race), k=1))$nn.idx |>
  as.vector()

predicted <-
  c(
    purrr::map2(
      .x =
        treated |> tibble::rowid_to_column("ID") |> {\(y)split(y,y$ID)}() # split(.$ID)
      , .y = treated_match_index
      , .f = (\(x,y){
        x |>
          dplyr::mutate(
            match = predict( mt0, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
            , bias_correct =
              predict( ols0, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] ) -
              predict( ols0, untreated[y,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
          )
      })
    )
    , purrr::map2(
      .x =
        untreated |> tibble::rowid_to_column("ID") |> {\(y)split(y,y$ID)}() # split(.$ID)
      , .y = untreated_match_index
      , .f = (\(x,y){
        x |>
          dplyr::mutate(
            match = predict( mt1, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
            , bias_correct =
              predict( ols1, x[1,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] ) -
              predict( ols1, treated[y,c('age', 'wt71', 'smokeintensity', 'exercise', 'education', 'sex', 'race')] )
          )
      })
    )
  ) |>
  # bind the treated and untreated data
  dplyr::bind_rows()

predicted |>
  dplyr::summarize(
    "ATE (est)" =
      mean( (2*quit_smoking - 1) * (wt82_71 - match - bias_correct) ))

