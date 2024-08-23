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