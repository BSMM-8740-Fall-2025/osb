# created April 30, 2025

require(ggplot2)
require(ggthemes)

# based on https://bookdown.org/paul/applied-causal-analysis/lab-2.html

# Directly import data from shared google folder into R
data <-
  readr::read_csv(
    "https://docs.google.com/uc?id=10h_5og14wbNHU-lapQaS1W6SBdzI7W6Z&export=download"
    , show_col_types = FALSE
  )

# Or download and import: data <- readr::read_csv("data-difference-in-differences.csv")
stargazer(data.frame(data), type = "html", summary = TRUE, out = "./www/public.html")

skimr::skim(data)
# ── Data Summary ────────────────────────
# Values
# Name                       data
# Number of rows             410
# Number of columns          18
# _______________________
# Column type frequency:
#   numeric                  18
# ________________________
# Group variables            None
#
# ── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# skim_variable             n_missing complete_rate    mean    sd   p0   p25   p50   p75  p100 hist
# 1 x_co_owned                        0         1      0.344  0.476 0     0     0     1     1    ▇▁▁▁▅
# 2 x_southern_nj                     0         1      0.227  0.419 0     0     0     0     1    ▇▁▁▁▂
# 3 x_central_nj                      0         1      0.154  0.361 0     0     0     0     1    ▇▁▁▁▂
# 4 x_northeast_philadelphia          0         1      0.0878 0.283 0     0     0     0     1    ▇▁▁▁▁
# 5 x_easton_philadelphia             0         1      0.105  0.307 0     0     0     0     1    ▇▁▁▁▁
# 6 x_st_wage_before                 20         0.951  4.62   0.347 4.25  4.25  4.5   4.95  5.75 ▇▃▃▁▁
# 7 x_st_wage_after                  21         0.949  5.00   0.253 4.25  5.05  5.05  5.05  6.25 ▁▇▁▁▁
# 8 x_hrs_open_weekday_before         0         1     14.4    2.81  7    12    15.5  16    24    ▁▅▇▁▁
# 9 x_hrs_open_weekday_after         11         0.973 14.5    2.75  8    12    15    16    24    ▂▅▇▁▁
# 10 y_ft_employment_before           12         0.971 21.0    9.75  5    14.6  19.5  24.5  85    ▇▅▁▁▁
# 11 y_ft_employment_after            14         0.966 21.1    9.09  0    14.5  20.5  26.5  60.5  ▂▇▅▁▁
# 12 d_nj                              0         1      0.807  0.395 0     1     1     1     1    ▂▁▁▁▇
# 13 d_pa                              0         1      0.193  0.395 0     0     0     0     1    ▇▁▁▁▂
# 14 x_burgerking                      0         1      0.417  0.494 0     0     0     1     1    ▇▁▁▁▆
# 15 x_kfc                             0         1      0.195  0.397 0     0     0     0     1    ▇▁▁▁▂
# 16 x_roys                            0         1      0.241  0.428 0     0     0     0     1    ▇▁▁▁▂
# 17 x_wendys                          0         1      0.146  0.354 0     0     0     0     1    ▇▁▁▁▂
# 18 x_closed_permanently              0         1      0.0146 0.120 0     0     0     0     1    ▇▁▁▁▁

#FIGURE 1
x_st_wage_before_nj <-
  data$x_st_wage_before[data$d_nj == 1]
x_st_wage_before_pa <-
  data$x_st_wage_before[data$d_pa == 1]

# Make a stacked bar plot - Plotly
# Set histogram bins
xbins <- list(start=4.20, end=5.60, size=0.1)

# Plotly histogram
p <- plotly::plot_ly(alpha = 0.6) |>
  plotly::add_histogram(x = x_st_wage_before_nj,
                xbins = xbins,
                histnorm = "percent",
                name = "Wage Before (New Jersey)") |>
  plotly::add_histogram(x = x_st_wage_before_pa,
                xbins = xbins,
                histnorm = "percent",
                name = "Wage Before (Pennsylvania)") |>
  plotly::layout(barmode = "group", title = "February 1992",
         xaxis = list(tickvals=seq(4.25, 5.55, 0.1),
                      title = "Wage in $ per hour"),
         yaxis = list(range = c(0, 50)),
         margin = list(b = 100,
                       l = 80,
                       r = 80,
                       t = 80,
                       pad = 0,
                       autoexpand = TRUE))
p


# WAGE AFTER
x_st_wage_after_nj <-
  data$x_st_wage_after[data$d_nj == 1]
x_st_wage_after_pa <-
  data$x_st_wage_after[data$d_pa == 1]

# Make a stacked bar plot - Plotly
xbins <- list(start=4.20,
              end=5.60,
              size=0.1)
p <- plotly::plot_ly(alpha = 0.6) |>
  plotly::add_histogram(x = x_st_wage_after_nj,
                xbins = xbins,
                histnorm = "percent",
                name = "Wage After (New Jersey)") |>
  plotly::add_histogram(x = x_st_wage_after_pa,
                xbins = xbins,
                histnorm = "percent",
                , name = "Wage After (Pennsylvania)") |>
  plotly::layout(barmode = "group", title = "November 1992",
         xaxis = list(tickvals=seq(4.25, 5.55, 0.1),
                      title = "Wage in $ per hour"),
         yaxis = list(range = c(0, 100)),
         margin = list(b = 100,
                       l = 80,
                       r = 80,
                       t = 80,
                       pad = 0,
                       autoexpand = TRUE))
p


# Table 3: Column 1-3, Row 1 (from left to right)

# 1st row: MEANs and SEs across subgroups
results <- data |>
  dplyr::group_by(d_nj) |> # group_by the treatment variable
  dplyr::select(d_nj, y_ft_employment_before) |> # only keep variable of interest
  dplyr::mutate(N = dplyr::n()) |>
  dplyr::group_by(N, .add = TRUE) |> # count number of rows
  dplyr::summarize(
    dplyr::across(
      everything()
      , list(mean = ~mean(.,na.rm=TRUE), var = ~var(.,na.rm=TRUE), na_sum = ~sum(is.na(.)))
      , .names = "{.fn}"
    )
    , .groups = "drop"
  ) |>
  dplyr::mutate(n = N - na_sum) |>
  dplyr::mutate(se = sqrt(var/n))

# Add row with differences
results <- dplyr::bind_rows(results, results[2,]-results[1,])
results$group<- c("Control (Pennsylvania)", "Treatment (New Jersey)", "Difference")
kableExtra::kable(results, digits=2)

results |>
  gt::gt("group") |>
  gt::fmt_number(-c(d_nj,N,na_sum,n),decimals = 2) |>
  gtExtras::gt_theme_espn()


data |> dplyr::group_by(d_nj) |>
  dplyr::summarise(mean.before = mean(y_ft_employment_before, na.rm=TRUE),
            mean.after = mean(y_ft_employment_after, na.rm=TRUE),
            var.before = var(y_ft_employment_before, na.rm=TRUE),
            var.after = var(y_ft_employment_after, na.rm=TRUE),
            n.before = sum(!is.na(y_ft_employment_before)),
            n.after = sum(!is.na(y_ft_employment_after))) |>
  dplyr::mutate(se.mean.before = sqrt(var.before/n.before)) |>
  dplyr::mutate(se.mean.after = sqrt(var.after/n.after))


# !!!!!!!!!!!!
data2 <-
  dplyr::select(data,
    y_ft_employment_after,
    y_ft_employment_before,
    d_nj,
    x_burgerking,
    x_kfc,
    x_roys,
    x_co_owned,
    x_st_wage_before,
    x_st_wage_after,
    x_closed_permanently,
    x_southern_nj,
    x_central_nj,
    x_northeast_philadelphia,
    x_easton_philadelphia
  ) |>
  dplyr::mutate(
    x_st_wage_after =
      dplyr::case_when(
        x_closed_permanently == 1 ~ NA_character_, # these stores get an NA
        TRUE ~ as.character(x_st_wage_after)
      )
  , x_st_wage_after = as.numeric(x_st_wage_after)
  ) |>
  na.omit()


# Model (i)/Column 1 (See exercise)

# Model (ii)/Column 2: Controls Chain/Ownership
fit2 <- lm((y_ft_employment_after-y_ft_employment_before) ~
             d_nj + x_burgerking + x_kfc + x_roys + x_co_owned,
           data = data2)
summary(fit2)

fit2 |> broom::tidy()



# ####################################
#
# from https://uclspp.github.io/PUBL0050/5-panel-data-and-difference-in-differences.html
#
# ####################################

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# 5.1.1 Refugees and support for the far right – Dinas et. al. (2018) ----

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!


# treatment – This is a binary variable which measures 1 if the observation is in the treatment group (a municipality that received many refugees) and the observation is in the post-treatment period (i.e. in 2016). Untreated units, and treatment units in the pre-treatment periods are coded as zero.
# ever_treated – This is a binary variable equal to TRUE in all periods for all treated municipalities, and equal to FALSE in all periods for all control municipalities.
# trarrprop – continuous (per capita number of refugees arriving in each municipality)
# gdvote – the outcome of interest. The Golden Dawn’s share of the vote. (Continuous)
# year – the year of the election. (Can take 4 values: 2012, 2013, 2015, and 2016)


load("slides/data/dinas_golden_dawn.Rdata")

post_treatment_data <- muni[muni$year == 2016,]
post_treat_mod <- lm(gdvote ~ treatment,data = post_treatment_data)

texreg::screenreg(post_treat_mod)

# Calculate the difference in means between treatment and control in the POST-treatment period
post_difference <-
  mean(muni$gdvote[muni$ever_treated == T & muni$year == 2016]) -
  mean(muni$gdvote[muni$ever_treated == F & muni$year == 2016])
post_difference

# Calculate the difference in means between treatment and control in the PRE-treatment period
pre_difference <-
  mean(muni$gdvote[muni$ever_treated == T & muni$year == 2015]) -
  mean(muni$gdvote[muni$ever_treated == F & muni$year == 2015])
pre_difference

# Calculate the difference-in-differences
diff_in_diff <- post_difference - pre_difference
diff_in_diff

# Subset the data to observations in either 2015 or 2016
muni_1516 <- muni[muni$year >= 2015,]

# Construct a dummy variable for the post-treatment period. Note that the way it is
# constructed the variable in R means it is stored as a logical vector (of TRUE and FALSE
# observations) rather than a numeric vector. R treats logical vectors as dummy variables,
# with TRUE being equal to 1 and FALSE being equal to 0.
muni_1516$post_treatment <- muni_1516$year == 2016

# Calculate the difference-in-differences
interaction_mod <- lm(gdvote ~ ever_treated * post_treatment, data = muni_1516)
interaction_mod2 <- lm(gdvote ~ ever_treated + post_treatment + treatment, data = muni_1516)

texreg::screenreg(list(interaction_mod,interaction_mod2))

# (5) ^^^^
group_period_averages <-
  stats::aggregate(
    x = muni$gdvote
    , by = list(muni$year, muni$ever_treated)
    , FUN = mean)
names(group_period_averages) <- c("year", "treated", "gdvote")

# ggplot
ggplot(muni,aes(x=year,y=gdvote,colour=ever_treated)) +
  stat_summary(fun="mean",geom = "point") +
  stat_summary(fun="mean",geom = "line") +
  stat_summary(fun.data="mean_se",geom = "errorbar", width =0.05)  +
  ylab("% Vote Golden Dawn") +
  xlab("") +
  ggtitle("Parallel Trends?") +
  # If you want you can play around with the color scheme,
  # uncomment/comment out any of the ones below
  scale_color_economist("",labels = c("Treatment","Control")) +
  # scale_color_fivethirtyeight("",labels = c("Treatment","Control")) +
  # scale_color_excel("",labels = c("Treatment","Control")) +
  # scale_color_colorblind("",labels = c("Treatment","Control")) +
  theme_clean() +
  theme(legend.background  = element_blank(),
        plot.background = element_rect(color= NA))


# (6) fixed effects
# NOTE: now using all the pre-treatment periods, rather than just 2015).
fe_mod <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + treatment,
             data  = muni)

# Because we added a dummy variable for each municipality, there are many coefficients in this
# model which we are not specifically interested in. Instead we are only interested in the
# coefficient associated with 'treatment'. We can look at only that coefficient by selecting
# based on rowname.
summary(fe_mod)$coefficients['treatment',]

fe_mod |> broom::tidy() |> dplyr::filter(term == 'treatment')


# (6) continuous treatment
fe_mod2 <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + trarrprop,
              data  = muni)

summary(fe_mod2)$coefficients['trarrprop',]

library(stargazer)

# Re-run interaction model with interaction already calculated before to help with
# table formatting
muni_1516$treatment <- muni_1516$ever_treated*muni_1516$post_treatment
interaction_mod <- lm(gdvote ~ ever_treated + post_treatment + treatment,
                      data = muni_1516)

stargazer::stargazer(post_treat_mod,
          interaction_mod,
          fe_mod,
          fe_mod2,
          type = 'text',
          column.labels = c("Naive DIGM","Regression DiD","Two-way FE"),
          column.separate = c(1,1,2),
          keep = c("treatment","trarrprop"),
          omit = c(2),
          covariate.labels = c("Binary Treatment","Continuous Treatment"),
          keep.stat = c("adj.rsq","n"),
          dep.var.labels = "Golden Dawn Vote Share")


# (8) parallel trends

# create a variable of time relative to treatment
library(tidyverse) # (for this I will use some tidyverse code - ignore the warnings)
library(broom)

muni <- muni |>
  dplyr::group_by(municipality) |> # group by unit of treatment
  dplyr::arrange(year) |> # arrange by time indicator
  dplyr::mutate(
    # difference between time indicator and first period where the treatment starts for the treatment group
    # negative values are the lags, positive ones the leads (note that there are no more than one lead here,
    # as we only have one post-treatment period)
    time_to_treat =
      dplyr::case_when(
        ever_treated == TRUE ~ (year - min(muni$year[muni$treatment == 1]))
        , TRUE ~ 0
      )
    ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # create factor version of time to treatment (period just before treatment as baseline)
    # you could choose another baseline if you want, it's a slightly arbitrary choice
    time_to_treat = forcats::fct_relevel(as.factor(time_to_treat),"-1"))

# the idea here is that you basically have a separate dummy variable for the treated observations at every
# period relative to when treatment actually started for that unit, which will calculate the difference between
# treatment and control groups (beyond the unit fixed effects) at every time period.

lagsleads_model <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + time_to_treat,
                      data = muni)
lagsleads <- broom::tidy(lagsleads_model) |>
  dplyr::filter(stringr::str_detect(term, "time_to_treat")) |>
  dplyr::mutate(time_to_treat = as.numeric(stringr::str_remove(term, "time_to_treat")),
         conf.low = estimate - 1.96*std.error,
         conf.high = estimate + 1.96*std.error,
         significant = abs(statistic) >=1.96) |>
  tibble::add_row(time_to_treat = -1, estimate = 0, significant = F)

ggplot(lagsleads, aes(x= time_to_treat, y = estimate)) +
  geom_vline(xintercept = -0.5) +
  geom_hline(yintercept = 0,  color = "lightgray") +
  geom_point(aes(color = significant)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = significant), width = 0.2) +
  geom_line() +
  labs(x = "Time to treatment", y = "DiD estimate", title = "Lags and leads plot") +
  scale_color_manual(values = c("darkgray","black")) +
  theme_clean() +
  lemon::coord_capped_cart(bottom="both",left="both") +
  theme(plot.background = element_rect(color=NA),
        axis.ticks.length = unit(2,"mm"),
        legend.position = "none")



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# 5.1.2 Minimum wages and employment – Card and Krueger (1994)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# (5.1.2 a)
library(foreign)
min_wage <- read.dta("slides/data/m_wage.dta")

# (5.1.2 b) Pre-Treatment
pre_treatment_difference <-
  mean(min_wage$wage_st[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$wage_st[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference

# Post-Treatment
post_treatment_difference <-
  mean(min_wage$wage_st2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$wage_st2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference

# Diff-in-diff (average wage)
difference_in_difference <- post_treatment_difference - pre_treatment_difference
difference_in_difference

# (5.1.2 c) Pre-Treatment
# difference-in-differences estimator for the outcome of interest (the number of full-time employees)
pre_treatment_difference <-
  mean(min_wage$emptot[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$emptot[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference

# Post-Treatment
post_treatment_difference <- mean(min_wage$emptot2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$emptot2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference

# Diff-in-diff
difference_in_difference <- post_treatment_difference - pre_treatment_difference
difference_in_difference

# (5.1.2 d) Pre-Treatment
# difference-in-differences estimator for the price of an average meal. Do restaurants that were subject to a wage increase raise their prices for fast–food?

pre_treatment_difference <-
  mean(min_wage$pmeal[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$pmeal[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference

post_treatment_difference <-
  mean(min_wage$pmeal2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$pmeal2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference

difference_in_difference <- post_treatment_difference -
  pre_treatment_difference

difference_in_difference
