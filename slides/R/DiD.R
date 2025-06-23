# created April 30, 2025

# Libraries ----
require(ggplot2)
require(ggthemes)
require(stargazer)
require(foreign)
require(fixest)
require(bacondecomp)

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

# Calculate the difference-in-differences (note: 2x2)
interaction_mod <- lm(gdvote ~ ever_treated * post_treatment, data = muni_1516)
interaction_mod2 <- lm(gdvote ~ ever_treated + post_treatment + treatment, data = muni_1516)

texreg::screenreg(list(interaction_mod,interaction_mod2))

# (5) Assess the parallel trends assumption
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


# (6) fixed effects regression: the fixed-effect estimator for the diff-in-diff model requires
#     “two-way” fixed-effects, i.e. sets of dummy variables for a) units and b) time periods.
# NOTE: now using all the pre-treatment periods, rather than just 2015).
fe_mod <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + treatment,
             data  = muni)

# Because we added a dummy variable for each municipality, there are many coefficients in this
# model which we are not specifically interested in. Instead we are only interested in the
# coefficient associated with 'treatment'. We can look at only that coefficient by selecting
# based on rowname.
summary(fe_mod)$coefficients['treatment',]

fe_mod |> broom::tidy() |> dplyr::filter(term == 'treatment')


# (6) continuous treatment: swap the treatment variable for the trarrprop variable
fe_mod2 <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + trarrprop,
              data  = muni)

summary(fe_mod2)$coefficients['trarrprop',]
fe_mod2 |> broom::tidy() |> dplyr::filter(term == 'trarrprop')



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


# (8) parallel trends: re-explore the parallel trends assumption, but this time using lags and leads

# create a variable of time relative to treatment
# library(tidyverse) # (for this I will use some tidyverse code - ignore the warnings)
# library(broom)

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

# 5.1.2 Minimum wages and employment – Card and Krueger (1994) ----

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# (5.1.2 a)

min_wage <- foreign::read.dta("slides/data/m_wage.dta")


# nj – a dummy variable equal to 1 if the restaurant is located in New Jersey
# emptot – the total number of full-time employed people in the pre-treatment period
# emptot2 – the total number of full-time employed people in the post-treatment period
# wage_st – a variable measuring the average starting wage in the restaurant in the pre-treatment period
# wage_st2 – a variable measuring the average starting wage in the restaurant in the post-treatment period
# pmeal – a variable measuring the average price of a meal in the pre-treatment period
# pmeal2 – a variable measuring the average price of a meal in the post-treatment period
# co_owned – a dummy variable equal to 1 if the restaurant was co-owned
# bk – a dummy variable equal to 1 if the restaurant was a Burger King
# kfc – a dummy variable equal to 1 if the restaurant was a KFC
# wendys – a dummy variable equal to 1 if the restaurant was a Wendys


# (5.1.2 b) Pre-Treatment: the difference-in-difference estimate for the average wage in NJ and PA.
#           Noting that the wage is not the outcome of interest in this case,

pre_treatment_difference <-
  mean(min_wage$wage_st[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$wage_st[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference # -0.01799783

# Post-Treatment
post_treatment_difference <-
  mean(min_wage$wage_st2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$wage_st2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference # 0.4633844

# Diff-in-diff (average wage)
difference_in_difference <- post_treatment_difference - pre_treatment_difference
difference_in_difference # 0.4813823

# (5.1.2 c) Pre-Treatment: difference-in-differences estimator for the outcome of interest (the number of full-time employees)
pre_treatment_difference <-
  mean(min_wage$emptot[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$emptot[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference # -2.891761

# Post-Treatment
post_treatment_difference <-
  mean(min_wage$emptot2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$emptot2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference # -0.1381549

# Diff-in-diff
difference_in_difference <- post_treatment_difference - pre_treatment_difference
difference_in_difference # 2.753606

# (5.1.2 d) Pre-Treatment: difference-in-differences estimator for the price of an average meal.
#           Do restaurants that were subject to a wage increase raise their prices for fast–food?

pre_treatment_difference <-
  mean(min_wage$pmeal[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$pmeal[min_wage$nj ==0],  na.rm = T)
pre_treatment_difference # 0.3086927

post_treatment_difference <-
  mean(min_wage$pmeal2[min_wage$nj ==1],  na.rm = T) -
  mean(min_wage$pmeal2[min_wage$nj ==0],  na.rm = T)
post_treatment_difference # 0.3881344

difference_in_difference <- post_treatment_difference - pre_treatment_difference
difference_in_difference # 0.0794417

# (5.1.2 e) Pre-Treatment: difference-in-differences estimator for the price of an average meal.
# (i) Convert the dataset from a “wide” format to a “long” format
# (ii) Estimate the difference-in-differences using linear regression
# ( iii) Run two models: one which only includes the relevant variables to estimate the diff-in-diff,
#        and one which additionally includes restaurant-level covariates which do not vary over time

# Version 1
## Create two data.frames (one for pre-treatment and one for post-treatment period observations)
min_wage_feb <- min_wage[,c("nj","wage_st","emptot","kfc", "wendys","co_owned")]
min_wage_nov <- min_wage[,c("nj","wage_st2","emptot2","kfc", "wendys","co_owned")]

## Create a treatment period indicator
min_wage_feb$treatment_period <- 0
min_wage_nov$treatment_period <- 1

## Make sure the two data.frames have the same column names
colnames(min_wage_nov) <- colnames(min_wage_feb)

## Stack the data.frames on top of one another
min_wage_long <- rbind(min_wage_feb, min_wage_nov)

min_wage_long_alt <- dplyr::bind_rows(
  min_wage |> dplyr::select(nj,wage_st,emptot,kfc, wendys,co_owned) |> dplyr::mutate(treatment_period = 0)
  , min_wage |> dplyr::select(nj,wage_st=wage_st2,emptot=emptot2,kfc, wendys,co_owned) |> dplyr::mutate(treatment_period = 1)
)

## Estimate the simple diff-in-diff
did <- lm(emptot ~ nj * treatment_period, min_wage_long)

## Estimate the covariate adjusted diff-in-diff
didcov <- lm(emptot ~ nj * treatment_period + kfc + wendys + co_owned, min_wage_long)

# Let's have a look at only the interaction to get the ATT's
stargazer::stargazer(did, didcov,
          type='text',
          column.labels = c("Simple DiD","Covariate adj. DiD"),
          dep.var.labels = c("Total Number of FT Employed"),
          covariate.labels = c("ATT Estimate"),
          keep = c("nj:treatment_period1"),
          keep.stat = c("n","adj.rsq"),
          header = F)

dplyr::bind_cols(
  broom::tidy(did) |>
  dplyr::filter(term == "nj:treatment_period") |> dplyr::select(2:3) |>
  dplyr::rename_with(.fn = ~paste(.,"Simple DiD"))
, broom::tidy(didcov) |>
  dplyr::filter(term == "nj:treatment_period") |> dplyr::select(2:3) |>
  dplyr::rename_with(.fn = ~paste(.,"Covariate adj. DiD"))
) |>
  dplyr::mutate(across(dplyr::everything(), ~as.character(round(.,3)) )) |>
  gt::gt() |>
  gtExtras::gt_merge_stack(col1=`estimate Simple DiD`,col2=`std.error Simple DiD`) |>
  gtExtras::gt_merge_stack(col1=`estimate Covariate adj. DiD`,col2=`std.error Covariate adj. DiD`) |>
  gt::cols_label(
    `estimate Simple DiD` = "Simple DiD"
    , `estimate Covariate adj. DiD` = "Covariate adj. DiD"
  ) |>
  gt::tab_header(title = "ATT Estimates", subtitle = "Simple vs covariate adjusted DiD") |>
  gt::tab_footnote(
    footnote = "std. error underneath estimate",
    locations = gt::cells_column_labels(columns = 1)
  )

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Bacon Decomposition ----
# from https://asjadnaqvi.github.io/DiD/docs/code_r/06_bacon_r/

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

theme_set(
  theme_linedraw() +
    theme(
      panel.grid.minor = element_line(linetype = 3, linewidth = 0.1),
      panel.grid.major = element_line(linetype = 3, linewidth = 0.1)
    )
)

# data
dat4 = data.frame(
  id = rep(1:3, times = 10),
  tt = rep(1:10, each = 3)
) |>
  within({
    D = (id == 2 & tt >= 5) | (id == 3 & tt >= 8)
    btrue = ifelse(D & id == 3, 4, ifelse(D & id == 2, 2, 0))
    y = id + 1 * tt + btrue * D
  })

dat4 |> head()


ggplot(dat4, aes(x = tt, y = y, col = factor(id))) +
  geom_point() + geom_line() +
  geom_vline(xintercept = c(4.5, 7.5), lty = 2) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Time variable", y = "Outcome variable", col = "ID")

# estimating a simple TWFE model
mod_twfe <- feols(y ~ D | id + tt, dat4)
mod_twfe |> broom::tidy()

# We can visualize these four comparison sets as follows:
rbind(
  dat4 |> subset(id %in% c(1,2)) |> transform(role = ifelse(id==2, "Treatment", "Control"), comp = "1.1. Early vs Untreated"),
  dat4 |> subset(id %in% c(1,3)) |> transform(role = ifelse(id==3, "Treatment", "Control"), comp = "1.2. Late vs Untreated"),
  dat4 |> subset(id %in% c(2,3) & tt<8) |> transform(role = ifelse(id==2, "Treatment", "Control"), comp = "2.1. Early vs Untreated"),
  dat4 |> subset(id %in% c(2:3) & tt>4) |> transform(role = ifelse(id==3, "Treatment", "Control"), comp = "2.2. Late vs Untreated")
) |>
  ggplot(aes(tt, y, group = id, col = factor(id), lty = role)) +
  geom_point() + geom_line() +
  facet_wrap(~comp) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  scale_linetype_manual(values = c("Control" = 5, "Treatment" = 1)) +
  labs(x = "Time variable", y = "Ouroleome variable", col = "ID", lty = "Role")

# Goodman-Bacon decomposition
bgd <- bacon(y ~ D, dat4, id_var = "id", time_var = "tt")

# check that the weighted mean of these estimates is exactly the same as our earlier (naive) TWFE coefficient estimate
bgd_wm <- weighted.mean(bgd$estimate, bgd$weight)
bgd_wm

ggplot(bgd, aes(x = weight, y = estimate, shape = type, col = type)) +
  geom_hline(yintercept = bgd_wm, lty  = 2) +
  geom_point(size = 3) +
  labs(
    x = "Weight", y = "Estimate", shape = "Type", col = "Type",
    title = "Bacon-Goodman decomposition example",
    caption = "Note: The horizontal dotted line depicts the full TWFE estimate."
  )


# translating from stata
units <- 3
start <- 1
end <- 10
time <- end - start + 1
obsv <- units * time

# Create data frame with observations
df <- data.frame(obs = 1:obsv)

# Create panel ID and time variables
df$id <- rep(1:units, each = time)
df$t <- rep(start:end, times = units)

# Sort data by id and t
df <- df[order(df$id, df$t), ]
rownames(df) <- NULL

# Label variables (using attributes in R)
# attr(df$id, "label") <- "Panel variable"
# attr(df$t, "label") <- "Time variable"

# Create treatment variable
df$D <- 0
df$D[df$id == 2 & df$t >= 5] <- 1
df$D[df$id == 3 & df$t >= 8] <- 1
#attr(df$D, "label") <- "Treated"



# Create outcome variable
df$Y <- 0
df$Y[df$id == 2 & df$t >= 5] <- df$D[df$id == 2 & df$t >= 5] * 2
df$Y[df$id == 3 & df$t >= 8] <- df$D[df$id == 3 & df$t >= 8] * 4
attr(df$Y, "label") <- "Outcome variable"

# df <- df |> dplyr::rename(
#   "Treated" = D
#   , "Panel variable" = id
#   , "Time variable" = t
#   , "Outcome variable" = Y
# )

# Equivalent to xtset in Stata would be using plm package
# But for data preparation, we don't need it yet
# If you need to use panel data models:
# library(plm)
# pdata <- pdata.frame(df, index = c("id", "t"))


# Assuming df is the data frame we created earlier

# Create the plot
ggplot(df, aes(x = t, y = Y, color = factor(id), group = factor(id))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = c(4.5, 7.5), linetype = "dashed") +
  scale_x_continuous(breaks = 1:10) +
  scale_color_discrete(name = "", labels = c("id=1", "id=2", "id=3")) +
  theme_minimal() +
  labs(x = "Time", y = "Outcome variable") +
  theme(legend.position = "bottom")

library(plm)
library(fixest)

# Equivalent to: xtreg Y D i.t, fe
# Using plm package
plm_model <- plm::plm(Y ~ D + factor(t),
                 data = df,
                 index = c("id", "t"),
                 model = "within")
summary(plm_model)
plm_model$coefficients[1]

# Equivalent to: reghdfe Y D, absorb(id t)
# Using fixest package
fixest_model <- feols(Y ~ D | id + t, data = df)
summary(fixest_model)

# Goodman-Bacon decomposition
bgd <- bacon(Y ~ D, df, id_var = "id", time_var = "t")

# check that the weighted mean of these estimates is exactly the same as our earlier (naive) TWFE coefficient estimate
bgd_wm <- weighted.mean(bgd$estimate, bgd$weight)
bgd_wm

ggplot(bgd, aes(x = weight, y = estimate, shape = type, col = type)) +
  geom_hline(yintercept = bgd_wm, lty  = 2) +
  geom_point(size = 3) +
  labs(
    x = "Weight", y = "Estimate", shape = "Type", col = "Type",
    title = "Bacon-Goodman decomposition example",
    caption = "Note: The horizontal dotted line depicts the full TWFE estimate."
  )

# Calculate overall mean of D
df$d_barbar <- mean(df$D)

# Calculate mean of D by id
df <- df |>
  dplyr::group_by(id) |>
  dplyr::mutate(d_meani = mean(D)) |>
  dplyr::ungroup()

# Calculate mean of D by t
df <- df |>
  dplyr::group_by(t) |>
  dplyr::mutate(d_meant = mean(D)) |>
  dplyr::ungroup()

# Calculate d_tilde using the formula
df$d_tilde <- (df$D - df$d_meani) - (df$d_meant - df$d_barbar)

# Calculate squared d_tilde
df$d_tilde_sq <- df$d_tilde^2

data_df <-
  df |>
  dplyr::mutate(d_barbar = mean(D)) |>  # Calculate overall mean of D
  # Calculate mean of D by id
  dplyr::group_by(id) |>
  dplyr::mutate(d_meani = mean(D)) |>
  dplyr::ungroup() |>
  # Calculate mean of D by t
  dplyr::group_by(t) |>
  dplyr::mutate(d_meant = mean(D)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    # Calculate d_tilde using the formula
    d_tilde = (D - d_meani) - (d_meant - d_barbar)
    # Calculate squared d_tilde
    , d_tilde_sq = d_tilde^2
  )
data_df |>
  dplyr::summarize(
    num = sum(Y * d_tilde)
    , denom = sum(d_tilde_sq)
    , D = num/denom) |> dplyr::pull(D)

# Run fixed effects regression with time dummies
fe_model <- plm::plm(D ~ factor(t),
                data = data_df,
                index = c("id", "t"),
                model = "within")
summary(fe_model)

# Get residuals (equivalent to predict double Dtilde, e)
df$Dtilde <- residuals(fe_model)

# Calculate variance of Dtilde (equivalent to scalar VD calculation)
Dtilde_summary <- summary(df$Dtilde)
n <- length(df$Dtilde)
VD <- ((n - 1) / n) * var(df$Dtilde)

# Print the value
print(paste("VD =", VD))

# So where do TWFE regressions go wrong? ----
rm(df)

# Define parameters
units <- 30
start <- 1
end <- 60
time <- end - start + 1
obsv <- units * time

# Create data frame with observations
df <- data.frame(obs = 1:obsv)

# Create panel ID and time variables
df$id <- rep(1:units, each = time)
df$t <- rep(start:end, times = units)

# Sort data by id and t
df <- df[order(df$id, df$t), ]
rownames(df) <- NULL

# Label variables (using attributes in R)
attr(df$id, "label") <- "Panel variable"
attr(df$t, "label") <- "Time variable"

# For panel data analysis (equivalent to xtset)
# library(plm)
# pdata <- pdata.frame(df, index = c("id", "t"))

# Set seed for reproducibility
set.seed(13082021)

# Create new variables (or reset if they exist)
df$Y <- 0       # outcome variable
df$D <- 0       # intervention variable
df$cohort <- NA # total treatment variables
df$effect <- NA # treatment effect size
df$timing <- NA # when the treatment happens for each cohort

# Assign cohorts (0-5) randomly to each ID
for (x in unique(df$id)) {
  chrt <- sample(0:5, 1)  # equivalent to runiformint(0,5)
  df$cohort[df$id == x] <- chrt
}

# For each cohort, assign effect size and timing
for (x in unique(df$cohort)) {
  # (a) effect - random integer between 2 and 10
  eff <- sample(2:10, 1)  # equivalent to runiformint(2,10)
  df$effect[df$cohort == x] <- eff

  # (b) timing - random integer between start+5 and end-5
  treatment_time <- sample((start + 5):(end - 5), 1)  # equivalent to runiformint(start+5, end-5)
  df$timing[df$cohort == x] <- treatment_time

  # Assign treatment indicator
  df$D[df$cohort == x & df$t >= treatment_time] <- 1
}

# Create outcome variable with treatment effect
df$Y <- df$id + df$t + ifelse(df$D == 1, df$effect * (df$t - df$timing), 0)

# Get unique cohort values and IDs
cohorts <- unique(df$cohort)
ids <- unique(df$id)

# Create a color mapping based on cohort
# We'll use a predefined color palette similar to tableau
# First, create a mapping from id to color based on cohort
id_colors <- df |>
  dplyr::select(id, cohort) |>
  dplyr::distinct() |>
  dplyr::mutate(color_index = cohort + 1)

# Create the plot
ggplot(df, aes(x = t, y = Y, group = id, color = factor(id))) +
  geom_line(linewidth = 0.2) +  # equivalent to lw(vthin)
  scale_color_manual(values = scales::hue_pal()(length(ids))) +
  theme_minimal() +
  theme(legend.position = "none") +  # equivalent to legend(off)
  labs(x = "Time", y = "Outcome")

library(ggthemes)

ggplot(df, aes(x = t, y = Y, group = id, color = factor(cohort))) +
  geom_line(linewidth = 0.2) +
  scale_color_tableau() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Time", y = "Outcome")

# Equivalent to: xtreg Y i.t D, fe
# Using plm package
plm_model <- plm::plm(Y ~ factor(t) + D,
                 data = df,
                 index = c("id", "t"),
                 model = "within")
summary(plm_model)

# Equivalent to: reghdfe Y D, absorb(id t)
# Using fixest package
fixest_model <- feols(Y ~ D | id + t, data = df)
summary(fixest_model)

# Goodman-Bacon decomposition
bgd <- bacon(Y ~ D, df, id_var = "id", time_var = "t")

# check that the weighted mean of these estimates is exactly the same as our earlier (naive) TWFE coefficient estimate
bgd_wm <- weighted.mean(bgd$estimate, bgd$weight)
bgd_wm

ggplot(bgd, aes(x = weight, y = estimate, shape = type, col = type)) +
  geom_hline(yintercept = bgd_wm, lty  = 2) +
  geom_point(size = 3) +
  labs(
    x = "Weight", y = "Estimate", shape = "Type", col = "Type",
    title = "Bacon-Goodman decomposition example",
    caption = "Note: The horizontal dotted line depicts the full TWFE estimate."
  )

bgd |> dplyr::arrange(estimate )
(bgd$estimate * bgd$weight) |> sum()


# The fix
# Load required packages
library(fixest)
library(ggplot2)
library(data.table)
library(dplyr)

# First, create relative time variable for event study
# This measures time relative to treatment
df <- df |>
  dplyr::group_by(id) |>
  dplyr::mutate(rel_time = t - timing) |>
  dplyr::ungroup()

# Convert to data.table for faster processing
data.table::setDT(df)

# Create dummies for relative time
# Let's use a window of -10 to +20 relative periods
max_rel_time <- 20
min_rel_time <- -10

# Code all periods before min_rel_time as min_rel_time
# Code all periods after max_rel_time as max_rel_time
df[rel_time < min_rel_time, rel_time := min_rel_time]
df[rel_time > max_rel_time, rel_time := max_rel_time]

# Run event study regression
# Omit rel_time = -1 as the reference period
event_study <- feols(Y ~ i(rel_time, ref = -1) | id + t,
                     data = df[rel_time >= min_rel_time & rel_time <= max_rel_time])

# Create data frame for plotting
es_results <- data.frame(
  rel_time = setdiff(min_rel_time:max_rel_time,c(-1)), #min_rel_time:max_rel_time,
  coef = coef(event_study),
  se = se(event_study)
)

# Add confidence intervals
es_results$ci_lower <- es_results$coef - 1.96 * es_results$se
es_results$ci_upper <- es_results$coef + 1.96 * es_results$se

# Create plot
ggplot(es_results, aes(x = rel_time, y = coef)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(x = "Time Relative to Treatment",
       y = "Treatment Effect",
       title = "Event Study: Treatment Effect Dynamics") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted")

# event_study_mpdta.R

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Event Study using fixest + mpdta ----

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# 1. Install and Load Packages
packages <- c("fixest", "did", "ggplot2")
installed <- rownames(installed.packages())

for (pkg in packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# 2. Load Dataset
# A dataset containing (the log of) teen employment in 500 counties in the U.S. from 2004 to 2007.
# This is a subset of the dataset used in Callaway and Sant'Anna (2021).
data(mpdta, package = "did")
head(mpdta)

# 3. Create Event-Time Variable
# mpdta$rel_year <- mpdta$year - mpdta$first.treat
mpdta <- mpdta |>
  # Create Event-Time Variable
  dplyr::mutate(rel_year = year - first.treat) |>
  dplyr::filter(dplyr::between(rel_year, -5, 5))

# 4. Keep Event Times in Reasonable Range
mpdta <- subset(mpdta, rel_year >= -5 & rel_year <= 5)

# 5. Estimate Event Study Regression
event_model <- fixest::feols(
  lemp ~ i(rel_year, treat, ref = -1) | countyreal + year,
  data = mpdta
)

# 6. View Estimates
print("Event Study Estimates:")
fixest::etable(event_model)

# 7. Plot Dynamic Effects
fixest::iplot(
  event_model,
  ref.line = 0,
  xlab = "Years Relative to Treatment",
  ylab = "Estimated Effect on Log Employment",
  main = "Event Study: Dynamic Effects of Treatment"
)

# The parallel trends assumption holds if:
#   All pre-treatment event-time coefficients (\beta_k for k < 0) are statistically indistinguishable from zero.

summary(event_model)

# If all these:
# - Have small coefficient magnitudes,
# - Are not statistically significant (e.g., p > 0.05),
# - Have confidence intervals containing 0,

# Then the data is consistent with parallel trends.

# Run Wald test for pre-treatment coefficients (rel_year < 0)
fixest::wald(event_model, keep = "rel_year::(-5|-4|-3|-2)")

# - If the p-value is large (e.g., > 0.10), fail to reject H_0: supports parallel trends.
# - If the p-value is small, reject H_0: evidence of pre-trends → DiD assumptions may not hold.

# Auto-detect all rel_year < 0 with regex
fixest::wald(event_model, keep = "rel_year::-\\d")

# 8. Optional: Save Plot
# ggsave("event_study_plot.png", width = 8, height = 6)



# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Key ideas from Ishimaru (2022) ----

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Install required packages
install.packages("fixest")
install.packages("did")
install.packages("ggplot2")

library(fixest)
library(did)
library(ggplot2)

# %%%%%%%%%%%%%%%%%%%%%%
# Simulate Panel Dataset
# %%%%%%%%%%%%%%%%%%%%%%
set.seed(123)
n_groups <- 130
n_periods <- 6
group_ids <- 1:n_groups
time_periods <- 1:n_periods

panel <- expand.grid(id = group_ids, t = time_periods)
panel <- panel[order(panel$id, panel$t), ]

# Assign cohorts
panel$cohort <- NA
panel$cohort[panel$id <= 50] <- 3  # early treated
panel$cohort[panel$id > 50 & panel$id <= 100] <- 5  # late treated
panel$cohort[panel$id > 100] <- Inf  # never treated

# Treatment indicator
panel$treated <- as.numeric(panel$t >= panel$cohort)

# Simulate untreated outcomes
panel$y0 <- 5 + 0.5 * panel$t + rnorm(nrow(panel), 0, 1)

# Heterogeneous effects: early = 2, late = 4, control = 0
panel$att <- with(panel, ifelse(t >= cohort & cohort == 3, 2,
                                ifelse(t >= cohort & cohort == 5, 4, 0)))

panel$y <- panel$y0 + panel$att

# %%%%%%%%%%%%%%%%%%%%%%
# TWFE Event Study Estimate
# %%%%%%%%%%%%%%%%%%%%%%
# Reference period is k = -1
twfe_model <- fixest::feols(y ~ i(t - cohort, treated, ref = -1) | id + t, data = panel)
twfe_event_study <- broom::tidy(twfe_model)

# %%%%%%%%%%%%%%%%%%%%%%
# Callaway & Sant’Anna Estimator
# %%%%%%%%%%%%%%%%%%%%%%
att_gt <- did::att_gt(yname = "y",
                 tname = "t",
                 idname = "id",
                 gname = "cohort",
                 data = panel,
                 est_method = "dr")

agg_effects <- did::aggte(att_gt, type = "dynamic")

# %%%%%%%%%%%%%%%%%%%%%%
# Plot Callaway & Sant’Anna ATTs
# %%%%%%%%%%%%%%%%%%%%%%
plot(agg_effects, main = "Event Study: Callaway & Sant’Anna (2021)")

# %%%%%%%%%%%%%%%%%%%%%%
# View & Compare TWFE Results
# %%%%%%%%%%%%%%%%%%%%%%
print("TWFE Event Study Coefficients:")
print(twfe_event_study)
