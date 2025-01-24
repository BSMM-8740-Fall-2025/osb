# created January 8, 2025
require(ggplot2)

# Load HMDA data
data('CPS1985', package = "AER")
hmda <- CPS1985 |> tibble::as_tibble()

# Prepare data for analysis
# Looking at income differences between racial groups
lending_data <- hmda |>
  dplyr::mutate(
    minority = factor(ethnicity != "cauc"),
    log_income = log(wage)
  ) |>
  dplyr::select(wage, log_income, minority, education, experience, union)

model_a <- lm(wage ~ education, data = lending_data |> dplyr::filter(union=='yes'))
model_b <- lm(wage ~ education, data = lending_data |> dplyr::filter(union=='no'))
model_c <- lm(wage ~ union + education, data = lending_data) # pooled regression on education and union indicator
model_d <- lm(wage ~ education, data = lending_data) # pooled regression on education alone

lending_data |>
  ggplot(aes(x=education, y=wage, color=union)) +
  geom_point() +
  geom_smooth(method="lm", se= F, aes(colour = union, group = union)) +
  ylim(NA,30)+
  theme_minimal()

# Get mean characteristics (including intercept, since ceof includes the intercept)
X_mean_a <-
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='yes') |>
        dplyr::select(education)
    )
  )

X_mean_b <-
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='no') |>
        dplyr::select(education)
    )
  )

# Get coefficients
beta_a <- coef(model_a)
beta_b <- coef(model_b)
beta_d <- coef(model_d)

# Calculate decomposition (referenced to union group)
tibble::tibble(
  explained = sum((X_mean_a - X_mean_b) * beta_a)
  , unexplained = sum(X_mean_b * (beta_a - beta_b))
  , total_gap <- explained + unexplained
)

sum(X_mean_a * beta_a)
sum(X_mean_b * beta_b)


# compare to direct means
diffs <- lending_data |>
  dplyr::group_by(union) |>
  dplyr::summarize(mean_wage = mean(wage)) |>
  dplyr::mutate(diff = mean_wage - dplyr::lag(mean_wage))

mean_y_diff = diffs |> dplyr::filter(union=='yes') |> dplyr::pull(diff)

# union = 0
Gap1    <- sum(X_mean_b * (beta_a - beta_b))
Gap0    <- sum(X_mean_a * (beta_b - beta_a))
Gap1    <- sum(X_mean_a * (beta_b - beta_a))
Gap0    <- sum(X_mean_b * (beta_a - beta_b))
Gap_OLS <- model_c$coefficients[2]
Gap_p   <- sum((X_mean_a - X_mean_b) * beta_d) + sum(X_mean_a * (beta_a - beta_d)) + sum(X_mean_b * (beta_d - beta_b))


c(Gap0, Gap1, Gap_OLS, Gap_p, Gap_p - mean_y_diff)
#                      unionyes
# -2.296231  2.240677  2.286172  2.162897 -1.332268e-14

Gap <- function(coef, data = lending_data){
  mean_y_diff <- data |>
    dplyr::group_by(union) |>
    dplyr::summarize(mean_wage = mean(wage)) |>
    dplyr::mutate(diff = mean_wage - dplyr::lag(mean_wage)) |>
    dplyr::filter(union=='yes') |>
    dplyr::pull(diff)

  X_mean_a <- colMeans( lending_data |> dplyr::filter(union=='yes') |> dplyr::select(education) )
  X_mean_b <- colMeans( lending_data |> dplyr::filter(union=='no')  |> dplyr::select(education) )

  mean_y_diff - coef * (X_mean_a - X_mean_b)
}

Gap_ <- function(coef, data = lending_data){
  dat <- data |> dplyr::mutate(union = ifelse(union=="yes", 1, 0))
  cov_dy   <- cov(dat$union, dat$wage)
  cov_dx   <- cov(dat$union, dat$education)
  var_d    <- var(dat$union)

  cov_dy/var_d  - coef * (cov_dx/var_d)
}

c( Gap_(beta_b[2]), Gap_(beta_a[2]) )

c( Gap(beta_b[2]), Gap(beta_a[2]) )

prop <- lending_data |> dplyr::select(union) |> table(); prop <- (prop[2]/prop[1])
var1 <- lending_data |> dplyr::filter(union=='yes') |> dplyr::pull(education) |> var()
var0 <- lending_data |> dplyr::filter(union=='no')  |> dplyr::pull(education) |> var()

w1 <- prop * var1/(prop * var1 + (1-prop) * var0)
w0 <- (1-prop) * var0/(prop * var1 + (1-prop) * var0)

c(w1, w0, 1-w1, w0+w1)

w1 * Gap1 + w0 * Gap0

(w1 * Gap1) + -w0 * Gap0

lending_data_0 <- lending_data |> dplyr::mutate(union = ifelse(union=="yes", 1, 0))
cov_dy   <- cov(lending_data_0$union, lending_data_0$wage)
cov_dx   <- cov(lending_data_0$union, lending_data_0$education)
cov_xy_0 <-
  cov(
    lending_data |> dplyr::filter(union=='no')  |> dplyr::pull(education)
    , lending_data |> dplyr::filter(union=='no')  |> dplyr::pull(wage)
  )
cov_xy_1 <-
  cov(
    lending_data |> dplyr::filter(union=='yes')  |> dplyr::pull(education)
    , lending_data |> dplyr::filter(union=='yes')  |> dplyr::pull(wage)
  )
cov_xy   <-
  cov(
    lending_data |> dplyr::pull(education)
    , lending_data |> dplyr::pull(wage)
  )
var_d    <- var(lending_data_0$union)
var_x    <- var(lending_data_0$education)

Gap1_   <- cov_dy/var_d - (cov_dx/var_d) * (cov_xy_1/var1)
Gap0_   <- cov_dy/var_d - (cov_dx/var_d) * (cov_xy_0/var0)

w1 * Gap1_ + w0 * Gap0_
Gap_OLS
( (cov_dy * var_x) - (cov_dx*cov_xy) )/( (var_x * var_d) - cov_dx^2)
# ==========================================
# Fit separate regressions
model_a <-
  lm(wage ~ education + experience, data = lending_data |> dplyr::filter(union=='yes'))
model_b <-
  lm(wage ~ education + experience, data = lending_data |> dplyr::filter(union=='no'))

# Get mean characteristics (including intercept)
X_mean_a <-
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='yes') |>
        dplyr::select(education, experience)
    )
  )
X_mean_b <-
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='no') |>
        dplyr::select(education, experience)
    )
  )

# Get coefficients
beta_a <- coef(model_a)
beta_b <- coef(model_b)

# Calculate decomposition
tibble::tibble(
  explained = sum((X_mean_a - X_mean_b) * beta_a)
  , unexplained = sum(X_mean_b * (beta_a - beta_b))
  , total_gap <- explained + unexplained
)

# Perform Oaxaca-Blinder decomposition
decomp <-
  oaxaca::oaxaca(
    wage ~ education + experience | union
    , data =
      lending_data |>
      dplyr::mutate(union = dplyr::case_when(union=='yes'~0, TRUE ~1))
  )

# JAN 15, 2025 ----

require(ggplot2)
require(patchwork)

# (1) just indicator ----

# Load HMDA data
data('CPS1985', package = "AER")
hmda <- CPS1985 |> tibble::as_tibble()

# Prepare data for analysis
# Looking at income differences between racial groups
lending_data <- hmda |>
  dplyr::mutate(
    minority = factor(ethnicity != "cauc"),
    log_income = log(wage)
  ) |>
  dplyr::select(wage, log_income, minority, education, experience, union)

model_c <- lm(wage ~ union, data = lending_data) # pooled regression on education and union indicator
model_c |> broom::tidy()

gap_dat <- lending_data |> dplyr::group_by(union) |> dplyr::summarize(wage = mean(wage))
# theme_set(theme_bw(base_size = 18) + theme(legend.position = "top"))

p_1 <- lending_data |>
  ggplot(aes(x = union, y = wage, color=union)) +
  geom_point() +
  geom_point(data = gap_dat, shape = 15,  size = 5) +
  theme_minimal(base_size = 18) + ylim(NA, 30)

p_gt_1 <- model_c |> broom::tidy() |>
  dplyr::bind_cols( gap_dat |> dplyr::mutate(diff = wage - dplyr::lag(wage)) ) |>
  gt::gt("term") |>
  gt::cols_label(wage ~ "mean wage") |>
  gt::fmt_number(-term, decimals = 3) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = 'left',
      color = "red",
      weight = gt::px(2.5),
      style = "double"
    ),
    locations = gt::cells_body(columns = union)
  ) |>
  gt::sub_missing(columns = diff)

p_1 / wrap_table(p_gt_1, space = "fixed")

# A tibble: 2 × 8
# term        estimate std.error statistic   p.value union  wage  diff
#   <chr>          <dbl>     <dbl>     <dbl>     <dbl> <fct> <dbl> <dbl>
# 1 (Intercept)     8.64     0.243     35.6  6.86e-143 no     8.64 NA
# 2 unionyes        2.16     0.572      3.78 1.74e-  4 yes   10.8   2.16


# (2) indicator & 1 covariate ----

mean_y_diff = gap_dat |> dplyr::mutate(diff = wage - dplyr::lag(wage)) |> dplyr::filter(union=='yes') |> dplyr::pull(diff)

p_2 <- lending_data |>
  ggplot(aes(x=education, y=wage, color=union)) +
  geom_point() +
  geom_smooth(method="lm", se= F, aes(colour = union, group = union)) +
  ylim(NA,30)+
  theme_minimal()

model_a <- lm(wage ~ education, data = lending_data |> dplyr::filter(union=='yes'))
model_b <- lm(wage ~ education, data = lending_data |> dplyr::filter(union=='no'))
model_c <- lm(wage ~ union + education, data = lending_data) # pooled regression on education and union indicator
model_d <- lm(wage ~ education, data = lending_data) # pooled regression on education alone
model_d <- lm(union ~ education, data = lending_data |> dplyr::mutate(union = ifelse(union == 'yes',1,0))) # propensity score

lending_data |>
  dplyr::mutate(p = model_d$coefficients[1] + education * model_d$coefficients[2]) |>
  ggplot(aes(x=education, y=p)) +
  geom_point()


# Get mean characteristics (including intercept, since ceof includes the intercept)
X_mean_a <- # mean education, union members
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='yes') |>
        dplyr::select(education)
    )
  )

X_mean_b <- # mean education, non-union members
  c(1,
    colMeans(
      lending_data |>
        dplyr::filter(union=='no') |>
        dplyr::select(education)
    )
  )

# Get coefficients
beta_a <- coef(model_a)
beta_b <- coef(model_b)
beta_c <- coef(model_c)
beta_d <- coef(model_d)

# Calculate decomposition (referenced to union group)
tibble::tibble(
  explained = sum((X_mean_a - X_mean_b) * beta_a)
  , unexplained = sum(X_mean_b * (beta_a - beta_b))
  , total_gap <- explained + unexplained
)

sum(X_mean_a * beta_a)
sum(X_mean_b * beta_b)

# union
Gap1    <- sum(X_mean_b * (beta_a - beta_b))
Gap0    <- sum(X_mean_a * (beta_a - beta_b))
Gap_OLS <- model_c$coefficients[2]
Gap_p   <- sum((X_mean_a - X_mean_b) * beta_d) + sum(X_mean_a * (beta_a - beta_d)) + sum(X_mean_b * (beta_d - beta_b))

c(Gap0, Gap1, Gap_OLS, Gap_p, Gap_p - mean_y_diff)

# Calculate decomposition (referenced to union group)
p_gt_2 <- tibble::tibble(
  gap = c("Gap0", "Gap1", "Gap_OLS", "Gap_p")
  , explained =
    c( sum((X_mean_a - X_mean_b) * beta_b), sum((X_mean_a - X_mean_b) * beta_a ),  sum((X_mean_a - X_mean_b) * beta_c[-2] ),0 )
  , unexplained = c( Gap0, Gap1, Gap_OLS, Gap_p )
  , total_gap <- explained + unexplained
) |>
gt::gt("gap") |>
  gt::fmt_number(-gap, decimals = 3) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = 'left',
      color = "red",
      weight = gt::px(2.5),
      style = "double"
    ),
    locations = gt::cells_body(columns = "total_gap <- explained + unexplained")
  )

p_2 / wrap_table(p_gt_2, space = "fixed")

free(p_2, type = "label") / wrap_table(p_gt_2, space = "fixed")

free(p_2, type = "space", side = "l") / wrap_table(p_gt_2, space = "fixed")

# ^^^^^^^ weights
p_union <-
  lending_data |> dplyr::mutate(union = ifelse(union == 'yes',1,0)) |>
  dplyr::summarize(p = sum(union)/length(union)) |>
  dplyr::pull(p)

var_1_2 <- lending_data |>
  dplyr::summarise(
    var_0 = dplyr::case_when(union == 'no' ~ education, TRUE ~ NA) |> var(na.rm = TRUE)
    , var_1 = dplyr::case_when(union == 'yes' ~ education, TRUE ~ NA) |> var(na.rm = TRUE)
  ) |> unlist()

p_w1_2 <-  (p_union * var_1_2[2])/( (p_union * var_1_2[2]) + ((1-p_union) * var_1_2[1]) )
p_w0_2 <-  ((1-p_union) * var_1_2[1])/( (p_union * var_1_2[2]) + ((1-p_union) * var_1_2[1]) )
p_w0_2 + p_w1_2

p_w0_2*Gap0 + p_w1_2*Gap1

p_w1_2*Gap0 + p_w0_2*Gap1

#                                 unionyes
# 2.296231e+00  2.240677e+00  2.286172e+00  2.162897e+00 -1.332268e-14

Gap_ <- function(coef, data = lending_data){
  dat <- data |> dplyr::mutate(union = ifelse(union=="yes", 1, 0))
  cov_dy   <- cov(dat$union, dat$wage)
  cov_dx   <- cov(dat$union, dat$education)
  var_d    <- var(dat$union)

  cov_dy/var_d  - coef * (cov_dx/var_d)
}

c( Gap_(beta_b[2]), Gap_(beta_a[2]) )


# (2) indicator & propensity ----

model_d <-
  lm(
    union ~ education + + experience
    , data = lending_data |> dplyr::mutate(union = ifelse(union == 'yes',1,0))
  ) # propensity score

propensity_dat <-
  lending_data |> dplyr::mutate(union = ifelse(union == 'yes',1,0)) |>
  tibble::add_column(
    propensity =
      model_d |> predict(new_data = lending_data |> dplyr::mutate(union = ifelse(union == 'yes',1,0)))
  )

p_3 <- propensity_dat |>
  ggplot(aes(x=propensity, y=wage, color=union)) +
  geom_point() +
  geom_smooth(method="lm", se= F, aes(colour = union, group = union)) +
  ylim(NA,30)+
  theme_minimal()

P_mean_a <-
  c(1,
    colMeans(
      propensity_dat |>
        dplyr::filter(union==1) |>
        dplyr::select(propensity)
    )
  )

P_mean_b <-
  c(1,
    colMeans(
      propensity_dat |>
        dplyr::filter(union==0) |>
        dplyr::select(propensity)
    )
  )

p_model_a <- lm(wage ~ propensity, data = propensity_dat |> dplyr::filter(union==1))
p_model_b <- lm(wage ~ propensity, data = propensity_dat |> dplyr::filter(union==0))
p_model_c <- lm(wage ~ union + propensity, data = propensity_dat) # pooled regression on education and union indicator
p_model_d <- lm(wage ~ propensity, data = propensity_dat) # pooled regression on education alone

# Get coefficients
p_beta_a <- coef(p_model_a)
p_beta_b <- coef(p_model_b)
p_beta_c <- coef(p_model_c)
p_beta_d <- coef(p_model_d)

p_Gap1    <- sum(P_mean_b * (p_beta_a - p_beta_b))
p_Gap0    <- sum(P_mean_a * (p_beta_a - p_beta_b))
p_Gap_OLS <- p_model_c$coefficients[2]
p_Gap_p   <- sum((P_mean_a - P_mean_b) * p_beta_d) + sum(P_mean_a * (p_beta_a - p_beta_d)) + sum(P_mean_b * (p_beta_d - p_beta_b))

p_gt_3 <- tibble::tibble(
  gap = c("p_Gap0", "p_Gap1", "p_Gap_OLS", "p_Gap_p")
  , explained =
    c( sum((P_mean_a - P_mean_b) * p_beta_b), sum((P_mean_a - P_mean_b) * p_beta_a ),  sum((P_mean_a - P_mean_b) * p_beta_c[-2] ),0 )
  , unexplained = c( p_Gap0, p_Gap1, p_Gap_OLS, p_Gap_p )
  , total_gap <- explained + unexplained
)|>
  gt::gt("gap") |>
  gt::fmt_number(-gap, decimals = 3) |>
  gt::tab_style(
    style = gt::cell_borders(
      sides = 'left',
      color = "red",
      weight = gt::px(2.5),
      style = "double"
    ),
    locations = gt::cells_body(columns = "total_gap <- explained + unexplained")
  )

p_3 / wrap_table(p_gt_3, space = "fixed")

free(p_3, type = "label") / wrap_table(p_gt_3, space = "fixed")

free(p_3, type = "space", side = "l") / wrap_table(p_gt_3, space = "fixed")
