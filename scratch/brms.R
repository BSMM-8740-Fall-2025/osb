# created October 22, 2024

# check if 'librarian' is installed and if not, install it
if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(
  tidyverse, broom, rsample, ggdag, causaldata, magrittr, gt, gtExtras, halfmoon, ggokabeito, ggplot2, r-causal/propensity, r-causal/causalworkshop, survey, cardx, gtsummary,
  brms, rmcelreath/rethinking, shinystan, patchwork, tidybayes, michael-franke/aida-package, michael-franke/faintr
  )

# set the default theme for plotting
theme_set(theme_bw(base_size = 18) + theme(legend.position = "top"))

# ===============================

data(Howell1)
d <- Howell1
d2 <-
  d |>
  filter(age >= 18)

brms::brm()

# b4.1 ----
b4.1 <-
  brm(data = d2,
      family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.01")

rethinking::plot(b4.1)

shinystan::launch_shinystan(b4.1)

b4.1$fit

summary(b4.1, prob = .89)
summary(b4.1)$fixed
vcov(b4.1)

post <- brms::as_draws_df(b4.1)
select(post, b_Intercept:sigma) |> cov()

post |>
  select(b_Intercept, sigma) |>
  cor()

# b4.3 ----
b4.3 <-
  brm(data = d2,
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.03")

post <- as_draws_df(b4.3)
rethinking::plot(b4.3)
brms::posterior_summary(b4.3)[1:3, ]

brms::as_draws_df(b4.3) |>
  select(b_Intercept:sigma) |>
  cor() |>
  round(digits = 2)

d2 <-
  d2 |>
  mutate(weight_c = weight - mean(weight))


b4.4 <-
  brm(data = d2,
      family = gaussian,
      height ~ 1 + weight_c,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.04")

brms::as_draws_df(b4.4) |>
  select(b_Intercept:sigma) |>
  cor() |>
  round(digits = 2)

rethinking::pairs(b4.4)

brms::posterior_summary(b4.4)[1:3, ]

d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_abline(intercept = brms::fixef(b4.3)[1],
              slope     = brms::fixef(b4.3)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

d2 %>%
  ggplot(aes(x = weight, y = height)) +
  geom_abline(intercept = brms::fixef(b4.3)[1],
              slope     = brms::fixef(b4.3)[2]) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  theme_bw() +
  theme(panel.grid = element_blank())

#
n <- 10

b4.3_010 <-
  brm(data = d2 %>%
        dplyr::slice(1:n),  # note our tricky use of `n` and `slice()`
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.03_010")

n <- 50

b4.3_050 <-
  brm(data = d2 %>%
        dplyr::slice(1:n),
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.03_050")

n <- 150

b4.3_150 <-
  brm(data = d2 %>%
        dplyr::slice(1:n),
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.03_150")

n <- 352

b4.3_352 <-
  brm(data = d2 %>%
        dplyr::slice(1:n),
      family = gaussian,
      height ~ 1 + weight,
      prior = c(prior(normal(178, 100), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(uniform(0, 50), class = sigma, ub = 50)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4,
      file = "scratch/fits/b04.03_352")


post010 <- as_draws_df(b4.3_010)
post050 <- as_draws_df(b4.3_050)
post150 <- as_draws_df(b4.3_150)
post352 <- as_draws_df(b4.3_352)

p1 <-
  ggplot(data = d2 %>% dplyr::slice(1:10),
         aes(x = weight, y = height)) +
  geom_abline(data = post010 %>% dplyr::slice(1:20),
              aes(intercept = b_Intercept, slope = b_weight, group = .draw),
              linewidth = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 10")

p2 <-
  ggplot(data = d2 %>% dplyr::slice(1:50),
         aes(x = weight, y = height)) +
  geom_abline(data = post050 %>% dplyr::slice(1:20),
              aes(intercept = b_Intercept, slope = b_weight, group = .draw),
              linewidth = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 50")

p3 <-
  ggplot(data = d2 %>% dplyr::slice(1:150),
         aes(x = weight, y = height)) +
  geom_abline(data = post150 %>% dplyr::slice(1:20),
              aes(intercept = b_Intercept, slope = b_weight, group = .draw),
              linewidth = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 150")

p4 <-
  ggplot(data = d2,
         aes(x = weight, y = height)) +
  geom_abline(data = post352 %>% dplyr::slice(1:20),
              aes(intercept = b_Intercept, slope = b_weight, group = .draw),
              linewidth = 1/3, alpha = .3) +
  geom_point(shape = 1, size = 2, color = "royalblue") +
  coord_cartesian(xlim = range(d2$weight),
                  ylim = range(d2$height)) +
  labs(subtitle = "N = 352")

# Now we can combine the ggplots with patchwork syntax to make the full version of Figure 4.5.

(p1 + p2 + p3 + p4) &
  theme_bw() &
  theme(panel.grid = element_blank())


mu_at_50 <-
  post %>%
  transmute(mu_at_50 = b_Intercept + b_weight * 50)

mu_at_50 %>%
  ggplot(aes(x = mu_at_50)) +
  geom_density(linewidth = 0, fill = "royalblue") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = expression(mu["height | weight = 50"])) +
  theme_classic()

tidybayes::mean_hdi(mu_at_50[,1], .width = c(.89, .95))

mu_at_50 %>%
  ggplot(aes(x = mu_at_50, y = 0)) +
  tidybayes::stat_halfeye(point_interval = mode_hdi, .width = .95,
               fill = "royalblue") +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(mu["height | weight = 50"])) +
  theme_classic()

mu <- stats::fitted(b4.3, summary = F)

weight_seq <- tibble(weight = seq(from = 25, to = 70, by = 1))
mu <-
  stats::fitted(b4.3,
         summary = F,
         newdata = weight_seq) %>%
  as_tibble() %>%
  # here we name the columns after the `weight` values from which they were computed
  set_names(25:70) %>%
  mutate(iter = 1:n())

mu <-
  mu %>%
  gather(weight, height, -iter) %>%
  # we might reformat `weight` to numerals
  mutate(weight = as.numeric(weight))

pred_height <-
  stats::predict(b4.3,
          newdata = weight_seq) %>%
  as_tibble() %>%
  bind_cols(weight_seq)

pred_height %>%
  dplyr::slice(1:6)


# =======================
price_volume_data <- data.table::fread(here::here("scratch/dat","price_data.csv"))

price_volume_data$`Volume Weighted Price` <- as.integer(price_volume_data$`Volume Weighted Price`)
price_volume_data$Volume <- as.integer(price_volume_data$Volume)

dat_pv <- price_volume_data |>
  dplyr::mutate(
  `Volume Weighted Price` = as.integer(`Volume Weighted Price`)
  , Volume = as.integer(Volume)
) %>%
  dplyr::mutate(
    month =
      lubridate::ymd( paste0(`Month of Price Date`,'-01') ) %>%
      lubridate::month()
    , year =
      lubridate::ymd( paste0(`Month of Price Date`,'-01') ) %>%
      lubridate::year()
    , .after = `Month of Price Date`
  ) %>%
  dplyr::group_by(`Part Number`) %>%
  tidyr::nest(data = -c(`Part Number`)) %>%
  dplyr::ungroup()

dat_pv <- price_volume_data |> dplyr::select(price = `Volume Weighted Price`, volume = Volume)

c_dat <- dat_pv |>
  dplyr::filter(`Part Number` == '776670-35WM') |>
  tidyr::unnest(data) |>
  dplyr::select(price = `Volume Weighted Price`, volume = Volume, year) |>
  dplyr::mutate(price = log( price/mean(price) ) )


dat_pv |>
  dplyr::filter(`Part Number` == '776670-35WM') |>
  tidyr::unnest(data) |>
  dplyr::select(price = `Volume Weighted Price`, volume = Volume, year) |>
  ggplot(aes(x=price, y = volume)) + geom_point()

glm(volume ~ 1 + price, family = poisson(), data = c_dat)
lm(volume ~ 1 + price, data = c_dat |> dplyr::mutate(volume = log(volume/mean(volume))))

piroc_01 <- brm(data =
    c_dat,
    family = poisson(),
    volume ~ 1 + price,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(cauchy(0, 10), ub = 0, class = b)
            ),
    iter = 4000, warmup = 1000, chains = 4, cores = 4,
    seed = 4,
    file = "scratch/fits/776670-35WM")

piroc_02 <- brm(data =
                  c_dat,
                family = poisson(),
                volume ~ 1 + price + (1 | year),
                prior = c(prior(normal(0, 5), class = Intercept),
                          prior(cauchy(0, 10), ub = 0, class = b)
                ),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 4,
                file = "scratch/fits/piroc_02")

piroc_03 <- brm(data =
                c_dat,
                family = negbinomial(),
                volume ~ 1 + price ,
                prior = c(prior(normal(0, 5), class = Intercept),
                          prior(cauchy(0, 10), ub = 0, class = b)
                ),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 4,
                file = "scratch/fits/piroc_03")

piroc_04 <- brm(data =
                  c_dat,
                family = hurdle_poisson(),
                volume ~ 1 + price ,
                prior = c(prior(normal(0, 5), class = Intercept),
                          prior(cauchy(0, 10), ub = 0, class = b)
                ),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 4,
                file = "scratch/fits/piroc_04")

piroc_05 <- brm(data =
                  c_dat ,
                family = brmsfamily("com_poisson"),
                volume ~ 1 + price ,
                prior = c(prior(normal(0, 5), class = Intercept),
                          prior(cauchy(0, 10), ub = 0, class = b)
                ),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 4,
                file = "scratch/fits/piroc_05")

hurdle_poisson

brms::fixef(piroc_01)
summary(piroc_01)
pp_check(piroc_01, ndraws = 400)
pp_check(piroc_01, type = "stat_2d")
plot(piroc_01, variable = c("b_Intercept", "b_price"))

plot(piroc_01)
plot(conditional_effects(piroc_01))

plot(piroc_02)
plot(conditional_effects(piroc_02))

brms::fixef(piroc_03)
conditional_effects(piroc_03)
plot(conditional_effects(piroc_03, spaghetti = TRUE, ndraws = 200, points=TRUE, mean = TRUE, rug = TRUE))
plot(conditional_effects(piroc_03, spaghetti = TRUE, ndraws = 300, mean = TRUE, rug = TRUE)) |> class()
plot(conditional_effects(piroc_03))
plot(conditional_effects(piroc_03, method = "posterior_predict"))
plot(piroc_03)

loo(piroc_01, piroc_03, piroc_04)


mod_196917_01 <-
  brm(data =
      dat_pv |>
      dplyr::filter(`Part Number` == '196917-01') |>
      tidyr::unnest(data)|> dplyr::select(price = `Volume Weighted Price`, volume = Volume) |>
        dplyr::mutate(price = log( price/mean(price) ) ),
    family = poisson(),
    volume ~ 1 + price,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(cauchy(0, 10), ub = 0, class = b)
    ),
    iter = 4000, warmup = 1000, chains = 4, cores = 4,
    seed = 4,
    file = "scratch/fits/196917-01")

brms::fixef(mod_196917_01)

res_brms <- dat_pv |>
  dplyr::slice_head(n=5) |>
  dplyr::mutate(
    results =
      purrr::map2(
        data
        , `Part Number`
        , function(x,y){
          dat <- x |>
            dplyr::select(price = `Volume Weighted Price`, volume = Volume, year) |>
            dplyr::mutate(price = log( price/mean(price) ) )
          brm(
            data = dat,
            family = poisson(),
            volume ~ 1 + price,
            prior = c(prior(normal(0, 5), class = Intercept),
                      prior(cauchy(0, 10), ub = 0, class = b)
            ),
            iter = 4000, warmup = 1000, chains = 4, cores = 4,
            seed = 4,
            file = paste0("scratch/fits/",y)
          ) |> brms::fixef()
        }
      )
  )
res_brms |> tidyr::unnest(results)

res_glm <- dat_pv |>
  dplyr::slice_head(n=5) |>
  dplyr::mutate(
    results =
      purrr::map(
        data
        , function(x){
          dat <- x |>
            dplyr::select(price = `Volume Weighted Price`, volume = Volume, year) |>
            dplyr::mutate(price = log( price/mean(price) ) )
          glm(volume ~ 1 + price, family = poisson(), data = dat) |>
            broom::tidy()
        }
      )
  )
res_glm |> tidyr::unnest(results)


cc_dat <- dat_pv |>
  #dplyr::slice_head(n=5) |>
  dplyr::slice(2:5) |>
  tidyr::unnest(data) |>
  dplyr::rename(site = `Part Number`, price = `Volume Weighted Price`, volume = Volume) |>
  group_by(site) |>
  dplyr::mutate(price = log( price/mean(price) )) |>
  dplyr::ungroup()

cc_dat |>
  # dplyr::group_by(site) |>
  dplyr::filter(site=="192061-01") |>
  ggplot(aes(x=price, y=volume, fill = site)) + geom_point()

piroc_005 <- brm(data =
                cc_dat,
                family = poisson(),
                volume ~ 1 + price*site + (1|site),
                prior = c(prior(normal(0, 5), class = Intercept),
                          prior(cauchy(0, 10), ub = 0, class = b)
                ),
                iter = 4000, warmup = 1000, chains = 4, cores = 4,
                seed = 4,
                control=list(adapt_delta=0.99, max_treedepth = 15),
                file = "scratch/fits/piroc_005-5")

pairs(piroc_005)
plot(piroc_005)
plot(conditional_effects(piroc_005, method = "posterior_predict"))
summary(piroc_005)
pp_check(piroc_005, ndraws = 400)

# @@@@@@@@@@@@@@@@
# Q1 ----

data(polite)

dat <- polite |> dplyr::select(subject, gender,task, context = attitude, pitch = f0mn)

dat |>
  tidyr::drop_na() |>
  group_by(gender) |>
  ggplot(aes(x=gender, y=pitch, color = context)) + geom_jitter(width=0.1)

tibble_means <- dat |>
  group_by(context, gender) |>
  tidyr::drop_na() |>
  summarize(mean = mean(pitch))
head(tibble_means)

mean(tibble_means$mean)


politeness_df <- dat %>%
  tidyr::drop_na() |>
  mutate(gender = factor(gender),   # you could rename or reorder the levels here
         context = factor(context))

stats::contrasts(politeness_df$gender)
stats::contrasts(politeness_df$context)

contrasts(politeness_df$gender) <- contr.treatment(n=2) # insert the number of levels here
colnames(contrasts(politeness_df$gender)) <- "M" # manually declare the contrast level names

fit_dummy_FE <- brm(
  pitch ~ gender * context,
  data = politeness_df,
  cores = 4,
  iter = 1000,
  file = "scratch/fits/fit_dummy_FE_01"
)

fit_dummy_FE.coefs <- fixef(fit_dummy_FE)[,1] %>% as.numeric() # get the estimated coefficients
summary(fit_dummy_FE)

y1 = fit_dummy_FE.coefs[1] + fit_dummy_FE.coefs[2]*0 +
  fit_dummy_FE.coefs[3]*0 + fit_dummy_FE.coefs[4]*(0)
y1

y2 = fit_dummy_FE.coefs[1] + fit_dummy_FE.coefs[2]*1 +
  fit_dummy_FE.coefs[3]*0 + fit_dummy_FE.coefs[4]*(1*0)
y2

cell_means_BayesStats <- fit_dummy_FE |>
  tidybayes::tidy_draws() |>
  select(starts_with("b_")) |>
  mutate(
    female_informal = b_Intercept,
    female_polite   = b_Intercept + b_contextpol,
    male_informal   = b_Intercept + b_genderM,
    male_polite     = b_Intercept + b_contextpol +
      b_genderM + `b_genderM:contextpol`
  ) |>
  select(5:8) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  reframe(aida::summarize_sample_vector(value)[-1])
cell_means_BayesStats


# sample for male speakers in polite contexts
faintr::filter_cell_draws(
  fit = fit_dummy_FE,
  group = gender == "M" & context == "pol"
) |> pull(draws) |>
  aida::summarize_sample_vector(name = "male_polite")


faintr::compare_groups(
  fit = fit_dummy_FE,
  higher  = gender == "F" & context == "inf",
  lower   = gender == "M" & context == "pol"
)


# manual creation of contrasts
contr.matrix <- matrix( rep(0.5, 2))
dummy.matrix <- contr.treatment(2)
contr.coding <- dummy.matrix - contr.matrix

# we should duplicate the values to not overwrite previous contrasts
politeness_df <- politeness_df %>%
  mutate(context_contr = context,
         gender_contr = gender)
contrasts(politeness_df$context_contr) <- contr.coding
contrasts(politeness_df$gender_contr)  <- contr.coding


lm.contr.FE <- brm(
  pitch ~ gender_contr * context_contr,
  data = politeness_df,
  cores = 4,
  iter =  1000,
  file = "scratch/fits/lm.contr.FE"
)

lm.contr.FE.coefs <- brms::fixef(lm.contr.FE)[,1] %>% as.numeric() # get vector of estimated coefficients
summary(lm.contr.FE)



# from https://learnb4ss.github.io/learnB4SS/articles/ex_08.html#adding-random-effects
# Q ----

data(polite)

# specify priors in log space
priors_simple_log <- c(
  # prior for the Intercept (= the reference level)
  brms::prior(normal(0, 2), class = Intercept),
  # prior for the fixed effect coefficient for polite attitude
  brms::prior(normal(0, .25), class = b, coef = attitudepol),
  # prior for the residual variance
  brms::prior(cauchy(0, 0.1), class = sigma)
)

# specify model
simple_model2_prior <- brms::brm(
  articulation_rate ~
    attitude,
  data = polite,
  prior = priors_simple_log,
  # specify that the model should only estimate the posterior based on the information in the prior
  sample_prior = "only",
  # specify lognormal family function
  family = lognormal,
  file = "slides/fits/simple_model2_prior"
)

# quick and dirty plot on the original scale
brms::conditional_effects(simple_model2_prior)

# specify model
simple_model2 <- brm(
  articulation_rate ~
    attitude,
  data = polite,
  prior = priors_simple_log,
  family = lognormal,
  file = "slides/fits/simple_model2"
)

# posterior predictive check
pp_check(simple_model2, nsamples = 100)

# Extract posterior coefficient and plot
simple_model2 %>%
  tidybayes::spread_draws(b_attitudepol) %>%
  # plot
  ggplot(aes(x = b_attitudepol)) +
  tidybayes::stat_halfeye() +
  geom_vline(xintercept = 0, lty = "dashed") +
  labs(x = "log(articulation rate)")

# Extract posterior and plot predicted values for both levels
polite %>%
  modelr::data_grid(attitude) %>%
  tidybayes::add_predicted_draws(simple_model2) %>%
  # plot
  ggplot(aes(x = .prediction, y = attitude, fill = attitude)) +
  tidybayes::stat_halfeye() +
  scale_fill_manual(values = c("#8970FF", "#FFA70B")) +
  labs(x = "articulation rate")


# get prior
get_prior(
  formula = articulation_rate ~ attitude + task,
  data = polite, family = lognormal
)

# specify priors
priors_multiple <- c(
  prior(normal(0, 2), class = Intercept),
  prior(normal(0, .25), class = b, coef = attitudepol),
  prior(normal(0, .25), class = b, coef = tasknot),
  prior(cauchy(0, .1), class = sigma)
)

# tun model
multiple_model <- brm(
  articulation_rate ~ attitude + task,
  data = polite,
  prior = priors_multiple,
  family = lognormal,
  file = "slides/fits/multiple_model")

summary(multiple_model)


# Extract posterior coefficient attitudepol
post_multiple_attitude <- multiple_model %>%
  tidybayes::spread_draws(b_attitudepol, b_tasknot)

ggplot(post_multiple_attitude) +
  tidybayes::stat_halfeye(aes(x = b_attitudepol)) +
  labs(title = "posterior for effect of attitude",
       x = "log(articulation rate)") +
  geom_vline(xintercept = 0, lty = "dashed")

# Extract posterior coefficient tasknot
ggplot(post_multiple_attitude) +
  tidybayes::stat_halfeye(aes(x = b_tasknot)) +
  labs(title = "posterior for effect of task",
       x = "log(articulation rate)") +
  geom_vline(xintercept = 0, lty = "dashed")


# model specification
complex_model_formula =
  articulation_rate ~ attitude + task +
  # add random intercept
  (1 | subject) +
  # add random slope
  (0 + attitude | subject)

# get prior
get_prior(
  articulation_rate ~ attitude + task +
    (1 | subject) +
    (0 + attitude | subject),
  data = polite,
  family = lognormal
)


# specify priors
priors_complex <- c(
  prior(normal(0, 2), class = Intercept),
  prior(normal(0, 0.25), class = b, coef = attitudepol),
  prior(normal(0, 0.25), class = b, coef = tasknot),
  # specify weakly informative prior for the random effects (slopes)
  prior(cauchy(0, 0.1), class = sd, coef = Intercept, group = subject),
  prior(cauchy(0, 0.1), class = sd, coef = attitudeinf, group = subject),
  prior(cauchy(0, 0.1), class = sd, coef = attitudepol, group = subject),
  # specify weakly informative prior for the correlation between random intercept and slope
  prior(lkj(2), class = cor, group = subject),
  prior(cauchy(0, 0.1), class = sigma)
)


# Run model
complex_model <- brm(
  articulation_rate ~ attitude + task +
    # add random intercept
    (1 | subject) +
    # add random slope
    (0 + attitude | subject),
  data = polite,
  prior = priors_complex,
  family = lognormal,
  # set seed
  seed = 999,
  file = "slides/fits/complex_model"
)

summary(complex_model)


# Extract posterior coefficient politeness
post_complex <- complex_model %>%
  tidybayes::spread_draws(b_attitudepol, b_tasknot)

# plot
ggplot(post_complex) +
  # Extract posterior coefficient politeness
post_complex <- complex_model %>%
  spread_draws(b_attitudepol, b_tasknot)

# plot
ggplot(post_complex) +
  tidybayes::stat_halfeye(aes(x = b_attitudepol))
  labs(x = "log(articulation rate)") +
  geom_vline(xintercept = 0, lty = "dashed")


  stat_halfeye(aes(x = b_attitudepol))
labs(x = "log(articulation rate)") +
  geom_vline(xintercept = 0, lty = "dashed")

# extract 95% HDI
tidybayes::hdi(post_complex$b_attitudepol)
cat("\n")
# extract probability of direction
bayestestR::p_direction(post_complex$b_attitudepol)

# @@@@@@@@@@@@@@@@@@@@
# Q4 ----


# Load required packages
library(tidyverse)
library(brms)
library(tidybayes)

# Set seed for reproducibility
set.seed(456)

# Generate synthetic data
n_regions <- 5             # Number of geographical regions
n_stores_per_region <- 12  # Stores per region
n_months <- 24            # Months of data
n_total <- n_regions * n_stores_per_region * n_months

# Generate region-level effects (some regions perform better/worse)
region_effects <- rnorm(n_regions, mean = 0, sd = 0.2)

# Generate store-level characteristics
store_data <- data.frame(
  store_id = 1:(n_regions * n_stores_per_region),
  region_id = rep(1:n_regions, each = n_stores_per_region),
  store_size = runif(n_regions * n_stores_per_region, 2000, 8000),  # Square feet
  competition_density = runif(n_regions * n_stores_per_region, 1, 10)  # Number of competitors within 5 miles
)

# Create the full dataset
data <- crossing(
  store_id = store_data$store_id,
  month = 1:n_months
) %>%
  left_join(store_data, by = "store_id") %>%
  mutate(
    # Seasonal effect (higher sales in months 11-12 for holiday season)
    seasonal_effect = ifelse(month %in% c(11, 12), 0.2, 0) +
      sin(2 * pi * month / 12) * 0.1,

    # Marketing spend varies by month and store
    marketing_spend = runif(n(), 5000, 15000),

    # Generate log-transformed monthly sales
    log_sales = 10 +
      region_effects[region_id] +                    # Region effect
      0.0001 * store_size +                         # Store size effect
      -0.05 * competition_density +                 # Competition effect
      0.0001 * marketing_spend +                    # Marketing effect
      seasonal_effect +                             # Seasonal effect
      rnorm(n(), 0, 0.1)                           # Random noise
  ) %>%
  mutate(
    sales = exp(log_sales)  # Convert back to raw sales
  )

data |> readr::write_csv("labs/data/regional_sales.csv")

# Fit hierarchical Bayesian model
sales_model <- brm(
  formula = log_sales ~ 1 +
    scale(store_size) +
    scale(competition_density) +
    scale(marketing_spend) +
    seasonal_effect +
    (1|region_id) +
    (1|store_id),
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(10, 1), class = "Intercept"),
    prior(normal(0, 0.5), class = "b"),
    prior(cauchy(0, 0.2), class = "sd"),
    prior(cauchy(0, 0.2), class = "sigma")
  ),
  chains = 4,
  cores = 4,
  seed = 456,
  file = "scratch/fits/sales_model"
)

pairs(sales_model)
print(sales_model)

# Generate predictions for a new store
new_store_data <- data.frame(
  store_size = 5000,
  competition_density = 5,
  marketing_spend = 10000,
  seasonal_effect = 0,
  region_id = 1,
  store_id = max(data$store_id) + 1
)

# Make predictions
predictions <- predict(sales_model, newdata = new_store_data, allow_new_levels=TRUE)

# Create diagnostic plots
# 1. Random effects by region
region_effects_plot <- plot(ranef(sales_model)$region_id)

# 2. Actual vs Predicted plot
fitted_values <- stats::fitted(sales_model)
plot_data <- data.frame(
  actual = data$log_sales,
  predicted = fitted_values[, "Estimate"],
  lower = fitted_values[, "Q2.5"],
  upper = fitted_values[, "Q97.5"]
)

prediction_plot <- ggplot(plot_data, aes(x = actual, y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "Actual Log Sales",
    y = "Predicted Log Sales",
    title = "Model Predictions vs Actual Sales"
  ) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
  theme_minimal()

# 3. Seasonal effects plot
seasonal_plot <- ggplot(data, aes(x = month, y = sales)) +
  stat_summary(fun = mean, geom = "line") +
  labs(
    x = "Month",
    y = "Average Sales",
    title = "Seasonal Sales Patterns"
  ) +
  theme_minimal()

# $$$$$$$$$$$$$$$$$

# Load required packages
library(tidyverse)
library(brms)
library(tidybayes)
library(lubridate)
library(scales)

# Set seed for reproducibility
set.seed(456)

# Generate synthetic data with expanded business metrics
n_regions <- 5
n_stores_per_region <- 12
n_months <- 24
n_total <- n_regions * n_stores_per_region * n_months

# Enhanced region-level characteristics
region_data <- data.frame(
  region_id = 1:n_regions,
  region_name = c("Northeast", "Southeast", "Midwest", "Southwest", "West"),
  region_min_wage = c(12.50, 10.00, 11.00, 9.50, 13.50),
  region_median_income = c(65000, 52000, 55000, 48000, 70000)
)

# Enhanced store-level characteristics
store_data <- data.frame(
  store_id = 1:(n_regions * n_stores_per_region),
  region_id = rep(1:n_regions, each = n_stores_per_region),
  store_size = runif(n_regions * n_stores_per_region, 2000, 8000),
  competition_density = runif(n_regions * n_stores_per_region, 1, 10),
  store_age = runif(n_regions * n_stores_per_region, 1, 15),
  is_premium_location = rbinom(n_regions * n_stores_per_region, 1, 0.3),
  parking_spots = round(runif(n_regions * n_stores_per_region, 20, 100))
) %>%
  left_join(region_data, by = "region_id")

# Generate more detailed time series data
data <- crossing(
  store_id = store_data$store_id,
  month = 1:n_months
) %>%
  left_join(store_data, by = "store_id") %>%
  mutate(
    date = as.Date("2022-01-01") + months(month - 1),

    # Enhanced seasonal effects
    seasonal_effect = ifelse(month %in% c(11, 12), 0.2, 0) +
      ifelse(month %in% c(1, 2), -0.1, 0) +
      sin(2 * pi * month / 12) * 0.1,

    # Marketing and operational metrics
    marketing_spend = runif(n(), 5000, 15000),
    staff_hours = runif(n(), 400, 800),
    customer_count = round(runif(n(), 1000, 3000)),

    # Generate various KPIs
    average_transaction = runif(n(), 30, 80),
    employee_satisfaction = runif(n(), 3, 5),
    customer_satisfaction = runif(n(), 3.5, 4.8),

    # Calculate base log sales
    log_sales = 10 +
      0.0001 * store_size +
      -0.05 * competition_density +
      0.0001 * marketing_spend +
      0.001 * region_median_income/1000 +
      0.1 * employee_satisfaction +
      0.2 * customer_satisfaction +
      0.05 * store_age +
      0.2 * is_premium_location +
      seasonal_effect +
      rnorm(n(), 0, 0.1)
  ) %>%
  mutate(
    # Calculate actual sales and derived metrics
    sales = exp(log_sales),
    labor_cost = staff_hours * region_min_wage,
    gross_margin = sales * 0.4,
    operating_costs = labor_cost + marketing_spend + (store_size * 2),  # $2 per sq ft operating cost
    profit = gross_margin - operating_costs,
    conversion_rate = customer_count / (customer_count * runif(n(), 1.2, 1.5)),
    sales_per_sqft = sales / store_size,
    sales_per_labor_hour = sales / staff_hours,
    profit_margin = profit / sales
  )

cols <- c('sales', 'store_size', 'competition_density', 'marketing_spend', 'employee_satisfaction', 'customer_satisfaction',
  'store_age', 'is_premium_location', 'region_median_income', 'seasonal_effect', 'region_id', 'store_id')
data |>
  # dplyr::select(dplyr::all_of(cols))
  recipes::recipe(sales ~ .) |>
  recipes::step_log(sales) |>
  recipes::step_select(all_of(cols)) |>
  recipes::step_num2factor(is_premium_location, region_id, store_id) |>
  recipes::step_normalize(all_numeric_predictors()) |>
  recipes::prep() |>
  recipes::bake(new_data=NULL) |> dplyr::glimpse()

# Fit enhanced hierarchical Bayesian model
enhanced_model <- brm(
  formula = log_sales ~ 1 +
    scale(store_size) +
    scale(competition_density) +
    scale(marketing_spend) +
    scale(employee_satisfaction) +
    scale(customer_satisfaction) +
    scale(store_age) +
    is_premium_location +
    scale(region_median_income) +
    seasonal_effect +
    (1|region_id) +
    (1|store_id),
  data = data,
  family = gaussian(),
  prior = c(
    prior(normal(10, 1), class = "Intercept"),
    prior(normal(0, 0.5), class = "b"),
    prior(cauchy(0, 0.2), class = "sd"),
    prior(cauchy(0, 0.2), class = "sigma")
  ),
  chains = 4,
  cores = 4,
  seed = 456,
  file = "scratch/fits/enhanced_model"
)

# Create business insight visualizations

# 1. Performance Dashboard
store_performance <- data %>%
  group_by(store_id, region_name) %>%
  summarise(
    avg_sales = mean(sales),
    avg_profit = mean(profit),
    avg_conversion = mean(conversion_rate),
    avg_satisfaction = mean(customer_satisfaction),
    sales_per_sqft = mean(sales_per_sqft),
    profit_margin = mean(profit_margin)
  )

# Store Performance Heatmap
performance_heatmap <- ggplot(store_performance,
                              aes(y = reorder(region_name, avg_sales), x = store_id)) +
  geom_tile(aes(fill = scale(avg_sales))) +
  scale_fill_viridis_c() +
  labs(title = "Store Sales Performance by Region",
       y = "Region", x = "Store ID", fill = "Standardized Sales") +
  theme_minimal()

# 2. KPI Trends
kpi_trends <- data %>%
  group_by(date) %>%
  summarise(
    avg_sales = mean(sales),
    avg_profit_margin = mean(profit_margin),
    avg_conversion = mean(conversion_rate),
    avg_satisfaction = mean(customer_satisfaction)
  ) %>%
  pivot_longer(cols = -date, names_to = "metric", values_to = "value")

kpi_plot <- ggplot(kpi_trends, aes(x = date, y = value, color = metric)) +
  geom_line() +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Key Performance Metrics Over Time",
       x = "Date", y = "Value") +
  theme_minimal()

# 3. ROI Analysis
roi_analysis <- data %>%
  group_by(store_id) %>%
  summarise(
    marketing_roi = sum((sales - lag(sales, default = first(sales))) / marketing_spend),
    total_investment = sum(marketing_spend + operating_costs),
    total_profit = sum(profit)
  )

roi_plot <- ggplot(roi_analysis,
                   aes(x = total_investment, y = total_profit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Investment vs. Profit Analysis",
       x = "Total Investment ($)",
       y = "Total Profit ($)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# 4. Factor Impact Analysis
posterior_samples <- as_tibble(enhanced_model)
factor_impact <- data.frame(
  Factor = rownames(fixef(enhanced_model)),
  Impact = fixef(enhanced_model)[,1],
  Lower = fixef(enhanced_model)[,3],
  Upper = fixef(enhanced_model)[,4]
)

impact_plot <- ggplot(factor_impact,
                      aes(x = reorder(Factor, Impact), y = Impact)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
  coord_flip() +
  labs(title = "Impact of Different Factors on Sales",
       x = "Factor", y = "Estimated Effect Size") +
  theme_minimal()

# 5. Store Efficiency Matrix
efficiency_matrix <- ggplot(store_performance,
                            aes(x = sales_per_sqft, y = profit_margin)) +
  geom_point(aes(color = region_name, size = avg_sales)) +
  labs(title = "Store Efficiency Matrix",
       x = "Sales per Square Foot",
       y = "Profit Margin",
       color = "Region",
       size = "Average Sales") +
  theme_minimal()
