# created October 22, 2024

# check if 'librarian' is installed and if not, install it
if (! "librarian" %in% rownames(installed.packages()) ){
  install.packages("librarian")
}

# load packages if not already loaded
librarian::shelf(
  tidyverse, broom, rsample, ggdag, causaldata, magrittr, gt, gtExtras, halfmoon, ggokabeito, ggplot2, r-causal/propensity, r-causal/causalworkshop, survey, cardx, gtsummary,
  brms, rmcelreath/rethinking, shinystan, patchwork, tidybayes
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
    , .after = `Month of Price Date`
  ) %>%
  dplyr::group_by(`Part Number`) %>%
  tidyr::nest(data = -c(`Part Number`)) %>%
  dplyr::ungroup()

dat_pv <- price_volume_data |> dplyr::select(price = `Volume Weighted Price`, volume = Volume)

piroc_01 <- brm(data =
      dat_pv |>
      dplyr::filter(`Part Number` == '776670-35WM') |>
      tidyr::unnest(data)|> dplyr::select(price = `Volume Weighted Price`, volume = Volume) |>
        dplyr::mutate(price = log( price/mean(price) ) ),
    family = poisson(),
    volume ~ 1 + price,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(cauchy(0, 10), ub = 0, class = b)
            ),
    iter = 4000, warmup = 1000, chains = 4, cores = 4,
    seed = 4,
    file = "scratch/fits/776670-35WM")

brms::fixef(piroc_01)
summary(piroc_01)
pp_check(piroc_01, ndraws = 400)
pp_check(piroc_01, type = "stat_2d")




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
