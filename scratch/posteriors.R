# created July 13, 2024

# from https://stats.stackexchange.com/questions/319494/showing-bayesian-updating-in-r

require(ggplot2)

# True distribution
p <- 0.5
n <- 10

# Prior on p (n assumed known), discretized

# range of probability [0.01 - 0.99]
p_values <- seq(0.01,0.99,0.001)
# probability is beta(1,1)
pr <- dbeta(p_values,3,2)
# Have to normalize given discreteness
pr <- 1000 * pr / sum(pr)

tibble::tibble(x = p_values, prob = pr) |>
  ggplot(aes(x = x, y=prob)) +
  geom_line()

  geom_histogram(alpha = 0.30, position = 'identity', color="#e9ecef", bins = 30)

plot(pr~p_values, col=1, ylim=c(0, 14), type="l")

# Run for 20 samples
for (i in 1:20) {
  x <- rbinom(1, n, p)
  ps <- dbinom(x, n, p_values) * pr
  ps <- 1000 * ps / sum(ps)
  lines(ps~p_values, col=(i+1))

  pr = ps
}

plot(pr~p_values, col=1, ylim=c(0, 14), type="l")

# USE THIS CODE ======================================
# showing the posterior approaching the correct value.


p <- 0.5
n <- 10
# range of probability [0.01 - 0.99]
p_values <- seq(0.01,0.99,0.001)
# prior probability is beta(1,1)
pr <- dbeta(p_values,7,2)
# Have to normalize given discreteness
pr <- pr / sum(pr) # 1000 * pr / sum(pr)
# create the data
dat <- tibble::tibble(x = p_values, prob = pr) |>
  dplyr::mutate(step= 0)

# dat |>
#   ggplot(aes(x = x, y=prob)) +
#   geom_line() +
#   theme_minimal()

# Run for M samples
for (i in 1:8) {
  x <- rbinom(1, n, p)
  ps <- dbinom(x, n, p_values) * pr
  ps <- ps / sum(ps) # 1000 * ps / sum(ps)
  # lines(ps~p_values, col=(i+1))
  dat <- dat |>
    dplyr::bind_rows(
      tibble::tibble(x = p_values, prob = ps) |>
        dplyr::mutate(step = i)
    )
  #
  pr = ps
}


dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = x, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  theme_minimal()

dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = x, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  facet_grid(cols = vars(step)) +
  theme_minimal()

dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = x, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  facet_wrap(vars(step), nrow = 3) +
  theme_minimal()


