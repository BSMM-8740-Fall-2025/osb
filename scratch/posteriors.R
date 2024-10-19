# created July 13, 2024

# from https://stats.stackexchange.com/questions/319494/showing-bayesian-updating-in-r


# I'm constructing an example similar to what you were doing, but not exactly the same.
# Here I assume the data follows a Binomial(𝑛=10, 𝑝=0.5) distribution,that 𝑛=10 is known,
# and that the initial prior on 𝑝 is uniform. At each of 20 iterations I generate one new data point
# from the Binomial(10, 0.5) distribution and update the posterior with it.

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

set.seed(8740)
p <- 0.5
n <- 10
# range of probability [0.01 - 0.99]
p_values <- seq(0.01,0.99,0.001)
# prior probability is beta(1,1)
pr <- dbeta(p_values,7,2)
# Have to normalize given discreteness
pr <- pr / sum(pr) # 1000 * pr / sum(pr)
# create the data
dat <- tibble::tibble(parameter = p_values, prob = pr, x = 0) |>
  dplyr::mutate(step= 0)

dat |>
  ggplot(aes(x = parameter, y=prob)) +
  geom_line() +
  theme_minimal()

# Run for M samples
for (i in 1:8) {
  # have the data generating process generate a data point
  x <- rbinom(1, n, p)
  # multiply the likelihood of observing the data point at each p-value
  # by the prior probability of each p-value:
  # this gives the posterior probability for each p-value
  ps <- dbinom(x, n, p_values) * pr
  # normalize
  ps <- ps / sum(ps) # 1000 * ps / sum(ps)
  # lines(ps~p_values, col=(i+1))
  # same the posterior p-value probabilities at this step
  dat <- dat |>
    dplyr::bind_rows(
      tibble::tibble(parameter = p_values, prob = ps, x = x) |>
        dplyr::mutate(step = i)
    )
  # update the prior probability of each p-value to be its posterior probability
  pr = ps
}

dat <- dat |> dplyr::group_by(step) |>
  dplyr::mutate(
    title =
      dplyr::case_when(
        step == 0 ~ stringr::str_glue("Step {step}: prior mean is {(pv*prob) |> sum() |> round(digits=3)}")
        , TRUE ~ stringr::str_glue("Step {step}: sample is {x} & posterior mean is {(pv*prob) |> sum() |> round(digits=3)}")
      )

  )

labels <- dat |> dplyr::distinct(step, title) |> dplyr::mutate(step = factor(step))
step_labels <- split(labels$title, labels$step)

# dat |> dplyr::distinct(step, pmean) |> dplyr::mutate(step = factor(step))
#
# require(magrittr)
# dat |> dplyr::distinct(step, pmean) |> dplyr::mutate(step = factor(step)) %>% split(., .$step)
#
# wsx <- dat |> dplyr::distinct(step, pmean) |> dplyr::mutate(step = factor(step))
# split(wsx$pmean, wsx$step)
#
# dat |> dplyr::distinct(step, pmean) |> dplyr::mutate(step = factor(step)) %>% split(.$pmean, .$step)
#
# dat |> dplyr::filter(step == 1)

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



step_labeller <- function(variable,value){
  return(step_labels[value])
}

dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = pv, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  # geom_text(
  #   # data = dat |> dplyr::distinct(pmean),
  #   aes(x = -Inf, y = Inf,label = paste("p=",pmean)),
  #   hjust = -0.1, vjust = 1.1, lineheight = 1
  # ) +
  #annotate("text", x = 0.75, y = 0.006, label = stringr::str_glue("{dat$pmean[1]}")) +
  facet_wrap(vars(step), nrow = 3, labeller=step_labeller) +
  theme_minimal()


step_labeller <- function(value){
  return(step_labels[value])
}

dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = parameter, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  facet_wrap(vars(step), nrow = 3, labeller=labeller(step = step_labeller)) +
  theme(plot.margin=unit(c(-1,-1,-5,-1), 'cm')) +
  theme_minimal() + labs(title = "Bayesian updating", subtitle = "Binomial data; n known, true p = 0.5")

dat |> dplyr::mutate(step = factor(step)) |>
  dplyr::group_by(step) |>
  ggplot(aes(x = parameters, y=prob, color = step)) +
  geom_line() +
  geom_vline(xintercept=0.5, color="grey", size=1, linetype = "dashed") +
  facet_wrap(vars(step), nrow = 2, labeller=labeller(step = step_labeller)) +
  theme(plot.margin=unit(c(5,1,5,1), 'cm')) +
  theme_minimal() +
  labs(title = "Bayesian updating", subtitle = "Binomial data; n known, true p = 0.5")

# USE THIS CODE ======================================
# showing the posterior approaching the correct value.

set.seed(8740)
b0 = 0; b1 = 5; sigma = 10; sampleSize = 31

data <-
  tibble::tibble(
    x = (-(sampleSize-1)/2):((sampleSize-1)/2)
    , y =  b0 + b1 * x + rnorm(n=sampleSize, mean=0, sd=sigma)
  )


likelihood = function(param, x, y){
  pred = param[2] + param[1] * x
  singlelikelihoods = dnorm(y, mean = pred, sd = param[3], log = T)
  sumll = sum(singlelikelihoods)
  # return the log of the product of probabilities (data)
  return(sumll)
}

prior = function(param){
  aprior = dunif(param[1], min=0, max=10, log = T)
  bprior = dnorm(param[2], sd = 5, log = T)
  sdprior = dunif(param[3], min=0, max=30, log = T)
  # return the log of the product of probabilities (parameters)
  return(aprior+bprior+sdprior)
}

proposalfunction = function(param){
  return( rnorm(3, mean = param, sd= c(0.1,0.5,0.3)) )
}

run_metropolis_MCMC = function(startvalue, iterations, data){
  # initialize
  chain = array(dim = c(iterations+1,3))
  chain[1,] <- startvalue
  # x = data$x, y = data$y

  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])
    probab =
      exp(
        likelihood(proposal, data$x, data$y) +
          prior(proposal) -
          likelihood(chain[i,], data$x, data$y) -
          prior(chain[i,])
      )
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }
  # return(mcmc(chain))
  return(chain)
}

startvalue = c(4,2,8)
chain = run_metropolis_MCMC(startvalue, 20000, data)

mcm_chain <- coda::mcmc(chain)
summary(mcm_chain)
plot(mcm_chain)

mcm_chain <- coda::mcmc(chain)
smry <- summary(mcm_chain)
smry$statistics |> tibble::as_tibble() |>
  dplyr::bind_cols(smry$quantiles |> tibble::as_tibble()) |>
  tibble::add_column(param = c('beta1','beta0','sigma')) |>
  gt::gt("param") |>
  gt::tab_spanner(label = "percentiles", columns = ends_with("%")) |>
  #gt::opt_table_font( size = "60px" ) |>
  gt::tab_options(table.font.size = "60px") |>
  gtExtras::gt_theme_espn()

|>
  gt::as_raw_html()

