data {
  // cmdstan_example_NI_02.stan February 04, 2023 
  /* Dimensions */
  int<lower=1> N; // rows

  /* price vector (integer) */
  array[N] real P;
  
  /* demand vector (integer) */
  array[N] int<lower=0> Y;

  /* hyperparameters*/
  real<lower=0> s; // scale for intercept prior
  real<lower=0> e_scale; // scale for elasticity prior
}

parameters {
  real <upper=0> elasticity;      // vector of elasticities - to compute \mu
  real intercept;                 // vector of group intercepts - to compute \mu

}

transformed parameters {
  array[N] real lambda;
  
  for (i in 1:N){
    lambda[i] = intercept + elasticity * P[i];
  }
  
}

model {
  /* Priors */
  target += normal_lpdf(intercept  | 0, s);
  target += cauchy_lpdf(elasticity | 0, e_scale);

  /* Likelihood */
  for (i in 1 : N) {
    target += poisson_lpmf(Y[i] | exp(lambda[i]) );
  }
}
generated quantities {
  array[N] int<lower=-1> y_new;
  vector[N] log_lik;
  for (i in 1 : N) {
      y_new[i]   = poisson_rng( exp(lambda[i]) );
      log_lik[i] = poisson_lpmf(Y[i] | exp(lambda[i]) );
  }
}

