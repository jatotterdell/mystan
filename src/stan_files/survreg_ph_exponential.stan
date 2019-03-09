functions {
  real exponential_lb_rng(real lambda, real lb) {
    real p = exponential_cdf(lb, lambda); // cdf for lb
    real u = uniform_rng(p, 1);           // unif in bounds
    real y = -log1m(u)/lambda;            // inverse cdf
    return y;
  }

  real exponential_ub_rng(real lambda, real ub) {
    real p = exponential_cdf(ub, lambda); // cdf for ub
    real u = uniform_rng(0, p);           // unif in bounds
    real y = -log1m(u)/lambda;            // inverse cdf
    return y;
  }

  real exponential_lub_rng(real lambda, real lb, real ub) {
    real p_lb = exponential_cdf(lb, lambda); // cdf for lb
    real p_ub = exponential_cdf(ub, lambda); // cdf for ub
    real u = uniform_rng(p_lb, p_ub);        // unif in bounds
    real y = -log1m(u)/lambda;               // inverse cdf
    return y;
  }
}

data {
  int<lower=0> N;
  int<lower=0> K;
  real y[N];
  int v[N];
  real rcens[N];
  matrix[N, K] X;
  real<lower=0> sigma0;
}

parameters {
  vector[K] beta;
}

model {
  vector[N] eta;
  eta = exp(X*beta);
  beta ~ normal(0, sigma0);
  for(n in 1:N) {
    if(v[n] == 0) // right-censored
      target += exponential_lccdf(y[n] | eta[n]);
    else if(v[n] == 1) // observed event time
      target += exponential_lpdf(y[n] | eta[n]);
    else if(v[n] == 2) // left-censored
      target += exponential_lcdf(y[n] | eta[n]);
    else if(v[n] == 3) // interval-censored
      target += log_diff_exp(exponential_lcdf(rcens[n] | eta[n]),
                             exponential_lcdf(y[n] | eta[n]));
  }
}

generated quantities {
  real y_tilde[N];

  for(n in 1:N) {
    if(v[n] == 0)
      y_tilde[n] = exponential_lb_rng(exp(dot_product(X[n,], beta)), y[n]);
    else if(v[n] == 1)
      y_tilde[n] = exponential_rng(exp(dot_product(X[n,], beta)));
    else if(v[n] == 2)
      y_tilde[n] = exponential_ub_rng(exp(dot_product(X[n,], beta)), y[n]);
    else if(v[n] == 3)
      y_tilde[n] = exponential_lub_rng(exp(dot_product(X[n,], beta)), y[n], rcens[n]);
  }
}
