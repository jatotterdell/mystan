functions {
  real weibull_lb_rng(real alpha, real sigma, real lb) {
    real p = weibull_cdf(lb, alpha, sigma);   // cdf for lb
    real u = uniform_rng(p, 1);               // unif in bounds
    real y = sigma * (-log1m(u))^inv(alpha);  // inverse cdf
    return y;
  }

  real weibull_ub_rng(real alpha, real sigma, real ub) {
    real p = weibull_cdf(ub, alpha, sigma);   // cdf for ub
    real u = uniform_rng(0, p);               // unif in bounds
    real y = sigma * (-log1m(u))^inv(alpha);  // inverse cdf
    return y;
  }

  real weibull_lub_rng(real alpha, real sigma, real lb, real ub) {
    real p_lb = weibull_cdf(lb, alpha, sigma); // cdf for lb
    real p_ub = weibull_cdf(ub, alpha, sigma); // cdf for ub
    real u = uniform_rng(p_lb, p_ub);
    real y = sigma * (-log1m(u))^inv(alpha);  // inverse cdf
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
  real<lower=0> gamma0;
}

parameters {
  real<lower=0> gamma;
  vector[K] beta;
}

transformed parameters {
  real alpha0;
  real lambda0;

  alpha0 = exp(-beta[1]/gamma);
  lambda0 = exp(beta[1]);
}

model {
  vector[N] eta;
  eta = exp(-X*beta/gamma);
  gamma ~ exponential(gamma0);
  beta ~ normal(0, sigma0);

  for(n in 1:N) {
    if(v[n] == 0) // right-censored
      target += weibull_lccdf(y[n] | gamma, eta[n]);
    else if(v[n] == 1) // observed event time
      target += weibull_lpdf(y[n] | gamma, eta[n]);
    else if(v[n] == 2) // left-censored
      target += weibull_lcdf(y[n] | gamma, eta[n]);
    else if(v[n] == 3) // interval-censored
      target += log_diff_exp(weibull_lcdf(rcens[n] | gamma, eta[n]),
                             weibull_lcdf(y[n] | gamma, eta[n]));
  }
}

// generated quantities {
//   real y_tilde[N];
//
//   for(n in 1:N) {
//     if(v[n] == 0)
//       y_tilde[n] = weibull_lb_rng(gamma, eta[n], y[n]);
//     else if(v[n] == 1)
//       y_tilde[n] = weibull_rng(gamma, eta[n]);
//     else if(v[n] == 2)
//       y_tilde[n] = weibull_ub_rng(gamma, eta[n], y[n]);
//     else if(v[n] == 3)
//       y_tilde[n] = weibull_lub_rng(gamma, eta[n], y[n], rcens[n]);
//   }
// }
