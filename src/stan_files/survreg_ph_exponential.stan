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
