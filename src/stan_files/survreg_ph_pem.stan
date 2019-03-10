functions {
  // find index of smallest y greater than x
  int num_gt(real x, real[] y) {
    int n = 0;
    for (i in 1:size(y))
      if (x > y[i])
        n = n + 1;
    return n;
  }

  // log survival function of PEM model (exp(-M(t)))
  real pem_lccdf(real y, real[] lambda, real[] s, int interval) {
    real out;
    if(y < s[interval])
      reject("y should be no greater than s[interval]");

    out = -lambda[interval]*(y - s[interval]);
    for(k in 1:(interval - 1))
      out = out - lambda[k]*(s[k + 1] - s[k]);
    return out;
  }
}

data {
  int<lower=1> N; // individuals
  int<lower=0> K; // number of parameters
  int<lower=1> J; // size of partition
  real y[N];
  real v[N];
  real s[J + 1];
  matrix[N, K] X;
}

transformed data{
  real<lower=0> a0; // shape hyperpar for gamma
  real<lower=0> b0; // scale hyperpar for gamma
  int interval[N];  // interval in which an individuals event occurred
  real slen[J];     // length of interval in s

  a0 = 1.5;
  b0 = 1.5;
  for(n in 1:N)
    interval[n] = num_gt(y[n], s);

  for(j in 1:J)
    slen[j] = s[j + 1] - s[j];
}

parameters {
  real<lower=0> lambda[J];
  vector[K] beta;
}

transformed parameters {
  vector[N] eta;
  vector[N] log_haz;
  vector[N] haz;
  real log_lambda[J];
  eta = X*beta;
  log_lambda = log(lambda);
  for(n in 1:N) {
    log_haz[n] = log_lambda[interval[n]] + eta[n];
    haz[n] = exp(log_haz[n]);
  }
}

model {
  real temp_lambda[J]; // store individuals hazards for an iteration
  lambda ~ gamma(a0, b0);
  beta ~ normal(0, 10);
  for(n in 1:N) {
    if(v[n] == 1)
      target += log_haz[n];
    for(j in 1:J) {
      temp_lambda[j] = lambda[j]*exp(eta[n]);
    }
    target += pem_lccdf(y[n] | temp_lambda, s, interval[n]);
  }
}
