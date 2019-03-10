data {
  int N;
  int K;
  real y[N];
  matrix[N, K] X;
}

parameters {
  real mu;
  vector[K] beta;
  real<lower=0> sigma2; // variance
  real<lower=0> lambda; // penalty parameter
}

transformed parameters {
  real<lower=0> sigma;
  sigma = sqrt(sigma2);
}

model {
  vector[N] lp;
  lp = mu + X*beta;
  mu ~ normal(0, 1);
  sigma2 ~ cauchy(0, 3);
  beta ~ double_exponential(0, sigma / lambda);
  lambda ~ cauchy(0, 1);

  target += -2*log(sigma);
  y ~ normal(lp, sigma);
}

generated quantities {
  real y_tilde[N];
  for(i in 1:N) {
    y_tilde[i] = normal_rng(mu + dot_product(X[i, ], beta), sigma);
  }
}
