data {
  int<lower=0> N;      // sample size
  int<lower=1> K;      // parameters
  int<lower=0> y[N];   // responses
  int<lower=1> n[N];   // trials (all 1 if bernoulli)
  matrix[N, K] X;      // design matrix
  vector[K] mu0;       // prior mean
  matrix[K, K] Sigma0; // prior covariance (avoid cov_matrix checks
  int<lower=0,upper=2> link;
}
parameters {
  vector[K] beta;
}
model {
  vector[N] p;
  if (link == 0)
    p = inv_logit(X * beta);
  else if (link == 1)
    p = Phi(X * beta);
  else
    for (i in 1:N) p[i] = student_t_cdf(dot_product(X[i, ], beta), 4, 0, 1);
  beta ~ multi_normal(mu0, Sigma0);
  y ~ binomial(n, p);
}
