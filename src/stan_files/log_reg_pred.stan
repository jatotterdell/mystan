data {
  int<lower=1> N; // sample size
  int<lower=1> K; // parameters
  int y[N];    // responses
  int n[N];    // trials (all 1 if bernoulli)
  matrix[N, K] X; // design matrix

  int<lower=1> Npred;
  int npred[Npred];
  matrix[Npred, K] Xpred;
}
parameters {
  vector[K] beta;
}
model {
  vector[N] eta = X * beta;
  beta[1] ~ normal(0, 1.75);
  for(i in 1:N) {
    y[i] ~ binomial(n[i], inv_logit(eta[i]));
  }
}
generated quantities {
  int ypred[Npred];
  for(i in 1:Npred) {
    ypred[i] = binomial_rng(npred[i], inv_logit(Xpred[i, ] * beta));
  }
}
