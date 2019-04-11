// survreg_ph_mspline.stan

data {
  int<lower=1> N; // number of observations
  int<lower=1> K; // number of covariates plus 1 for intercept
  int<lower=1> M; // number of mSpline knots
  real y[N];      // event times
  int v[N];       // censoring indicator
  matrix[N, K] X; // design matrix
  matrix[N, M] Z; // mSpline matrix
  matrix[N, M] W; // iSpline matrix
  vector[K] mu0; // Normal mean hyperparameter
  matrix[K, K] Sigma0; // Normal covariance hyperparameter
  vector[M] alpha; // Dirichlet hyperparameter
  int<lower=0, upper=1> estimate; // estimate the model or just simulate data
}

parameters {
    simplex[M] gamma;
    vector[K] beta;
}

model {
  vector[N] mu;
  vector[N] z;
  vector[N] w;

  mu = X*beta;
  z = log(Z*gamma);
  w = W*gamma;

  // prior
  target += multi_normal_lpdf(beta | mu0, Sigma0);

  // likelihood
  if(estimate) {
    for(n in 1:N) {
      if (v[n] == 1)
        target += mu[n] + z[n];
      target += -exp(mu[n]) * w[n];
    }
  }
}
