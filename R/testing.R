# library(mystan)
# library(splines2)
# library(flexsurv)
# library(ggplot2)
#
# sme <- stan_model("src/stan_files/survreg_ph_exponential.stan")
# smw <- stan_model("src/stan_files/survreg_ph_weibull.stan")
# smm <- stan_model("src/stan_files/survreg_ph_mspline.stan")
#
#
# df <- flexsurv::bc
# N <- nrow(df)
# y <- log(as.vector(df$rectime))
# v <- df$censrec
# X <- as.matrix(cbind(1, as.integer(df$group == "Medium"), as.integer(df$group=="Poor")))[, -1]
# K <- ncol(X)
#
# n_knots <- 2 # needs to be > 1
# knots <- quantile(y, head(tail(seq(0, 1, length.out = n_knots + 2), -1), -1))
# nknots <- length(knots)
# order <- 3
# W <- iSpline(y, knots = knots, degree = order - 1, intercept = TRUE)
# Z <- deriv(isOut)
# W <- iSpline(y, knots = knots, Boundary.knots = range(y), degree = 2, intercept = TRUE)
# Z <- mSpline(y, knots = knots, Boundary.knots = range(y), degree = 2, intercept = TRUE)
# M <- ncol(Z)
#
# ggplot(mapping=aes(x=x, y=y))+
#   geom_line(data=data.frame(x=y, y=W[,1]))+
#   geom_line(data=data.frame(x=y, y=W[,2]))+
#   geom_line(data=data.frame(x=y, y=W[,3]))+
#   geom_line(data=data.frame(x=y, y=W[,4]))
#
# ggplot(mapping=aes(x=x, y=y))+
#   geom_line(data=data.frame(x=y, y=Z[,1]))+
#   geom_line(data=data.frame(x=y, y=Z[,2]))+
#   geom_line(data=data.frame(x=y, y=Z[,3]))+
#   geom_line(data=data.frame(x=y, y=Z[,4]))
#
# stan_dat <- list(
#   N = N,
#   K = K,
#   M = M,
#   y = y,
#   v = v,
#   X = X,
#   Z = Z,
#   W = W,
#   mu0 = rep(0, K),
#   Sigma0 = diag(c(10, rep(1, K - 1))),
#   alpha = rep(1, M),
#   estimate = 1,
#   rcens = y,
#   sigma0 = 10,
#   gamma0 = 2
# )
#
# fit1 <- sampling(sme, data = stan_dat, chains = 1, cores=2, iter = 2000, control = list(adapt_delta = .95))
# fit2 <- sampling(smw, data = stan_dat, chains = 1, cores=2, iter = 2000, control = list(adapt_delta = .95))
# fit3 <- sampling(smm, data = stan_dat, chains = 1, cores=2, iter = 2000, control = list(adapt_delta = .95))
#
# post3 <- as.array(fit3)
# gammas <- summary(fit3)$summary[sprintf("gamma[%d]", 1:M), "50%"]
# betas  <-  summary(fit3)$summary[sprintf("beta[%d]", 1:2), "50%"]
# ss <- as.vector(-Z %*% gammas * exp(X %*% betas))
# Ss <- exp(ss)
# # survival curves
# ggplot(data=data.frame(x=exp(y)/365, y=Ss), aes(x=x, y=y))+
#   geom_point()
