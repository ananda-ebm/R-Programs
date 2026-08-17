data {
  int<lower=0> N;
  vector[N] X;
  vector[N] Y;
}

parameters {
  real a;
  real b;
  real<lower=0> sigma;
}

model {
  Y[1:N] ~ normal(a + b * X[1:N], sigma);
}
