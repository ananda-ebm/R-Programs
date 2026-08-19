data {
  int<lower = 1> N;

  vector[N] Y2;
  vector[N] Y1;

  vector<lower = 0>[N] sigma2;
}

parameters {
  real beta;
  real lambda1;
  real<lower = 0> psi;

  vector[N] lambda0_raw;
}

transformed parameters {
  vector[N] lambda0;
  vector[N] mu2;

  lambda0 = beta + psi * lambda0_raw;
  mu2 = lambda0 + lambda1 * Y1;
}

model {
  // Priors
  beta ~ normal(0, 10);
  lambda1 ~ normal(0, 10);
  psi ~ student_t(3, 0, 2.5);

  lambda0_raw ~ std_normal();

  // Sampling distribution
  Y2 ~ normal(mu2, sigma2);
}

generated quantities {
  vector[N] Y2_rep;

  for (i in 1:N) {
    Y2_rep[i] = normal_rng(mu2[i], sigma2[i]);
  }
}
