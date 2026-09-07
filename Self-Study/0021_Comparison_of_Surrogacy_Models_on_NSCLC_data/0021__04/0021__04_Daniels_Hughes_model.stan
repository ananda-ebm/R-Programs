data {
  int<lower=1> N;

  vector[N] theta_hat;
  vector[N] gamma_hat;

  vector<lower=0>[N] se_theta;
  vector<lower=0>[N] se_gamma;
}

parameters {
  vector[N] gamma;
  vector[N] z;

  real alpha;
  real beta;

  real<upper=log(2)> log_tau;
  
  real<lower=0, upper=0.999> rho;
}

transformed parameters {
  
  real<lower=0> tau = exp(log_tau);
  
  vector[N] theta;
  theta = alpha + beta * gamma + tau * z;
}

model {

  // Priors
  alpha ~ normal(0, sqrt(1000));
  beta  ~ normal(0, sqrt(1000));

  gamma ~ normal(0, sqrt(1000));
  z ~ std_normal();

  rho ~ uniform(0, 0.999);
  
  target += log_tau - log(2);

  // Within-study model
  for (i in 1:N) {

    vector[2] y_obs;
    vector[2] mu;
    matrix[2,2] L;

    y_obs[1] = theta_hat[i];
    y_obs[2] = gamma_hat[i];

    mu[1] = theta[i];
    mu[2] = gamma[i];

    L[1,1] = se_theta[i];
    L[1,2] = 0;

    L[2,1] = rho * se_gamma[i];
    L[2,2] = se_gamma[i] * sqrt(1 - square(rho));

    y_obs ~ multi_normal_cholesky(mu, L);
  }
}
