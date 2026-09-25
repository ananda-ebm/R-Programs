data {
  int<lower=1> N;

  vector[N] theta_hat;
  vector[N] gamma_hat;

  vector<lower=0>[N] se_theta;
  vector<lower=0>[N] se_gamma;
}

parameters {
  vector[N] gamma;
  
  real alpha;
  real beta;
  
  real<lower=0, upper=0.999> rho_w;

  real<upper=log(2)> log_tau;
}

transformed parameters {
  
  real<lower=0> tau = exp(log_tau);
  real<lower=0> tau_sq = square(tau);
}

model {

  // Priors
  alpha ~ normal(0, sqrt(1000));
  beta  ~ normal(0, sqrt(1000));

  gamma ~ normal(0, sqrt(1000));
  
  rho_w ~ uniform(0, 0.999);
  
  target += log_tau - log(2);

  // combined model
  for (i in 1:N) {

    vector[2] y_obs = [theta_hat[i], gamma_hat[i]]';
    
    vector[2] mu = [alpha + beta * gamma[i], gamma[i]]';
    
    matrix[2,2] Sigma;

    Sigma[1,1] = square(se_theta[i]) + square(tau);
    Sigma[1,2] = rho_w * se_theta[i] * se_gamma[i];

    Sigma[2,1] = Sigma[1,2];
    Sigma[2,2] = square(se_gamma[i]);

    y_obs ~ multi_normal(mu, Sigma);
  }
}
