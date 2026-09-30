data {
  int<lower=1> N;
  
  vector[N] y1;
  vector[N] y2;
  
  vector<lower=0>[N] se1;
  vector<lower=0>[N] se2;
}

parameters {
  real eta1;

  real lambda0;

  real<lower=0, upper=2> tau1;
  real<lower=0, upper=2> tau2;

  real<lower=-0.999, upper=0.999> rho_b;
  
  real<lower=0, upper=0.999> rho_w;
}

transformed parameters {
  
  real<lower=0> psi1 = tau1;
  real<lower=0> psi1_sq = square(tau1);
  
  real lambda1 = (tau2 / tau1) * (rho_b);
  
  real<lower=0> psi2_sq = square(tau2) - square(lambda1) * square(tau1);
  real<lower=0> psi2 = sqrt(psi2_sq);
}

model {

  // combined model
  
  for (i in 1:N){
    
    // observation
    vector[2] y_obs = [y1[i], y2[i]]';
    
    // mean
    vector[2] mu = [eta1, lambda0 + lambda1 * eta1]';
    
    // covariance matrix
    matrix[2,2] Sigma;
    
    Sigma[1,1] = square(se1[i]) + psi1_sq;
    Sigma[1,2] = se1[i] * se2[i] * rho_w + lambda1 * psi1_sq;
    
    Sigma[2,1] = Sigma[1,2];
    Sigma[2,2] = square(se2[i]) + psi2_sq + square(lambda1) * psi1_sq;
    
    // distribution
    y_obs ~ multi_normal(mu, Sigma);
  }
  
  // priors
  
  eta1 ~ normal(0, sqrt(1000));

  lambda0 ~ normal(0, sqrt(1000));

  tau1 ~ uniform(0, 2);
  tau2 ~ uniform(0, 2);
  
  rho_b ~ uniform(-0.999, 0.999);
  rho_w ~ uniform(0, 0.999);
}
