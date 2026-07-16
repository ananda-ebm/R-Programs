data {
  int<lower=1> N;

  vector[N] theta_hat;   // observed true endpoint effects
  vector[N] gamma_hat;   // observed surrogate endpoint effects

  array[N] matrix[2,2] Sigma;
  
  real<lower=0> sigma_c_sq;
}

parameters {

  vector[N] theta;		// true treatment effects on clinical endpoint
  vector[N] gamma;		// true treatment effects on surrogate endpoint

  real alpha;
  real beta;
  real<lower=0> tau;
}

model{
	alpha ~ normal(0, sqrt(1e8));
	beta ~ normal(0, sqrt(1e8));
	
	gamma ~ normal(0, sqrt(1e8));
	
	target += log(sigma_c_sq)
				- 2 * log(sigma_c_sq + square(tau));
	
	theta ~ normal(alpha + beta * gamma, tau);
	
	for(i in 1:N){

    vector[2] y_obs;
    vector[2] mu;

    y_obs[1] = theta_hat[i];
    y_obs[2] = gamma_hat[i];

    mu[1] = theta[i];
    mu[2] = gamma[i];

    y_obs ~ multi_normal(mu, Sigma[i]);

  }
}
