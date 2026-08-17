data {
  int<lower=0> V;
  vector<lower=0, upper=1>[V] Sex;
  vector<lower=0>[V] Income;
  vector<lower=0, upper=1>[V] Discount;
  
  array[V] int<lower=0, upper=1> Y;
}

parameters {
  vector[4] b;
}

transformed parameters{
  vector<lower=0, upper=1>[V] p = inv_logit(b[1] + b[2] * Sex + b[3] * Income + b[4] * Discount);
}

model {
  Y[1:V] ~ bernoulli(p[1:V]);
}

generated quantities{
  array[V] int<lower=0, upper=1> yp = bernoulli_rng(p[1:V]);
}
