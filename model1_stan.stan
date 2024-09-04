data {
  int<lower=0> N;
  vector[N] yield_precents;
  vector[N] GDP;
  vector[N] inflation_rate;  
  vector[N] unemployment_rate;
  vector[N] interest;

}

parameters {
  real b_0;                 
  real b_gdp;                
  real b_inflation_rate;
  real b_unemployment_rate;
  real b_interest;
  real<lower=0> sigma;      
}


model {
  // Priors
  b_0 ~ normal(0, 10);
  b_gdp ~ normal(0, 5);
  b_inflation_rate ~ normal(0,5);
  b_unemployment_rate ~ normal(0,5);
  b_interest ~ normal(0,5);
  sigma ~ exponential(1);

  // Likelihood
  yield_precents ~ normal(b_0 + b_gdp * GDP + b_inflation_rate * inflation_rate + b_unemployment_rate * unemployment_rate + b_interest * interest, sigma);
}

generated quantities {
  vector[N] log_lik;  // log-likelihood for each observation
  vector[N] y_rep;  // posterior predictive distribution
  vector[N] mu;
  for (n in 1:N){
    mu[n] = (b_0 + b_gdp * GDP[n] + b_inflation_rate * inflation_rate[n] + b_unemployment_rate * unemployment_rate[n] + b_interest * interest[n]);
    log_lik[n] = normal_lpdf(yield_precents[n] | mu[n], sigma);
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}
