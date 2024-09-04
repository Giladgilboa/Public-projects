data {
  int<lower=0> N;
  vector[N] daily_yield;
  vector[N] Positive_sentiment;
  vector[N] Negative_sentiment;
  vector[N] analyst_Comments;
}

parameters {
  real b_0;
  real b_positive;
  real b_negative;
  real b_analyst;
  real<lower=0> sigma;
}

model {
  // Priors
  b_0 ~ normal(0, 10);
  b_positive ~ normal(0, 5);
  b_negative ~ normal(0, 5);
  b_analyst ~ normal(0, 5);
  sigma ~ exponential(1);
  
  daily_yield ~ normal(b_0 + b_positive * Positive_sentiment +
                   b_negative * Negative_sentiment +
                   b_analyst * analyst_Comments , sigma);
}

generated quantities {
  vector[N] log_lik;
  real y_rep[N];
  vector[N] mu;
  for (n in 1:N){
    mu[n] = b_0 + b_positive * Positive_sentiment[n] +
            b_negative * Negative_sentiment[n] +
            b_analyst * analyst_Comments[n] ;
    log_lik[n] = normal_lpdf(daily_yield[n] | mu[n], sigma);
    y_rep[n] = normal_rng(mu[n], sigma);
  }
}