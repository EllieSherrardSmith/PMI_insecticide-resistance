// logistic function
data {
  int<lower=1> N;    // rows of data
  vector[N] x;       // predictor
  real<lower=0> y[N]; // response
}
parameters {
  real<lower=0> phi; // 
  real<lower=0> beta;  // 
  real<lower=0> alpha;// 
}
model {
  // priors:
    phi ~ cauchy(0, 1);
  beta ~ normal(0, 1);
  alpha ~ normal(0, 1);

// data model:
for (n in 1:N)
    y[n] ~ normal((1 / (1 + alpha * exp(beta * x[n]))), phi);
}

#generated quantities {
#  vector[N] log_lik;
#  for (n in 1:N){
#    log_lik[n] <- normal_log(y[n], 1 - (1 / (1 + alpha * exp(beta * x[n]))), phi);
#  }
#}