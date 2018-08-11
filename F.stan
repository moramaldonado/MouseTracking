data {
int<lower=1> N;
real lda[N];
int<lower=1> J;
int<lower=1> K;
int<lower=1, upper=J> subj[N];
int<lower=1, upper=K> item[N];
}
parameters {
  vector[1] beta;
  vector[J] u;
  vector[K] w;
  real<lower=0> sigma_e;
  real<lower=0> sigma_u;
  real<lower=0> sigma_w;
  }
model {
real mu;
u ~ normal(0, sigma_u);
w ~ normal(0, sigma_w);
for (i in 1:N){
    mu = beta[1] + u[subj[i]] + w[item[i]];
    lda[i] ~ normal(mu, sigma_e);}
}
