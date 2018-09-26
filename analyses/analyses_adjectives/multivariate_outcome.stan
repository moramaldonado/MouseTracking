// data block: declaration of the variables in the data object stanDat: specifications about the data type (real, integers)
data {
      int<lower=0> N; //number of observations
      int<lower=1> K; //number of variables
      int<lower=1> J; //number of variables
      vector[J] x[N]; // predictor (polarity)
      vector[K] y[N]; // outcome (lda measure and maximal deviation)
    }
// parameters block: three parameters: the fixed slope, the fixed intercept and the SD of the error. (priors)
parameters {
      matrix[K, J] beta; // slope and intercept
      cholesky_factor_corr[K] L_Omega; //  correlation matrix btw the two dependent measures
      vector<lower=0>[K] L_sigma; // variance and covariance matrix
} 
// if priors are underspecified in the model, Stan assumes uniform priors on parameters ovr their leval values as determined by their declared constraints.
model {
      vector[K] mu[N];
      matrix[K, K] L_Sigma;
      
      for (n in 1:N)
        mu[n] = beta * x[n];
      
      L_Sigma = diag_pre_multiply(L_sigma, L_Omega);
      to_vector(beta) ~ normal(0, 5);
      L_Omega ~ lkj_corr_cholesky(4);
      L_sigma ~ cauchy(0, 2.5);
      y ~ multi_normal_cholesky(mu, L_Sigma);
    }