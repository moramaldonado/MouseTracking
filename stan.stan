//model y_{n} = alpha + beta * X_{n} + epsilon
// y ~ normal(alpha + beta * x, sigma)



// data block: declaration of the variables in the data object stanDat: specifications about the data type (real, integers)
data {
      int<lower=0> N; //number of observations
      vector[N] x; // predictor (polarity)
      vector[N] y; // outcome (lda measure)
    }
    
// parameters block: three parameters: the fixed slope, the fixed intercept and the SD of the error. (priors)
parameters {
      real alpha; // intercept
      real beta; // slope
      real<lower=0> sigma; // noise term or error scale: improper uniform prior with lower bound of 0 is assumed by Stan
} 

// if priors are underspecified in the model, Stan assumes uniform priors on parameters ovr their leval values as determined by their declared constraints.

model {
      y ~ normal(alpha + beta * x, sigma); // likelihood
    }