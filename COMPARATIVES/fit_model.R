## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES AND FUNCTIONS
source("paper/R/packages-functions.R")
require('rstan')

## CHARGE NEGATION DATA FROM D&D REPLICATION
load('negation_data_processed.RData')

# pre-step: subset data to just true affirmative trials
mydata.affirmative.true <- subset(negation_data, Polarity=='P' & Response=='true')
mydata.affirmative.true <- mydata.affirmative.true  %>% dplyr::select('lda_measure','Subject','Adjective')
mydata.affirmative.true$Adjective <- factor(mydata.affirmative.true$Adjective)

# data conversion: Stan requires the data to be a list (unlike lmer)
stanDat <- list(subj = as.integer(mydata.affirmative.true$Subject),
                item = as.integer(mydata.affirmative.true$Adjective),
                J = nlevels(mydata.affirmative.true$Subject),
                K = nlevels(mydata.affirmative.true$Adjective),
                lda= mydata.affirmative.true$lda_measure,
                N=nrow(mydata.affirmative.true))

# fit model in the file fixeff.stan
# This function generates four chains of samples (Markov chains) which are independent from each other
# Each of the four chains contains an n number of iterations (samples), which in this case is set to 2000, the first 1000 samples are warm up (MCMC to converge in a posterior distribution, once it's done it remains pretty stable), the second 1000 samples are used to analyse. NB: The number of iterations needed to converge depends on the number of parameters
# The result of the model is saved in an object of class .stan
fixEffit <- stan(file="fixeff.stan", data = stanDat, iter=2000, chains =4)


# plot traceplot, excluding warm-up:
# Looking at each chain after warm up -- This is the first diagnostic to check whether the chains have converged to the posterior distribution.
traceplot(fixEffit, pars = c("beta", "sigma_e"), inc_warmup = FALSE)

# examine quantiles of posterior distributions: joint posterior probability distribution of each of the parameters (beta0, beta1 and sd-error)
# by doing this printing, we obtain a table that contains the Rhat statistics, which are a second diagnostic of the convergence of the model [R = right most column in the table, values around 1 if the model converged]
## See credible intervals: the 1 - alpha % (95% in this case) contains 95% of the posterior probability density
print(fixEffit, pars = c("beta", "sigma_e", "sigma_u","sigma_w"), probs = c(0.025, 0.5, 0.975))

# examine quantiles of parameter of interest:
# beta1 <- unlist(extract(fixEfFit, pars = "beta[2]"))
print(quantile(beta1, probs = c(0.025, 0.5, 0.975)))

