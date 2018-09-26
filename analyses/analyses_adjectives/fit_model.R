## CLEAN ENVIRONMENT
rm(list = ls())

## CHARGE PACKAGES AND FUNCTIONS
source("paper/R/packages-functions.R")
require('rstan')

## CHARGE NEGATION DATA FROM D&D REPLICATION
load('negation_data_processed.RData')


## DATA
# subset data to just true affirmative trials (for super simple model)
mydata.affirmative.true <- subset(negation_data, Polarity=='P' & Response=='true')
mydata.affirmative.true <- mydata.affirmative.true  %>% dplyr::select('lda_measure','Subject','Adjective')
mydata.affirmative.true$Adjective <- factor(mydata.affirmative.true$Adjective)


#subset data to true trials 
mydata.true <- subset(negation_data, Response=='true')
mydata.true <- mydata.true  %>% dplyr::select('lda_measure','Subject','Adjective', 'MaxDeviation', 'Polarity')
mydata.true$Adjective <- factor(mydata.true$Adjective)

#contrast coding -1/1 for predictor Polarity
mydata.true$Polarity <- factor(mydata.true$Polarity)
mydata.true$predictor <- ifelse(mydata.true$Polarity=='P', 0, 1)


# log-transform the maximal deviation
mydata.true$LogMaxDeviation <- log(mydata.true$MaxDeviation)


par(mfrow=c(2,2))
hist(subset(mydata.true, predictor==0)$LogMaxDeviation)
hist(subset(mydata.true, predictor==1)$LogMaxDeviation)
hist(subset(mydata.true, predictor==0)$lda_measure)
hist(subset(mydata.true, predictor==1)$lda_measure)

# scale both measures with z-score
mydata.true$LogMaxDeviation.scale <-  scale(mydata.true$LogMaxDeviation, center=TRUE, scale=TRUE)
mydata.true$lda_measure.scale <-  scale(mydata.true$lda_measure, center=TRUE, scale=TRUE)

par(mfrow=c(2,2))
hist(subset(mydata.true, predictor==0)$LogMaxDeviation.scale) 
abline(v=mean(subset(mydata.true, predictor==0)$LogMaxDeviation.scale),col="red")
hist(subset(mydata.true, predictor==1)$LogMaxDeviation.scale)
abline(v=mean(subset(mydata.true, predictor==1)$LogMaxDeviation.scale),col="red")
hist(subset(mydata.true, predictor==0)$lda_measure.scale)
abline(v=mean(subset(mydata.true, predictor==0)$lda_measure.scale),col="red")
hist(subset(mydata.true, predictor==1)$lda_measure.scale)
abline(v=mean(subset(mydata.true, predictor==1)$lda_measure.scale),col="red")


## 1) SIMPLE MODEL TO TRY (WITH RANDOM STRUCTURE, NO PREDICTORS) ####

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





## 2) SIMPLE MODEL TO TRY (WITHOUT RANDOM STRUCTURE, PREDICTOR FOR POLARITY) ####

# data conversion: Stan requires the data to be a list (unlike lmer)
stanDat <- list(y= mydata.true$lda_measure,
                x= mydata.true$predictor,
                N=nrow(mydata.true))

# fit model in the file fixeff.stan
# This function generates four chains of samples (Markov chains) which are independent from each other
# Each of the four chains contains an n number of iterations (samples), which in this case is set to 2000, the first 1000 samples are warm up (MCMC to converge in a posterior distribution, once it's done it remains pretty stable), the second 1000 samples are used to analyse. NB: The number of iterations needed to converge depends on the number of parameters
# The result of the model is saved in an object of class .stan
fixEffit <- stan(file="simple.stan", data = stanDat, iter=2000, chains =4)

# examine quantiles of posterior distributions: joint posterior probability distribution of each of the parameters (beta0, beta1 and sd-error)
# by doing this printing, we obtain a table that contains the Rhat statistics, which are a second diagnostic of the convergence of the model [R = right most column in the table, values around 1 if the model converged]
## See credible intervals: the 1 - alpha % (95% in this case) contains 95% of the posterior probability density
print(fixEffit, pars = c("beta", "alpha", "sigma"), probs = c(0.025, 0.5, 0.975))





## 3) MULTIVARIATE MODEL ON DALE AND DURAN REPLICATION ####
# NB: I am doing this with lda measure and maximal deviation
# the data is the same as before (mydata.true) 

#prepare data for stan
stanDat.complete <- list(y= matrix(c(mydata.true$lda_measure.scale,mydata.true$LogMaxDeviation.scale),ncol=2),
                x=matrix(c(rep(1, nrow(mydata.true)), mydata.true$predictor), ncol = 2),
                N=nrow(mydata.true), K=2, J=2)



fixEffit.complete <- stan(file="multivariate_outcome.stan", data = stanDat.complete, iter=2000, chains =4)


#results
print(fixEffit.complete, pars = c("beta", 'L_sigma', 'L_Omega'), probs = c(0.025, 0.5, 0.975))

# beta[1,1] is the first intercept
# beta[1,2] is the first slope
# beta[2,1] is the second intercept
# beta[2,2] is the second slope
# Omega are the elements of the correlation matrix
# Sigma are the elements of the covariance matrix

## bayesian diagnostic with shiny
library(shinystan)
launch_shinystan(fixEffit.complete)

# save model just in case
save(fixEffit.complete, file='model_replicationDD.RData')






## 4) SAME MODEL APPLIED TO VALIDATION DATA ####
load('Validation.RData')


## DATA
mydata.validation  <- calibration_data  %>% dplyr::select('lda_measure_full','Subject', 'MaxDeviation', 'Polarity')

#contrast coding -1/1 for predictor Polarity
mydata.validation$Polarity <- factor(mydata.validation$Polarity)
mydata.validation$predictor <- ifelse(mydata.validation$Polarity=='straight', 1, -1)


# log-transform the maximal deviation
mydata.validation$LogMaxDeviation <- log(mydata.validation$MaxDeviation)

par(mfrow=c(2,2))
hist(subset(mydata.validation, predictor==1)$LogMaxDeviation)
hist(subset(mydata.validation, predictor==-1)$LogMaxDeviation)
hist(subset(mydata.validation, predictor==1)$lda_measure_full)
hist(subset(mydata.validation, predictor==-1)$lda_measure_full)

# scale both measures with z-score
mydata.validation$LogMaxDeviation.scale <-  scale(mydata.validation$LogMaxDeviation, center=TRUE, scale=TRUE)
mydata.validation$lda_measure.scale <-  scale(mydata.validation$lda_measure_full, center=TRUE, scale=TRUE)

par(mfrow=c(2,2))
hist(subset(mydata.validation, predictor==1)$LogMaxDeviation.scale)
hist(subset(mydata.validation, predictor==-1)$LogMaxDeviation.scale)
hist(subset(mydata.validation, predictor==1)$lda_measure.scale)
hist(subset(mydata.validation, predictor==-1)$lda_measure.scale)


# prepare data for stan
stanDat.complete.validation <- list(y= matrix(c(mydata.validation$lda_measure.scale,mydata.validation$LogMaxDeviation.scale),ncol=2),
                         x=matrix(c(rep(1, nrow(mydata.validation)), mydata.validation$predictor), ncol = 2),
                         N=nrow(mydata.validation), K=2, J=2)


fixEffit.complete.validation <- stan(file="multivariate_outcome.stan", data = stanDat.complete.validation, iter=2000, chains =4)


#results
print(fixEffit.complete.validation, pars = c("beta", 'L_sigma', 'L_Omega'), probs = c(0.025, 0.5, 0.975))

