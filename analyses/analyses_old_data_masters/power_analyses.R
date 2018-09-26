plot_permutation_test <- function(ptest) {
  ptest_d <- tibble::as_tibble(ptest["permuted"])
  result <- ggplot2::ggplot(ptest_d, ggplot2::aes(x=permuted)) +
    ggplot2::geom_histogram(fill="#FFD86D", colour="black")
  result <- result + ggplot2::geom_vline(xintercept=ptest$observed, lwd=2,
                                         lty="dashed")
  return(result)
}


difference_in_means <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- mean(d_1[[var]]) - mean(d_2[[var]])
  return(result)
}


difference_in_weighted_means <- function(d, w, dep_var, group_var, group1, group2) {
  group_factor <- factor(d[[group_var]])
  y <- d[[dep_var]]
  w <- w/(tapply(w, group_factor, sum)[group_factor])
  mns <- tapply(y*w, group_factor, sum)
  return(mns[[group1]] - mns[[group2]])
}

difference_in_weighted_medians <- function(d, w, dep_var, group_var,
                                           group1, group2) {
  group_factor <- factor(d[[group_var]])
  y <- d[[dep_var]]
  median1 <- matrixStats::weightedMedian(y, w, idxs=which(group_factor==group1), interpolate=)
  median2 <- matrixStats::weightedMedian(y, w, idxs=which(group_factor==group2), interpolate=T)
  return(median1-median2)
}

weighted_sd <- function(y, w) {
  n <- length(y)
  w <- w/sum(w)
  center <- sum(w * y)
  y <- y - center
  y <- y^2
  lambda <- n/(n - 1L)
  sigma2 <- lambda * sum(w * y)
  return(sqrt(sigma2))
}

weighted_sds <- function(d, w, dep_var, group_var, group1, group2) {
  group_factor <- factor(d[[group_var]])
  y <- d[[dep_var]]
  sd1 <- weighted_sd(y[group_factor==group1],
                     w[group_factor==group1])
  sd2 <- weighted_sd(y[group_factor==group2],
                     w[group_factor==group2])
  return(c(sd1, sd2))
}


tilted_projection <- function(d, var, group_var, group1, group2,
                              statistic=difference_in_weighted_means,
                              target_stat=NULL, target_alpha=NULL,
                              bs_samples_initial=250, importance_samples=375,
                              index=1) {
  if (is.null(target_stat) & is.null(target_alpha)) {
    target_stat <- statistic(d, rep(1, nrow(d)), var, group_var, group1, group2)
  }
  group_factor <- factor(d[[group_var]])
  dist_estimate <- boot::tilt.boot(d, statistic,
                                   c(bs_samples_initial, importance_samples), 
                                   stype="w", strata=group_factor,
                                   tilt=T, theta=target_stat,
                                   dep_var=var, group_var=group_var,
                                   group1=group1, group2=group2,
                                   index=index)
  dist1 <- dist_estimate$weights[index+1,group_factor == group1]
  dist2 <- dist_estimate$weights[index+1,group_factor == group2]
  return(list(dist1, dist2))
}

sample_tilted_projection <- function(d, proj, n1, n2,
                                     var, group_var, group1, group2) {
  l <- list()
  l[[group_var]] <- c(rep(group1, n1), rep(group2, n2))
  l[[var]] <- c(sample(d[d[[group_var]]==group1,][[var]], n1,
                       replace=T, prob=proj[[1]]),
                sample(d[d[[group_var]]==group2,][[var]], n2,
                       replace=T, prob=proj[[2]]))
  result <- tibble::as_tibble(l)
  return(result)
}

permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    d_perm <- random(d, grouping_var)
    permutation_statistics[i] <- statistic(d_perm, var,  'Random', group1, group2)
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}


###############################

power_test <- c(1:5)


hypothetical_true_dist <- tilted_projection(controls.true, 'lda_measure', 'Condition', 'Pos', 'Neg')
N_EXPERIMENTS <- 100
ptests <- list()
pvals <- rep(0, N_EXPERIMENTS)
samples <- controls.true[c(),]
test_statistics <- rep(0, N_EXPERIMENTS)
controls_pos <- dplyr::filter(controls.true, Condition=="Pos")
controls_neg <- dplyr::filter(controls.true, Condition=="Neg")

n1 <- nrow(controls_pos)
n2 <- nrow(controls_neg)



for (i in 1:N_EXPERIMENTS) {
  fake_data <- sample_tilted_projection(controls.true,
                                        hypothetical_true_dist,
                                        n1, n2,
                                        "lda_measure", "Condition",
                                        "Pos", "Neg")
  ptests[[i]] <- permutation_twogroups(fake_data,
                                       "lda_measure", "Condition",
                                       "Pos", "Neg", 
                                       difference_in_means,
                                       n_samples=99)
  pvals[i] <- permutation_pvalue_left(ptests[[i]])
  samples <- dplyr::bind_rows(samples,
                              dplyr::mutate(fake_data, Experiment=paste0("E", i)))
  test_statistics[i] <- difference_in_means(fake_data, "lda_measure",
                                              "Condition", "Pos",
                                              "Neg")
}


#Plot distribution for each experiment, each condition
ggplot2::ggplot(dplyr::filter(samples, Experiment %in% paste0("E", 1:10)),
                ggplot2::aes(y=lda_measure, x=Condition, fill=Condition)) +
  ggplot2::geom_violin(draw_quantiles=0.5) +
  ggplot2::facet_wrap(~ Experiment) +
  #ggplot2::ylim(c(4,10)) + 
  ggplot2::theme(legend.position="bottom")

# each permutated distribution vs observed mean difference
ptest_plots <- list()
for (i in 1:9) {
  ptest_plots[[i]] <- plot_permutation_test(ptests[[i]]) 
  #+ ggplot2::xlim(c(-0.2,0.3))
}
do.call(gridExtra::grid.arrange, ptest_plots)

## pvalues
p <- ggplot2::ggplot(tibble::tibble(pvals=pvals),
                     ggplot2::aes(x=pvals)) +
  ggplot2::geom_histogram(fill="#BBDDF0", colour="black")
p <- p + ggplot2::geom_vline(xintercept=0.05)
print(p)


##power
power_test[1] <- 1 - sum(pvals > 0.05)/N_EXPERIMENTS



## smaller samples - real power
N_EXPERIMENTS <- 100
ptests_small_n <- list()
pvals_small_n <- rep(0, N_EXPERIMENTS)
samples_small_n <- controls.true[c(),]
test_statistics_small_n <- rep(0, N_EXPERIMENTS)
# Smaller Ns: equivalent to N=10 approx
n1 <- 1100
n2 <- 1100
for (i in 1:N_EXPERIMENTS) {
  fake_data <- sample_tilted_projection(controls.true,
                                        hypothetical_true_dist,
                                        n1, n2,
                                        "lda_measure", "Condition",
                                        "Pos", "Neg")
  ptests_small_n[[i]] <- permutation_twogroups(fake_data,
                                               "lda_measure", "Condition",
                                               "Pos", "Neg",
                                               difference_in_means,
                                               n_samples=99)
  pvals_small_n[i] <- permutation_pvalue_left(ptests_small_n[[i]])
  samples_small_n <- dplyr::bind_rows(
    samples_small_n,
    dplyr::mutate(fake_data, Experiment=paste0("E", i)))
  test_statistics_small_n[i] <- difference_in_means(fake_data,   "lda_measure", "Condition",
                                                    "Pos", "Neg")
}

##power
power_test[2] <- 1 - sum(pvals_small_n > 0.05)/N_EXPERIMENTS





## smaller samples - real power
N_EXPERIMENTS <- 100
ptests_small_n <- list()
pvals_small_n <- rep(0, N_EXPERIMENTS)
samples_small_n <- controls.true[c(),]
test_statistics_small_n <- rep(0, N_EXPERIMENTS)
# Smaller Ns: equivalent to N=10 approx
n1 <- 900
n2 <- 900
for (i in 1:N_EXPERIMENTS) {
  fake_data <- sample_tilted_projection(controls.true,
                                        hypothetical_true_dist,
                                        n1, n2,
                                        "lda_measure", "Condition",
                                        "Pos", "Neg")
  ptests_small_n[[i]] <- permutation_twogroups(fake_data,
                                               "lda_measure", "Condition",
                                               "Pos", "Neg",
                                               difference_in_means,
                                               n_samples=99)
  pvals_small_n[i] <- permutation_pvalue_left(ptests_small_n[[i]])
  samples_small_n <- dplyr::bind_rows(
    samples_small_n,
    dplyr::mutate(fake_data, Experiment=paste0("E", i)))
  test_statistics_small_n[i] <- difference_in_means(fake_data,   "lda_measure", "Condition",
                                                    "Pos", "Neg")
}

##power
power_test[3] <- 1 - sum(pvals_small_n > 0.05)/N_EXPERIMENTS


## smaller samples - real power
N_EXPERIMENTS <- 100
ptests_small_n <- list()
pvals_small_n <- rep(0, N_EXPERIMENTS)
samples_small_n <- controls.true[c(),]
test_statistics_small_n <- rep(0, N_EXPERIMENTS)
# Smaller Ns: equivalent to N=10 approx
n1 <- 600
n2 <- 600
for (i in 1:N_EXPERIMENTS) {
  fake_data <- sample_tilted_projection(controls.true,
                                        hypothetical_true_dist,
                                        n1, n2,
                                        "lda_measure", "Condition",
                                        "Pos", "Neg")
  ptests_small_n[[i]] <- permutation_twogroups(fake_data,
                                               "lda_measure", "Condition",
                                               "Pos", "Neg",
                                               difference_in_means,
                                               n_samples=99)
  pvals_small_n[i] <- permutation_pvalue_left(ptests_small_n[[i]])
  samples_small_n <- dplyr::bind_rows(
    samples_small_n,
    dplyr::mutate(fake_data, Experiment=paste0("E", i)))
  test_statistics_small_n[i] <- difference_in_means(fake_data,   "lda_measure", "Condition",
                                                    "Pos", "Neg")
}

##power
power_test[4] <- 1 - sum(pvals_small_n > 0.05)/N_EXPERIMENTS


## smaller samples - real power
N_EXPERIMENTS <- 100
ptests_small_n <- list()
pvals_small_n <- rep(0, N_EXPERIMENTS)
samples_small_n <- controls.true[c(),]
test_statistics_small_n <- rep(0, N_EXPERIMENTS)
# Smaller Ns: equivalent to N=10 approx
n1 <- 300
n2 <- 300
for (i in 1:N_EXPERIMENTS) {
  fake_data <- sample_tilted_projection(controls.true,
                                        hypothetical_true_dist,
                                        n1, n2,
                                        "lda_measure", "Condition",
                                        "Pos", "Neg")
  ptests_small_n[[i]] <- permutation_twogroups(fake_data,
                                               "lda_measure", "Condition",
                                               "Pos", "Neg",
                                               difference_in_means,
                                               n_samples=99)
  pvals_small_n[i] <- permutation_pvalue_left(ptests_small_n[[i]])
  samples_small_n <- dplyr::bind_rows(
    samples_small_n,
    dplyr::mutate(fake_data, Experiment=paste0("E", i)))
  test_statistics_small_n[i] <- difference_in_means(fake_data,   "lda_measure", "Condition",
                                                      "Pos", "Neg")
}


#Plot distribution for each experiment, each condition
ggplot2::ggplot(dplyr::filter(samples_small_n, Experiment %in% paste0("E", 1:10)),
                ggplot2::aes(y=lda_measure, x=Condition, fill=Condition)) +
  ggplot2::geom_violin(draw_quantiles=0.5) +
  ggplot2::facet_wrap(~ Experiment) +
  #ggplot2::ylim(c(4,10)) + 
  ggplot2::theme(legend.position="bottom")

# each permutated distribution vs observed mean difference
ptest_plots <- list()
for (i in 1:9) {
  ptest_plots[[i]] <- plot_permutation_test(ptests_small_n[[i]]) + ggplot2::xlim(c(-0.9,0.9))
}
do.call(gridExtra::grid.arrange, ptest_plots)

##power
power_test[5] <- 1 - sum(pvals_small_n > 0.05)/N_EXPERIMENTS
